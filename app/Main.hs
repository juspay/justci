{-# LANGUAGE OverloadedRecordDot #-}

-- | Thin harness: argv → parse → dispatch → exit. All CLI machinery
-- (parser, 'Command' sum, option records) lives in "JustCI.CLI"; the
-- per-mode bodies live in "JustCI.Pipeline". Adding a feature should not
-- fatten this file — every line here is either dispatch or the
-- @CI=true@ mode gate that picks between 'runLocal' and 'runStrict'.
module Main where

import JustCI.CLI (Args (..), Command (..), parseCli)
import JustCI.Pipeline (ensureRunDir, runDumpYaml, runGraph, runLocal, runPcPassthrough, runProtect, runStrict)
import System.Environment (lookupEnv)
import System.Exit (exitWith)

main :: IO ()
main = do
  (args, passthrough) <- parseCli
  case args.cmd of
    Run opts -> do
      dirs <- ensureRunDir
      strict <- (== Just "true") <$> lookupEnv "CI"
      let runner = if strict then runStrict else runLocal
      runner opts passthrough dirs
    DumpYaml -> runDumpYaml
    Graph -> runGraph
    Protect opts -> runProtect opts
    PcPassthrough verb pcArgs -> do
      dirs <- ensureRunDir
      runPcPassthrough verb pcArgs dirs >>= exitWith
