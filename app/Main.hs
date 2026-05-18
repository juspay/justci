{-# LANGUAGE OverloadedRecordDot #-}

-- | Thin harness: argv → parse → dispatch → exit. All CLI machinery
-- (parser, 'Command' sum, option records) lives in "CI.CLI"; the
-- per-mode bodies live in "CI.Pipeline". Adding a feature should not
-- fatten this file — every line here is either dispatch or the
-- @CI=true@ mode gate that picks between 'runLocal' and 'runStrict'.
module Main where

import CI.CLI (Args (..), Command (..), parseCli)
import CI.Pipeline (ensureRunDir, runDumpYaml, runGraph, runLocal, runProtect, runStrict)
import System.Environment (lookupEnv)

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
