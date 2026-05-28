{-# LANGUAGE OverloadedRecordDot #-}

-- | Thin harness: argv → parse → dispatch → exit. All CLI machinery
-- (parser, 'Command' sum, option records) lives in "JustCI.CLI"; the
-- per-mode bodies live in "JustCI.Pipeline". Adding a feature should not
-- fatten this file — every line here is dispatch. The strict-vs-dev
-- decomposition that used to live as a @CI=true@ env-var gate at this
-- layer now lives behind 'runPipeline', which resolves the user's
-- @--no-strict@ \/ @--no-snapshot@ \/ @--no-post@ flags into a
-- 'JustCI.Pipeline.RunPolicy' itself.
module Main where

import JustCI.CLI (Args (..), Command (..), parseCli)
import JustCI.Pipeline (RunDir (..), ensureRunDir, resolveRunDir, runDumpYaml, runGraph, runPcPassthrough, runPipeline, runProtect)
import System.Exit (exitWith)

main :: IO ()
main = do
  (args, passthrough) <- parseCli
  case args.cmd of
    Run opts -> do
      dirs <- ensureRunDir
      runPipeline opts passthrough dirs
    DumpYaml -> runDumpYaml
    Graph -> runGraph
    Protect opts -> runProtect opts
    PcPassthrough verb pcArgs -> do
      -- resolveRunDir, not ensureRunDir: the passthrough is read-only,
      -- shouldn't materialise .ci/ for a checkout that has never run.
      dirs <- resolveRunDir
      exitWith =<< runPcPassthrough verb pcArgs dirs.sock
