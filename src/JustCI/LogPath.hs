{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The per-run log path convention:
-- @.justci\/\<short-sha\>\/\<platform\>\/\<recipe\>.log@.
-- One module owns the whole shape — SHA-keyed directory (7-char
-- abbreviated, so the path fits GitHub's 140-char commit-status
-- @description@ budget), a per-platform subdirectory (so the
-- linux\/macos lanes never collide on the same recipe name), and a
-- per-recipe filename. A change to the convention (longer SHA prefix
-- to avoid collisions, a flat layout, S3 URLs) edits this file and
-- nothing else; the YAML emitter and the commit-status poster both
-- consume from here and stay byte-identical by construction.
module JustCI.LogPath
  ( -- * Shared constants
    shortShaLen,

    -- * Path composition
    logDirFor,
    platformDir,
    logPathFor,
  )
where

import qualified Data.Text as T
import Data.Text.Display (display)
import JustCI.Git (Sha)
import JustCI.Node (NodeId, nodeName, nodePlatform)
import JustCI.Platform (Platform)
import System.FilePath ((</>))

-- | The number of SHA hex chars used in log-directory and remote-cache-directory
-- names. 7 chars gives ~1-in-268-million collision probability per repo, which
-- is far beyond any real CI workload; and it fits comfortably inside GitHub's
-- 140-char commit-status @description@ budget.
--
-- Both 'logDirFor' (local @.justci\/\<sha\>\/@ path) and 'JustCI.Transport.cachedRunDir'
-- (remote @\${XDG_STATE_HOME:-\$HOME/.local/state}\/justci\/\<sha\>\/@ path) must use the same prefix length so
-- a contributor can correlate the two directories. Import this constant rather
-- than hardcoding @7@.
shortShaLen :: Int
shortShaLen = 7

-- | Compose the per-run log directory: @.justci\/\<short-sha\>\/@. Returns
-- a repo-relative path with a 'shortShaLen'-char abbreviated SHA so the
-- @description@ field on a GitHub commit status stays readable inside
-- its 140-char budget (a 40-char hex blob blew most of it on one
-- field). Repo-relative so the path is portable across machines: a
-- contributor seeing the status can paste it straight into their own
-- checkout instead of looking at the runner's filesystem layout.
logDirFor :: Sha -> FilePath
logDirFor sha
  | T.length shaText < shortShaLen =
      error $
        "JustCI.LogPath.logDirFor: sha shorter than "
          <> show shortShaLen
          <> " chars: "
          <> T.unpack shaText
  | otherwise = ".justci" </> T.unpack (T.take shortShaLen shaText)
  where
    shaText = display sha

-- | Per-platform subdirectory under the run's log directory.
-- @.justci\/\<short-sha\>\/\<platform\>\/@. Exposed so 'JustCI.Pipeline' can
-- create the directories ahead of process-compose's spawn — the
-- per-recipe log files inside don't get auto-created by pc.
platformDir :: FilePath -> Platform -> FilePath
platformDir logDir p = logDir </> T.unpack (display p)

-- | Compose @\<logDir\>\/\<platform\>\/\<recipe\>.log@. Decomposes
-- 'NodeId' (rather than relying on its 'Display') because the on-disk
-- layout (nested subdir) is genuinely a different shape from the wire
-- identifier (@\<recipe\>\@\<platform\>@). Same module owns both
-- decompositions so they stay aligned without typeclass coincidence.
logPathFor :: FilePath -> NodeId -> FilePath
logPathFor logDir n =
  platformDir logDir (nodePlatform n) </> T.unpack (nodeName n) <> ".log"
