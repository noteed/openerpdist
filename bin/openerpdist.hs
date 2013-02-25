{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

import Paths_openerpdist (version, getDataDir)

import OpenERP.Dist

main :: IO ()
main = (runCmd =<<) . cmdArgs $
  modes
    [ cmdPatch
    , cmdSdist
    , cmdCheckSdist
    , cmdTestSdist
    ]
  &= summary versionString
  &= program "openerpdist"

-- | String with the program name, version and copyright.
versionString :: String
versionString = "openerpdist " ++ showVersion version
  ++ " - Copyright (c) 2013 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    Patch
    -- ^ Patch upstream sources
  | Sdist
    -- ^ Call patch and then build a tarball.
  | CheckSdist
    -- ^ Check the previously built tarball can regenerate itself.
  | TestSdist
    -- ^ Extract a previously built tarball and run the tests.
  deriving (Data, Typeable)

-- | Create a 'Patch' command.
cmdPatch :: Cmd
cmdPatch = Patch
    &= help "Patch upstream sources."
    &= explicit
    &= name "patch"

-- | Create a 'Sdist' command.
cmdSdist :: Cmd
cmdSdist = Sdist
    &= help "Build a tarball."
    &= explicit
    &= name "sdist"

-- | Create a 'CheckSdist' command.
cmdCheckSdist :: Cmd
cmdCheckSdist = CheckSdist
    &= help "Check the previously built tarball can regenerate itself."
    &= explicit
    &= name "check-sdist"

-- | Create a 'TestSdist' command.
cmdTestSdist :: Cmd
cmdTestSdist = TestSdist
    &= help "Extract a previously built tarball and run the tests."
    &= explicit
    &= name "test-sdist"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd Patch{..} = do
  dataDir <- getDataDir
  exitIfDiffPresent
  patch dataDir

runCmd Sdist{..} = sdist

runCmd CheckSdist{..} = checkSdist

runCmd TestSdist{..} = testSdist
