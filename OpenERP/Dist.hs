{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module OpenERP.Dist where

import Control.Exception (bracket)
import System.Directory
  ( canonicalizePath, copyFile, createDirectory, doesFileExist
  , getCurrentDirectory, setCurrentDirectory
  )
import System.Exit
import System.FilePath ((</>))
import System.Process (rawSystem)

import OpenERP.Dist.Descriptor (generateSetup)

-- TODO keep in sync with setup.py.
fullname :: String
fullname = "openerp-core-7.0.1"

tarball :: String
tarball = fullname ++ ".tar.gz"

-- | Check if the working copy is clean before messing with it, call
-- `exitFailure` otherwise.
exitIfDiffPresent :: IO ()
exitIfDiffPresent = do
  -- TODO Properly check if `bzr` is available.
  exitCode <- rawSystem "bzr" ["diff"]
  case exitCode of
    ExitSuccess -> return () -- No diff, working copy clean.
    _ -> do -- There is a diff (or it is not even a working copy), so abort.
      putStrLn "Not a working copy or it contains changes, aborting."
      exitFailure

patch :: FilePath -> IO ()
patch dataDir = do
  exist <- doesFileExist "__openerp__.py"
  if exist
    then do
      -- Assume an addons.
      dir <- getCurrentDirectory
      cdir <- canonicalizePath dir
      generateSetup cdir
    else do
      -- Assume openerp-core.
      -- Replace some files with ours.
      copyFile (dataDir </> "openerpcore" </> "setup.py") "setup.py"
      copyFile (dataDir </> "openerpcore" </> "MANIFEST.in") "MANIFEST.in"

sdist :: IO ()
sdist = do
  -- TODO Check exit code.
  _ <- rawSystem "python2" ["setup.py", "sdist"]
  return ()

checkSdist :: IO ()
checkSdist = do
  let checkDirA = "check_sdist_a" -- original
      checkDirB = "check_sdist_b" -- the one we build as a check
      checkDir = "check_sdist" -- the original but used to do the build
  -- Don't use createDirectoryIfMissing so that we crash.
  createDirectory checkDirA
  createDirectory checkDirB
  createDirectory checkDir
  -- TODO Check exit code.
  copyFile ("dist" </> tarball) (checkDir </> tarball)
  withDirectory checkDir $ do
    -- TODO Check exit code.
    _ <- rawSystem "tar" ["xf", tarball, "-C", ".." </> checkDirA]
    _ <- rawSystem "tar" ["xf", tarball]
    setCurrentDirectory fullname
    -- Execute ourselves
    _ <- rawSystem "openerpdist" ["sdist"]
    _ <- rawSystem "tar"
      ["xf", "dist" </> tarball, "-C", ".." </> ".." </> checkDirB]
    return ()
  exitCode <- rawSystem "diff" ["-u", "-r", checkDirA, checkDirB]
  case exitCode of
    ExitSuccess -> do
      putStrLn "Success: the tarball can regenerate itself."
      exitSuccess
    _ -> do
      putStrLn "Failure: the tarball cannot regenerate itself."
      exitFailure

testSdist :: IO ()
testSdist = do
  let testDir = "test_sdist"
  createDirectory testDir
  -- TODO Check exit code.
  _ <- rawSystem "tar" ["xf", "dist" </> tarball, "-C", testDir]
  withDirectory (testDir </> fullname) $ do
    _ <- rawSystem "python2" ["oe", "--help"]
    return ()

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir f = do
  previousDir <- getCurrentDirectory
  bracket
    (setCurrentDirectory dir)
    (const $ setCurrentDirectory previousDir)
    (const f)
