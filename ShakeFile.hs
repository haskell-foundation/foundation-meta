-- |
-- Module      : ShakeFile
-- License     : BSD-Style
-- Maintainer  : Foundation (github.com/haskell-foundation)
-- Stability   : stable
-- Portability : unknown
--
-- This ShakeFile will be use to generate documentation for the different
-- projects, helping to the automatisation of the release, documentation
-- generation, publishing benchmark
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad (when, unless)
import Data.List (stripPrefix)

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import System.Exit (ExitCode(..))
import System.Console.GetOpt
import Data.Data
import Data.Typeable

foundationMetaOptions :: ShakeOptions
foundationMetaOptions = shakeOptions
  { shakeFiles = "shake_files"
  }

data Flags
    = Release
    | FoundationPath FilePath
  deriving (Show, Eq, Ord, Data, Typeable)

configFlags :: [OptDescr (Either a Flags)]
configFlags =
  [ Option [] ["release"]    (NoArg $ Right Release) "generate the release version (only need to commit and push)"
  , Option [] ["foundation"] (ReqArg (Right . FoundationPath) "DIR")    "foundation library directory"
  ]

getFoundationDir :: [Flags] -> FilePath
getFoundationDir [] = "_build/foundation"
getFoundationDir (FoundationPath d:_) = d
getFoundationDir (_:xs) = getFoundationDir xs

newtype FoundationVersion = FoundationVersion FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype FoundationAuthor = FoundationAuthor FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype FoundationRef = FoundationRef FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype FoundationDate = FoundationDate FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype CurrentDir = CurrentDir ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype FoundationFetch = FoundationFetch FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype FoundationBenchs = FoundationBenchs FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype FoundationInstallBenchs = FoundationInstallBenchs FilePath
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

main :: IO ()
main = shakeArgsWith foundationMetaOptions configFlags $ \flags targets -> return $ Just $ do
  if null targets then want ["all"] else want targets
  let release = Release `elem` flags
  let foundationDir = getFoundationDir flags
  let resultDir = if release then "result" </> "release" else "result/devel"

  fetchFoundation <- addOracle $ \(FoundationFetch fp) -> do
    foundationExist <- doesDirectoryExist fp
    unless foundationExist $ do
      putNormal "cloning foundation directory"
      unit $ cmd "git" "clone" "git@github.com:haskell-foundation/foundation" foundationDir
    when release $ do
      putNormal "updating foundation directory"
      unit $ cmd (Cwd foundationDir) "git" "fetch"
      unit $ cmd (Cwd foundationDir) "git" ["checkout", "origin/master", "-B", "release" </> "TODO"]
  getCurrentDir <- addOracle $ \(CurrentDir ()) ->
     head . lines . fromStdout <$> cmd "pwd"
  getCommitAuthor <- addOracle $ \(FoundationAuthor fp) -> do
    fetchFoundation $ FoundationFetch fp
    Stdout out <- cmd (Cwd fp) "git" ["log", "-n 1", "--format=\"%an <%aE>\""]
    return $ head $ lines out
  getCommitDate <- addOracle $ \(FoundationDate fp) -> do
    fetchFoundation $ FoundationFetch fp
    Stdout out <- cmd (Cwd fp) "git log -n 1 --format='%ai'"
    return $ head $ lines out
  getCommitRef <- addOracle $ \(FoundationRef fp) -> do
    fetchFoundation $ FoundationFetch fp
    Stdout out <- cmd (Cwd fp) "git log -n 1 --format='%H'"
    return $ head $ lines out
  getFoundationVersion <- addOracle $ \(FoundationVersion fp) -> do
    fetchFoundation $ FoundationFetch fp
    (Stdout out, Exit exitCode) <- cmd (Cwd fp) "git describe --tags"
    if exitCode == ExitSuccess
      then return $ head $ lines out
      else return "master"
  getFoundationBenchs <- addOracle $ \(FoundationBenchs fp) -> do
    fetchFoundation $ FoundationFetch fp
    getDirectoryFiles fp ["benchs/*.hs", "benchs/compare-libs//*.hs"]
  installFoundationBenchs <- addOracle $ \(FoundationInstallBenchs fp) -> do
    fetchFoundation $ FoundationFetch fp
    unit $ cmd (Cwd $ fp </> "benchs") "stack install"

  resultDir </> "foundation" </> "*" </> "info" %> \infoFile -> do
    version <- getFoundationVersion $ FoundationVersion foundationDir
    ref <- getCommitRef $ FoundationRef foundationDir
    date <- getCommitDate $ FoundationDate foundationDir
    author <- getCommitAuthor $ FoundationAuthor foundationDir
    putNormal $ "foundation version: " ++ show version
    putNormal $ "foundation commit:  " ++ show ref
    putNormal $ "foundation author:  " ++ show author
    putNormal $ "foundation date:    " ++ show date
    writeFileChanged infoFile $ unlines [date, version, ref, author]
  foundationDir </> "benchs//*" %> \benchFile -> do
    putNormal $ "build bench: " ++ show benchFile
    fetchFoundation $ FoundationFetch foundationDir
    let sourceFile = case stripPrefix (foundationDir </> "benchs" ++ "/") benchFile of
          Nothing -> error "unexpected..."
          Just fp -> fp <.> "hs"
    putNormal $ "source: " ++ show sourceFile
    putNormal $ "bench:  " ++ show (benchFile <.> "hs")
    installFoundationBenchs $ FoundationInstallBenchs foundationDir
    unit $ cmd (Cwd $ foundationDir </> "benchs")
      "stack"
      ["ghc", "--", "-O", sourceFile]
  resultDir </> "foundation" </> "*" </> "benchs//*" <.> "csv"  %> \benchResultFile -> do
    putNormal $ "run bench: " ++ show benchResultFile
    version <- getFoundationVersion $ FoundationVersion foundationDir
    let workingDir = resultDir </> "foundation" </> version
    let benchFile = case stripPrefix (workingDir ++ "/") benchResultFile of
          Nothing -> error "unexpected..."
          Just fp -> dropExtension $ foundationDir </> fp
    need [benchFile]
    cmd benchFile ["--csv", benchResultFile, "-o", benchResultFile -<.> "html"]
  resultDir </> "foundation" </> "*" </> "doc" </> "index.html"  %> \docTracker -> do
    currentDir <- getCurrentDir $ CurrentDir ()
    let docTrackerDir = currentDir </> dropFileName docTracker
    putNormal $ "run haddock: " ++ show docTrackerDir
    fetchFoundation $ FoundationFetch foundationDir
    cmd (Cwd foundationDir)
        "stack" "haddock"
        ["--haddock-arguments", "--odir=" ++ docTrackerDir]

  "www/site" %> \_ ->
    cmd (Cwd "www") "stack" ["install", "--local-bin-path", "."]

  phony "www" $ do
    need ["www/site"]
    return ()

  phony "clean" $ do
    putNormal "clean Shake's cache"
    removeFilesAfter "shake_files" ["//*"]
    removeFilesAfter "." ["ShakeFile.o", "ShakeFile.hi"]

  phony "doc" $ do
    version <- getFoundationVersion $ FoundationVersion foundationDir
    let workingDir = resultDir </> "foundation" </> version
    need [workingDir </> "info", workingDir </> "doc" </> "index.html"]

  phony "benchs" $ do
    version <- getFoundationVersion $ FoundationVersion foundationDir
    benchSources <- getFoundationBenchs $ FoundationBenchs foundationDir
    -- 1. we ac
    let workingDir = resultDir </> "foundation" </> version
    need [workingDir </> "info"]
    mapM_ (\b -> need [workingDir </> b -<.> "csv"]) benchSources

  phony "test" $ do
    fetchFoundation $ FoundationFetch foundationDir
    unit $ cmd (Cwd foundationDir) "stack" "test"
    l <- getDirectoryFiles foundationDir ["tests/*.hs", "Foundation.hs", "Foundation//*.hs"]
    trackWrite l

  phony "all" $ do
    need ["test"]
    need ["benchs", "www", "doc"]
