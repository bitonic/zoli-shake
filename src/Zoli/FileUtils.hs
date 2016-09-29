module Zoli.FileUtils
  ( mkdirP
  , rmDir
  ) where

import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

import           Zoli.Core

mkdirP :: FilePath -> Action ()
mkdirP fp = traced ("Creating directory " ++ show fp) (createDirectoryIfMissing True fp)

rmDir :: FilePath -> Action ()
rmDir fp = traced ("Removing directory " ++ show fp) (removeDirectoryRecursive fp)