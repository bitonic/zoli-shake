module Zoli.FileUtils
  ( mkdirP
  , rmDir
  ) where

import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

import           Zoli.Core

mkdirP :: (Monad m) => FilePath -> Rule tok m ()
mkdirP fp = traced ("Creating directory " ++ show fp) (createDirectoryIfMissing True fp)

rmDir :: (Monad m) => FilePath -> Rule tok m ()
rmDir fp = traced ("Removing directory " ++ show fp) (removeDirectoryRecursive fp)