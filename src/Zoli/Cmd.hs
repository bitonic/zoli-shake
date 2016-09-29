module Zoli.Cmd
  ( -- * Options
    Shake.CmdOption(..)

    -- * Results
  , Shake.CmdResult
  , ProcessHandle
  , ExitCode
  , Shake.CmdLine(..)
  , Shake.CmdTime(..)
  , Shake.Process
  , Shake.Exit(..)
  , Shake.Stdouterr(..)
  , Shake.Stderr(..)
  , Shake.Stdout(..)

    -- * Running
  , cmd
  , cmd_
  ) where

import qualified Development.Shake as Shake
import qualified Development.Shake.Command as Shake
import           System.Process (ProcessHandle)
import           System.Exit (ExitCode)
import           Data.List (intercalate)
import           Data.Char (isSpace)

import           Zoli.Core

cmd :: (Shake.CmdResult r) => [Shake.CmdOption] -> String -> [String] -> Action r
cmd opts ex args = traced (intercalate " " (map showArg (ex : args))) (Shake.cmd opts ex args)
  where
    showArg arg = if any isSpace arg then show arg else arg

cmd_ :: (Shake.CmdResult r) => String -> [String] -> Action r
cmd_ = cmd []
