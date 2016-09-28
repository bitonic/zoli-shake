{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
module Zoli
  ( -- * Rules
    Rule
  , Need(..)
  , need
  , need_
  , needFiles
  , needToks
  , traced
  , always

  , OutPath
  , RuleHandler
  , Rules
  , rule
  , phony
  , want

    -- * Patterns
  , Pattern
  , (@@)
  , SimplePat
  , pat1
  , pat2

    -- * Commands
  , module Zoli.Cmd

    -- * File utilities
  , module Zoli.FileUtils

    -- * Running
  , mkRules
  ) where

import           Zoli.Core
import           Zoli.Pattern
import           Zoli.Run
import           Zoli.Cmd
import           Zoli.FileUtils

need :: (Monad m, Pattern tok) => [Need tok] -> Rule tok m [FilePath]
need needs = do
  need_ needs
  return
    [ case tok of
        Tok pat s -> pat @@ s
        File fp -> fp
    | tok <- needs
    ]

needFiles :: (Monad m) => [FilePath] -> Rule tok m ()
needFiles files = need_ (map File files)

needToks :: (Monad m, Pattern tok) => [(tok a, a)] -> Rule tok m [FilePath]
needToks toks = need (map (uncurry Tok) toks)