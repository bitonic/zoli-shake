{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Zoli
  ( -- * Rules
    Rule
  , Need(..)
  , need
  , need_
  , needFiles
  , needToks
  , needToks_
  , traced
  , always

  , OutPath
  , RuleHandler
  , Rules
  , rule
  , phony
  , want
  , wantToks
  , wantToks_

    -- * Patterns
  , Pattern
  , (@@)
  , SimplePat
  , simplePat1
  , simplePat2
  , Pat
  , mkPat
  , pt
  , StrPat(..)

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

needToks_ :: (Monad m, Pattern tok) => [tok ()] -> Rule tok m [FilePath]
needToks_ toks = needToks (zip toks (repeat ()))

wantToks :: (Monad m, Pattern tok) => [(tok a, a)] -> Rules tok r m ()
wantToks toks = want (map (uncurry Tok) toks)

wantToks_ :: (Monad m, Pattern tok) => [tok ()] -> Rules tok r m ()
wantToks_ toks = wantToks (zip toks (repeat ()))