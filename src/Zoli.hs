{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Zoli
  ( -- * Rules
    Token
  , Action
  , Need(..)
  , need
  , need_
  , needFiles
  , needToks
  , needToks_
  , traced
  , always

  , OutPath
  , Rules
  , rule
  , phony
  , want
  , wantToks
  , wantToks_

    -- * Patterns
  , Pattern
  , (@@)
  , Pat
  , mkPat
  , pt
  , StrPat(..)

    -- * Commands
  , module Zoli.Cmd

    -- * File utilities
  , module Zoli.FileUtils

    -- * Running
  , runRules
  ) where

import           Zoli.Core
import           Zoli.Pattern
import           Zoli.Cmd
import           Zoli.FileUtils

need :: [Need] -> Action [FilePath]
need needs = do
  need_ needs
  return
    [ case tok of
        Tok pat s -> pat @@ s
        File fp -> fp
    | tok <- needs
    ]

needFiles :: [FilePath] -> Action ()
needFiles files = need_ (map File files)

needToks :: [(Token a, a)] -> Action [FilePath]
needToks toks = need (map (uncurry Tok) toks)

needToks_ :: [Token ()] -> Action [FilePath]
needToks_ toks = needToks (zip toks (repeat ()))

wantToks :: [(Token a, a)] -> Rules ()
wantToks toks = want (map (uncurry Tok) toks)

wantToks_ :: [Token ()] -> Rules ()
wantToks_ toks = wantToks (zip toks (repeat ()))