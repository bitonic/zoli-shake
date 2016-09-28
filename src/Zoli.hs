{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
module Zoli
  ( -- * Rules
    Rule
  , Need(..)
  , need
  , need_
  , traced

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

    -- * Running
  , mkRules
  ) where

import           Zoli.Core
import           Zoli.Pattern
import           Zoli.Run

need :: (Monad m, Pattern tok) => [Need tok] -> Rule tok m [FilePath]
need needs = do
  need_ needs
  return
    [ case tok of
        Tok pat s -> pat @@ s
        File fp -> fp
    | tok <- needs
    ]
