{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Zoli.Core
    ( Token
    , Action
    , Need(..)
    , need_
    , traced
    , always

    , OutPath
    , Rules
    , runRules
    , rule
    , phony
    , want
    ) where

import           Control.Monad.IO.Class (MonadIO(..))
import qualified Development.Shake as Shake
import           System.Directory (doesFileExist)
import           Control.Monad (guard, unless)

import           Zoli.Pattern

-- Action
------------------------------------------------------------------------

data Token a where
  RuleToken :: (Pattern f) => f a -> Token a
  PhonyToken :: String -> Token ()

instance Pattern Token where
  patMatch t s = case t of
    PhonyToken s' -> guard (s == s')
    RuleToken pat -> patMatch pat s

  patInstantiate (PhonyToken s) () = return s
  patInstantiate (RuleToken pat) s = patInstantiate pat s

  patRender = \case
    PhonyToken s -> s
    RuleToken pat -> patRender pat

data Need
  = File !FilePath
  | forall a. Tok (Token a) a

newtype Action a = Action {unAction :: Shake.Action a}
  deriving (Functor, Applicative, Monad, MonadIO)

needToFilePath :: (MonadIO m) => Need -> m FilePath
needToFilePath = \case
  Tok tok x -> return (tok @@ x)
  File fp -> do
    exists <- liftIO (doesFileExist fp)
    unless exists $
      fail ("Needed file " ++ show fp ++ " does not exist.")
    return fp

need_ :: [Need] -> Action ()
need_ needs = Action (Shake.need =<< mapM needToFilePath needs)

traced :: String -> IO a -> Action a
traced s m = Action (Shake.traced s m)

always :: Action ()
always = Action Shake.alwaysRerun

-- Rules
------------------------------------------------------------------------

-- | The tmp filepath where we need to write the target.  It will then
-- be copied to the 'TargetFile'.
type OutPath = FilePath

newtype Rules a = Rules {unRules :: Shake.Rules a}
  deriving (Functor, Applicative, Monad, MonadIO)

runRules :: Rules a -> Shake.Rules a
runRules = unRules

-- | Add a given rule to the build process.
rule ::
     (Pattern f)
  => f a -> (a -> OutPath -> Action ()) -> Rules (Token a)
rule pat rh = Rules $ do
  patRender pat Shake.%> \out -> case patMatch pat out of
    [] -> fail ("Could not match filepath " ++ show out ++ " with pattern " ++ patRender pat)
    [n] -> unAction (rh n out)
    _ : _ -> fail ("Pattern " ++ patRender pat ++ " had multiple matches with filepath " ++ show out)
  return (RuleToken pat)

phony :: String -> Action () -> Rules (Token ())
phony s h = Rules $ do
  Shake.phony s (unAction h)
  return (PhonyToken s)

want :: [Need] -> Rules ()
want needs = Rules (Shake.want =<< mapM needToFilePath needs)
