{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Zoli.Run
  ( mkRules
  ) where

import           Control.Monad (guard)
import qualified Development.Shake as Shake
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Free (FreeT(..), FreeF(..))

import           Zoli.Core
import           Zoli.Pattern

data Token a where
  RuleToken :: (Pattern f) => f a -> Token a
  PhonyToken :: String -> Token ()

instance Pattern Token where
  patMatch t s = case t of
    PhonyToken s' -> guard (s == s')
    RuleToken pat -> patMatch pat s

  patInstantiate (PhonyToken s) () = s
  patInstantiate (RuleToken pat) s = patInstantiate pat s

  patRender = \case
    PhonyToken s -> s
    RuleToken pat -> patRender pat

mkRules :: forall m a.
     (Monad m)
  => (forall n tok. (MonadIO n, Pattern tok) => Rules tok n m a)
  -> m (Shake.Rules a)
mkRules = goRules . unRules
  where
    goRules :: FreeT (RulesF Token Shake.Action) m a -> m (Shake.Rules a)
    goRules ft = do
      ff <- runFreeT ft
      case ff of
        Pure x -> return (return x)
        Free rf -> case rf of
          Phony phonyName r cont -> do
            rules <- goRules (cont (PhonyToken phonyName))
            return $ do
              Shake.phony phonyName (goRule (unRule r))
              rules
          AddRule pat r cont -> do
            rules <- goRules (cont (RuleToken pat))
            return $ do
              patRender pat Shake.%> \out -> case patMatch pat out of
                Nothing -> error ("Could not match filepath " ++ show out ++ " with pattern " ++ patRender pat)
                Just n -> goRule (unRule (r n out))
              rules
          Want needs cont -> do
            rules <- goRules cont
            return $ do
              Shake.want
                [ case n of
                    Tok tok x -> tok @@ x
                    File fp -> fp
                | n <- needs ]
              rules

    goRule :: forall b. FreeT (RuleF Token) Shake.Action b -> Shake.Action b
    goRule ft = do
      ff <- runFreeT ft
      case ff of
        Pure x -> return x
        Free rf -> case rf of
          Need ns ft' -> do
            Shake.need
              [ case n of
                  Tok tok x -> tok @@ x
                  File fp -> fp
              | n <- ns ]
            goRule ft'
          Traced s m h -> do
            x <- Shake.traced s m
            goRule (h x)
          Always cont -> do
            Shake.alwaysRerun
            goRule cont
