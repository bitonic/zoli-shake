{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module Zoli.Core
    ( Rule(..)
    , RuleF(..)
    , Need(..)
    , need_
    , traced

    , OutPath
    , RuleHandler
    , Rules(..)
    , RulesF(..)
    , rule
    , phony
    , want
    ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Free (FreeT, wrap)

import           Zoli.Pattern

-- Rule
------------------------------------------------------------------------

newtype Rule tok m a = Rule {unRule :: FreeT (RuleF tok) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

data RuleF tok a
  = Need ![Need tok] a
  | forall b. Traced !String (IO b) (b -> a)

instance Functor (RuleF tok) where
  fmap f = \case
    Need needs x -> Need needs (f x)
    Traced s m h -> Traced s m (fmap f h)

data Need tok
  = forall a. Tok !(tok a) !a
  | File !FilePath
  -- | Refer to an existing file.
  --
  -- IMPORTANT: The intended use for 'File' is only for which which
  -- exist *before* the build process starts.  Every file created as part
  -- of the build process should be referred to using 'Token's returned by
  -- 'rule'.

need_ :: (Monad m) => [Need tok] -> Rule tok m ()
need_ needs = Rule (wrap (Need needs (return ())))

traced :: (Monad m) => String -> IO a -> Rule tok m a
traced s m = Rule (wrap (Traced s m return))

-- Rules
------------------------------------------------------------------------

-- | The tmp filepath where we need to write the target.  It will then
-- be copied to the 'TargetFile'.
type OutPath = FilePath

type RuleHandler tok r a = a -> OutPath -> Rule tok r ()

newtype Rules tok (r :: * -> *) m a = Rules {unRules :: FreeT (RulesF tok r) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

data RulesF tok r a
  = Phony !String (Rule tok r ()) (tok () -> a)
  | forall f p. (Pattern f) => AddRule !(f p) (RuleHandler tok r p) (tok p -> a)
  | Want ![Need tok] a

instance Functor (RulesF tok r) where
  fmap f (Phony s r h) = Phony s r (fmap f h)
  fmap f (AddRule pat rh h) = AddRule pat rh (fmap f h)
  fmap f (Want n h) = Want n (f h)

-- | Add a given rule to the build process.
rule ::
     (Monad m, Pattern tok, Pattern f)
  => f a -> RuleHandler tok r a -> Rules tok r m (tok a)
rule pat rh = Rules (wrap (AddRule pat rh return))

phony :: (Monad m, Pattern tok) => String -> Rule tok r () -> Rules tok r m (tok ())
phony s h = Rules (wrap (Phony s h return))

want :: (Monad m, Pattern tok) => [Need tok] -> Rules tok r m ()
want needs = Rules (wrap (Want needs (return ())))
