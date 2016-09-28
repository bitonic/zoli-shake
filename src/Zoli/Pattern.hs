{-# LANGUAGE GADTs #-}
module Zoli.Pattern
  ( Pattern(..)
  , (@@)
  , SimplePat
  , pat1
  , pat2
  ) where

import           Data.Typeable (Typeable)
import           Data.List (isPrefixOf)
import           Control.Monad (guard)

-- |
-- * If @'patInstantiate' pat x = s@, then @'patMatch' pat s = Just x@
class Pattern f where
  patMatch :: f a -> String -> Maybe a
  patInstantiate :: f a -> a -> String

  -- | Turn this pattern into its shake equivalent.
  patRender :: f a -> String

(@@) :: Pattern f => f a -> a -> String
(@@) = patInstantiate

data SimplePat a where
  SimplePat1 :: String -> SimplePat ()
  SimplePat2 :: String -> String -> SimplePat String
  deriving (Typeable)

instance Pattern SimplePat where
  patMatch pat s = case pat of
    SimplePat1 s' -> guard $ s == s'
    SimplePat2 s1 s2 -> do
      guard (s1 `isPrefixOf` s)
      guard (reverse s2 `isPrefixOf` reverse s)
      guard (length s >= length s1 + length s2)
      return (dropEnd (length s2) (drop (length s1) s))

  patInstantiate (SimplePat1 s) () = s
  patInstantiate (SimplePat2 s1 s2) s = s1 ++ s ++ s2

  patRender (SimplePat1 s) = s
  patRender (SimplePat2 s1 s2) = s1 ++ "//*" ++ s2

pat1 :: String -> SimplePat ()
pat1 = SimplePat1

pat2 :: String -> String -> SimplePat String
pat2 = SimplePat2

dropEnd :: Int -> String -> String
dropEnd n = reverse . drop n . reverse
