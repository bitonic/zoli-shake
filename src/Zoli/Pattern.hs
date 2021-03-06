{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Zoli.Pattern
  ( Pattern(..)
  , (@@)

  , Pat
  , mkPat
  , pt

  , StrPat(..)
  ) where

import           Data.Typeable (Typeable)
import           Data.List (isPrefixOf, inits, tails)
import           Control.Monad (guard)
import           System.FilePath (isPathSeparator, pathSeparator)
import           Data.List.Extra (split)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import           Control.Arrow (first)
import           Data.String (IsString(..))
import           Data.Typeable (eqT, (:~:)(..), typeRep)
import           Data.Proxy (Proxy(..))

-- |
-- * If @'patInstantiate' pat x = s@, then @'patMatch' pat s = Just x@
class Pattern f where
  patMatch :: f a -> String -> [a]
  patInstantiate :: f a -> a -> Either String String

  -- | Turn this pattern into its shake equivalent.
  patRender :: f a -> String

(@@) :: Pattern f => f a -> a -> String
pat @@ x = case patInstantiate pat x of
  Left err -> error ("@@: " ++ err)
  Right y -> y

data Pat_ a where
  End :: Pat_ ()
  Lit :: String -> Pat_ a -> Pat_ a
  Sep :: Pat_ a -> Pat_ a
  Star :: Pat_ a -> Pat_ (String, a)
  Skip :: Pat_ a -> Pat_ (String, a)

deriving instance Show (Pat_ a)

parse :: forall x. String -> (forall a. (Typeable a) => Pat_ a -> x) -> x
parse s000 cont = case s000 of
  [] ->
    cont End
  ch : ch' : chs | isPathSeparator ch && isPathSeparator ch' ->
    parse chs (\p -> cont (Skip p))
  ch : chs | isPathSeparator ch ->
    parse chs (\p -> cont (Sep p))
  s ->
    let (beforeSep, fromSep) = break isPathSeparator s
    in parse fromSep (\p -> parseSegment beforeSep p cont)
  where
    parseSegment :: forall a. (Typeable a) => String -> Pat_ a -> (forall b. (Typeable b) => Pat_ b -> x) -> x
    parseSegment s00 p0 cont0 = go (split (== '*') s00) cont0
      where
        go :: [String] -> (forall b. (Typeable b) => Pat_ b -> x) -> x
        go s0 cont_ = case s0 of
          [] -> cont_ p0
          [s] -> cont_ (Lit s p0)
          "" : ss -> go ss (\p -> cont_ (Star p))
          s : ss -> go ss (\p -> cont_ (Lit s (Star p)))

parseQ :: String -> TH.ExpQ
parseQ = \case
  [] ->
    [|End|]
  ch : ch' : chs | isPathSeparator ch && isPathSeparator ch' ->
    [|Skip $(parseQ chs)|]
  ch : chs | isPathSeparator ch ->
    [|Sep $(parseQ chs)|]
  s -> do
    let (beforeSep, fromSep) = break isPathSeparator s
    [| $(parseSegmentQ beforeSep (parseQ fromSep)) |]
  where
    parseSegmentQ :: String -> TH.ExpQ -> TH.ExpQ
    parseSegmentQ s0 after = go (split (== '*') s0)
      where
        go :: [String] -> TH.ExpQ
        go = \case
          [] -> after
          [s] -> [|Lit $(TH.stringE s) $(after)|]
          "" : ss -> [|Star $(go ss)|]
          s : ss -> [|Lit $(TH.stringE s) (Star $(go ss))|]

instance Pattern Pat_ where
  patRender = concat . go
    where
      go :: forall a. Pat_ a -> [String]
      go = \case
        End -> []
        Lit s p -> s : go p
        Sep p -> [pathSeparator] : go p
        Star p -> "*" : go p
        Skip p -> "//" : go p

  patInstantiate pat0 x0 = concat <$> go pat0 x0
    where
      go :: forall a. Pat_ a -> a -> Either String [String]
      go End () = return []
      go (Lit s p) x = (s :) <$> go p x
      go (Sep p) x = ([pathSeparator] :) <$> go p x
      go (Star p) (s, x) = if any isPathSeparator s
        then Left ("Trying to instantiate string with path separator: " ++ show s)
        else (s :) <$> go p x
      go (Skip p) (s, x) = (s :) <$> go p x

  patMatch pat0 s = case pat0 of
    End -> guard (null s)
    Lit s' p -> do
      guard (s' `isPrefixOf` s)
      patMatch p (drop (length s') s)
    Sep p -> case s of
      ch : chs | isPathSeparator ch -> patMatch p chs
      _ -> []
    Star p -> do
      let (beforeSep, fromSep) = break isPathSeparator s
      (beforeSepA, beforeSepB) <- zip (inits beforeSep) (tails beforeSep)
      (beforeSepA, ) <$> patMatch p (beforeSepB ++ fromSep)
    Skip p -> do
      let allPrefixes s' = let
            (beforeSep, fromSep) = break isPathSeparator s'
            in if null fromSep
              then [([], fromSep)]
              else let
                fromSep' = tail fromSep
                in map (first (++ [pathSeparator])) ((beforeSep, fromSep') : allPrefixes fromSep')
      (before, after) <- allPrefixes s
      (before ,) <$> patMatch p after

class Flatten a where
  type FlattenTo a :: *
  flatten :: a -> FlattenTo a
  unFlatten :: FlattenTo a -> a

instance Flatten () where
  type FlattenTo () = ()
  flatten = id
  unFlatten = id

instance Flatten (a, ()) where
  type FlattenTo (a, ()) = a
  flatten (x, ()) = x
  unFlatten x = (x, ())

instance Flatten (a, (b, ())) where
  type FlattenTo (a, (b, ())) = (a, b)
  flatten (a, (b, ())) = (a, b)
  unFlatten (a, b) = (a, (b, ()))

instance Flatten (a, (b, (c, ()))) where
  type FlattenTo (a, (b, (c, ()))) = (a, b, c)
  flatten (a, (b, (c, ()))) = (a, b, c)
  unFlatten (a, b, c) = (a, (b, (c, ())))

instance Flatten (a, (b, (c, (d, ())))) where
  type FlattenTo (a, (b, (c, (d, ())))) = (a, b, c, d)
  flatten (a, (b, (c, (d, ())))) = (a, b, c, d)
  unFlatten (a, b, c, d) = (a, (b, (c, (d, ()))))

instance Flatten (a, (b, (c, (d, (e, ()))))) where
  type FlattenTo (a, (b, (c, (d, (e, ()))))) = (a, b, c, d, e)
  flatten (a, (b, (c, (d, (e, ()))))) = (a, b, c, d, e)
  unFlatten (a, b, c, d, e) = (a, (b, (c, (d, (e, ())))))

instance Flatten (a, (b, (c, (d, (e, (f, ())))))) where
  type FlattenTo (a, (b, (c, (d, (e, (f, ())))))) = (a, b, c, d, e, f)
  flatten (a, (b, (c, (d, (e, (f, ())))))) = (a, b, c, d, e, f)
  unFlatten (a, b, c, d, e, f) = (a, (b, (c, (d, (e, (f, ()))))))

instance Flatten (a, (b, (c, (d, (e, (f, (g, ()))))))) where
  type FlattenTo (a, (b, (c, (d, (e, (f, (g, ()))))))) = (a, b, c, d, e, f, g)
  flatten (a, (b, (c, (d, (e, (f, (g, ()))))))) = (a, b, c, d, e, f, g)
  unFlatten (a, b, c, d, e, f, g) = (a, (b, (c, (d, (e, (f, (g, ())))))))

data Pat a where
  Pat :: Flatten b => Pat_ b -> Pat (FlattenTo b)

deriving instance Show (Pat a)

mkPat :: String -> TH.ExpQ
mkPat fp = [|Pat $(parseQ fp)|]

pt :: TH.QuasiQuoter
pt = TH.QuasiQuoter
  { TH.quoteExp = mkPat
  , TH.quotePat = error "pt: cannot quote pattern"
  , TH.quoteType = error "pt: cannot quote type"
  , TH.quoteDec = error "pt: cannot quote dec"
  }

instance Pattern Pat where
  patRender (Pat x) = patRender x
  patInstantiate (Pat p) x = patInstantiate p (unFlatten x)
  patMatch (Pat p) s = map flatten (patMatch p s)

instance IsString (Pat ()) where
  fromString s = parse s $ \(pat :: Pat_ a) -> case eqT :: Maybe (a :~: ()) of
    Nothing -> error ("Pat fromString: requested type () is not the pattern's " ++ patRender pat ++ " type of " ++ show (typeRep (Proxy @a)))
    Just Refl -> Pat pat

data StrPat a where
  StrPat :: String -> StrPat ()

instance Pattern StrPat where
  patRender (StrPat s) = s
  patMatch (StrPat s) s' = guard (s == s')
  patInstantiate (StrPat s) () = return s