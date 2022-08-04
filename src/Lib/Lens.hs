{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Lens where

import Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.Monoid (First)
import Lib.RIO hiding (lift)


type ToLike' s a = forall p f. (Profunctor p, Contravariant f) => Optic' p f s a


oneOf :: Eq a => NonEmpty a -> Prism' a ()
oneOf xs = prism' (\() -> NE.head xs) $ guard . (`elem` xs)


-- Example:
--   (10, 20, 30) ^. caseOf _2 (only 20) set 0 == (10, 0, 30)
--   [10, 20, 30] ^. caseOf _head (only 10) over (+1) == [11, 20, 30]
caseOf ::
    (Indexable i t, Applicative f) =>
    ((c -> f c) -> d) ->
    Getting (First i) c i ->
    ((t c (f c) -> d) -> parameter -> a -> b) ->
    parameter ->
    ToLike' a b
caseOf focus criteria action parameter =
    to $ action (focus . filteredBy criteria) parameter


-- Example:
--   [] ^. caseOf' _Empty over (5 :) == [5]
caseOf' ::
    (Indexable i t, Applicative f) =>
    Getting (First i) c i ->
    ((t c (f c) -> c -> f c) -> parameter -> a -> b) ->
    parameter ->
    ToLike' a b
caseOf' = caseOf id


liftFold2 :: (a -> b -> c) -> Fold s a -> Fold s b -> Fold s c
liftFold2 f a b = runFold $ f <$> Fold a <*> Fold b


liftFold3 :: (a -> b -> c -> d) -> Fold s a -> Fold s b -> Fold s c -> Fold s d
liftFold3 f a b c = runFold $ f <$> Fold a <*> Fold b <*> Fold c


liftFold4 :: (a -> b -> c -> d -> e) -> Fold s a -> Fold s b -> Fold s c -> Fold s d -> Fold s e
liftFold4 f a b c d = runFold $ f <$> Fold a <*> Fold b <*> Fold c <*> Fold d
