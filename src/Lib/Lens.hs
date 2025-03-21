{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Lens where

import Control.Lens (Prism', prism', review, (^?))
import RIO hiding (
    ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    lens,
    over,
    preview,
    set,
    sets,
    to,
    view,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
 )


prismProduct :: Prism' s a -> Prism' s b -> Prism' s (a, b)
prismProduct pa pb =
    prism'
        (\(a, _) -> review pa a)
        (\s -> (,) <$> s ^? pa <*> s ^? pb)
