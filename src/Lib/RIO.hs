{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.RIO (
    module RIO,
    logSticky,
    logStickyDone,
) where

import Control.Lens ((<%=))
import RIO hiding (
    ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    lens,
    logSticky,
    logStickyDone,
    many,
    over,
    preview,
    set,
    sets,
    some,
    to,
    view,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
 )
import qualified RIO.Text as T (pack)

import qualified RIO (logSticky, logStickyDone)
import RIO.Process (HasProcessContext (..))


mkDerivedApp ::
    (HasStateRef s env, HasLogFunc s, HasProcessContext s) =>
    RIO env SimpleApp
mkDerivedApp = do
    logFun <- logFuncL <%= id
    procContext <- processContextL <%= id
    mkSimpleApp logFun $ Just procContext


instance Display String where
    display = display . T.pack


logStickyCommon ::
    (HasStateRef s env, HasLogFunc s, HasProcessContext s) =>
    (Utf8Builder -> RIO SimpleApp ()) ->
    Text ->
    RIO env ()
logStickyCommon logger contents = do
    derivedApp <- mkDerivedApp
    runRIO derivedApp $ logger $ display contents


logSticky ::
    (HasStateRef s env, HasLogFunc s, HasProcessContext s) =>
    Text ->
    RIO env ()
logSticky = logStickyCommon RIO.logSticky


logStickyDone ::
    (HasStateRef s env, HasLogFunc s, HasProcessContext s) =>
    Text ->
    RIO env ()
logStickyDone = logStickyCommon RIO.logStickyDone
