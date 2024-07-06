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

module Lib.RIO
  ( module Control.Lens
  , module RIO
  ) where

import Control.Lens
import RIO hiding
  ( ASetter
  , ASetter'
  , Getting
  , Lens
  , Lens'
  , SimpleGetter
  , lens
  , many
  , over
  , preview
  , set
  , sets
  , some
  , to
  , view
  , (%~)
  , (.~)
  , (^.)
  , (^..)
  , (^?)
  )
import qualified RIO.Text as T


instance Display String where
  display = display . T.pack
