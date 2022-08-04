{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.ToString where

import Control.Lens
import Data.String.ToString (ToString (..))
import Lib.Lens
import Lib.RIO
import Path (File, SomeBase, fromSomeFile)
import qualified RIO.Text as T
import Text.URI (RText, RTextLabel, unRText)


class LensToString a where
    _String :: ToLike' a String


instance ToString a => LensToString a where
    _String = to toString


instance ToString (RText (label :: RTextLabel)) where
    toString = unRText >>> T.unpack


instance ToString (SomeBase File) where
    toString = fromSomeFile
