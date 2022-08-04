{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.MIME where

import Control.Lens
import Data.MIME
import Data.MIME.TransferEncoding (TransferEncodingError, transferDecodedBytes)
import Lib.Lens
import Lib.RIO


parsedMIME :: Fold ByteString MIMEMessage
parsedMIME = parsed $ message mime


makePrisms ''MIME


instance Plated MIMEMessage where
    plate = body . _Multipart . _3 . each


flatten :: Fold MIMEMessage MIMEMessage
flatten = to universe . folded


decodedBytes :: ToLike' WireEntity (Either TransferEncodingError ByteString)
decodedBytes = transferDecodedBytes
