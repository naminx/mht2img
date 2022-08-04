{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.MIME
import Formatting (format, (%))
import Formatting.Combinators (lpadded)
import Formatting.Formatters (int, text)
import Import
import qualified RIO.ByteString as BS
import qualified RIO.Text.Lazy as TL (pack, unpack)
import System.FilePath.Glob (glob)
import System.FilePath.Lens (basename)
import Text.URI (URI, mkURIBs)
import Text.URI.Lens (uriPath)


run :: RIO AppEnv ()
run = do
    patterns <- view $ cliOptions . mhtFiles
    targetFiles <- liftIO $ traverse glob patterns
    traverse_ go $ concat targetFiles
  where
    go file = do
        rawData <- BS.readFile file
        itraverse_ writeImage $ rawData ^.. allImages
    allImages =
        parsedMIME . flatten
            . filteredBy (contentType . ctType . only "image")
            . filteredBy (contentType . ctSubtype . oneOf targetedSubtype)
    targetedSubtype = NE.fromList ["jpeg", "png", "webp"]


writeImage :: Int -> MIMEMessage -> RIO AppEnv ()
writeImage n msg = do
    traverse_ (logInfo . display) $ msg ^? outputFile
    sequenceA_ $ msg ^? writeImageFile
  where
    writeImageFile :: Fold MIMEMessage (RIO AppEnv ())
    writeImageFile = liftFold2 BS.writeFile outputFile rawData

    outputFile :: Fold MIMEMessage FilePath
    outputFile = liftFold2 composeName nameBase nameExt

    rawData :: Fold MIMEMessage ByteString
    rawData = entities . decodedBytes . _Right

    composeName :: FilePath -> FilePath -> FilePath
    composeName fileBase fileExt =
        TL.unpack $ format spec (n + 1) (TL.pack fileBase) (TL.pack fileExt)
      where
        spec = lpadded 3 '0' int % "-" % text % "." % text

    nameExt :: Getter MIMEMessage FilePath
    nameExt = contentType . ctSubtype . _String . caseOf' (only "jpeg") set "jpg"

    nameBase :: Fold MIMEMessage FilePath
    nameBase = header "Content-Location" . toURI . uriFileName . basename

    toURI :: Fold ByteString URI
    toURI = to mkURIBs . _Just

    uriFileName :: Fold URI FilePath
    uriFileName = uriPath . _last . _String
