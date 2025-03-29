{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Run where

import Control.Arrow.Unicode ((⋙))
import Control.Lens
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.CaseInsensitive (foldedCase)
import Data.Char (toLower)
import Data.IMF.Syntax
import Data.MIME
import Data.MIME.Base64
import Data.MIME.TransferEncoding
import Data.String.Conversions
import Data.Tuple.Extra (uncurry3)
import Formatting (format, (%))
import Formatting.Combinators (lpadded)
import Formatting.Formatters (int, text)
import Import
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import Replace.Attoparsec.ByteString
import System.Path
import Text.Pretty.Simple
import Text.Taggy.Lens
import Text.URI (URI, mkURIBs, unRText)
import Text.URI.Lens (uriPath)


run :: RIO AppEnv ()
run = do
    targetFile <- view $ cliOptions . mhtFiles
    imgTypes <- view $ cliOptions . imageTypes
    rawData <- BS.readFile $ toString targetFile
    minW <- view $ cliOptions . minWidth
    minH <- view $ cliOptions . minHeight
    itraverse_ writeImageData
        $ concatMap (filter (filterBySize minW minH)
            . mapMaybe imageBS
            . either (pure . Left) (map Right))
        $ rawData ^.. allImages imgTypes
  where
    allImages imgTypes =
        parsedMIME . flatten . to (filterImg imgTypes) . _Just
    filterImg imgTypes msg = case msg ^. contentType . ctType of
        "image" ->
            if msg ^. contentType . ctSubtype `elem` map (cs ⋙ mk) imgTypes
                then Just $ Left msg
                else Nothing
        "application" ->
            if msg ^. contentType . ctSubtype == "octet-stream"
                then
                    let ext = T.drop 1 $ msg ^. fileFull . to (takeExtension ⋙ cs)
                        imtype = if mk ext == mk "jpg" then "jpeg" else ext
                     in if mk imtype `elem` map mk imgTypes
                            then
                                Just $ Left $ msg
                                    & contentType . ctType .~ "image"
                                    & contentType . ctSubtype .~ mk (cs imtype)
                            else Nothing
                else Nothing
        "text" -> case msg ^. contentType . ctSubtype of
            "html" ->
                Just
                    $ Right
                    $ msg
                    ^.. plainText
                        . to cs
                        . html
                        . allNamed (only "img")
                        . filtered isDataUrlImage
            _ -> Nothing
        _ -> Nothing
      where
        plainText =
            entities
                . transferDecodedBytes
                -- Type annotation is needed to satisfy
                -- `AsTransferEncodingError e => ... (Either e ByteString)`
                -- in the type annotation for `transferDecodedBytes`
                -- although it is discarded with `_Right`.
                . (_Right :: Prism' (Either EncodingError ByteString) ByteString)
        isDataUrlImage =
            isJust . preview (attr "src" . _Just . to cs . filtered isDataUrlSrc)
        isDataUrlSrc = isRight . parseOnly isDataImageBase64
        isDataImageBase64 = do
            _ <- string "data:image/"
            _ <- choice $ stringCI <$> map cs imgTypes
            _ <- string ";base64,"
            return ()


filterBySize :: Int -> Int -> ImageInfoData -> Bool
filterBySize minW minH (ImageInfoData _ imgW imgH _ _) =
  imgW >= minW && imgH >= minH


imgBase64Parser :: Parser (ContentType, ByteString)
imgBase64Parser = do
    _ <- string "data:"
    ct <- parseContentType
    _ <- string ";base64,"
    (dat, _) <- anyTill endOfInput
    return (ct, dat)


writeImageData :: Int -> ImageInfoData -> RIO AppEnv ()
writeImageData n (ImageInfoData imType _ _ src bs) = do
    logInfo $ display outputFile
    BS.writeFile outputFile bs
  where
    outputFile = if src == ""
      then cs $ format spec (n + 1) $ cs $ toLower <$> show imType
      else cs $ format spec2 (n + 1) (cs src) $ cs $ toLower <$> show imType
    spec = lpadded 3 '0' int % "." % text
    spec2 = lpadded 3 '0' int % "-" % text % "." % text


imageBS :: Either MIMEMessage Element -> Maybe ImageInfoData
imageBS img = bs >>= preview imgDim >>= uncurry3 imageInfo
  where
    bs = either bsFromMsg bsFromElem img
    src = either srcFromMsg srcFromElem img
    imageInfo t w h = ImageInfoData t w h src <$> bs


bsFromMsg :: MIMEMessage -> Maybe ByteString
bsFromMsg msg =
    msg ^? entities . transferDecodedBytes . _right
  where
    _right = _Right :: Prism' (Either EncodingError ByteString) ByteString


bsFromElem :: Element -> Maybe ByteString
bsFromElem elm =
    img ^? _Just . _2 . clonePrism contentTransferEncodingBase64
  where
    img = elm ^? attr "src" . _Just . cs' . parsed imgBase64Parser
    cs' = to (cs :: Text -> ByteString)


srcFromMsg :: MIMEMessage -> Text
srcFromMsg msg =
    msg ^. fileFull . to (takeBaseName ⋙ toString ⋙ cs)


-- Mainly <img> captured from <camvas>, thus no src name
srcFromElem :: Element -> Text
srcFromElem _ = ""


fileFull :: Fold MIMEMessage RelFile
fileFull =
    header "Content-Location"
        . to mkURIBs
        . _Just
        . uriFileName


fileBase :: Fold MIMEMessage RelFile
fileBase = fileFull . to takeBaseName


uriFileName :: Fold URI RelFile
uriFileName = uriPath . _last . to (unRText ⋙ cs ⋙ relFile)


jpegToJpg :: (Eq a, IsString a) => a -> a
jpegToJpg subtype =
    if subtype == "jpeg" then "jpg" else subtype
