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
    itraverse_ writeImage
        $ concatMap (either (pure . Left) (map Right))
        $ rawData
        ^.. allImages imgTypes
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


imgBase64Parser :: Parser (ContentType, ByteString)
imgBase64Parser = do
    _ <- string "data:"
    ct <- parseContentType
    _ <- string ";base64,"
    (dat, _) <- anyTill endOfInput
    return (ct, dat)


writeImage :: Int -> Either MIMEMessage Element -> RIO AppEnv ()
writeImage n img = case img of
    Left msg -> writeMimeMsgImg n msg
    Right elemt -> writeDataUrlImage n elemt


writeDataUrlImage :: Int -> Element -> RIO AppEnv ()
writeDataUrlImage n elemt = do
    minW <- view $ cliOptions . minWidth
    minH <- view $ cliOptions . minHeight
    case bs ^? _Just . imgDim of
        Just (imgType, w, h) -> when (w >= minW && h >= minH) $ do
            logInfo $ display outName
            traverse_ (BS.writeFile outName) bs
          where
            outName = outputFile imgType
        _ -> return ()
  where
    bs = img ^? _Just . _2 . clonePrism contentTransferEncodingBase64
    img = elemt ^? attr "src" . _Just . decodeDataUrl
    decodeDataUrl = to (cs :: Text -> ByteString) . parsed imgBase64Parser
    --  fileExt = foldedCase $ img ^. _Just . _1 . ctSubtype . to jpegToJpg
    outputFile imgType = cs $ format spec (n + 1) $ cs $ toLower <$> show imgType
    spec = lpadded 3 '0' int % "." % text


writeMimeMsgImg :: Int -> MIMEMessage -> RIO AppEnv ()
writeMimeMsgImg n msg = do
    minW <- view $ cliOptions . minWidth
    minH <- view $ cliOptions . minHeight
    -- traverse_ (logInfo . display) $ msg ^? outputFileF . to toString
    -- sequenceA_ $ msg ^? writeImageFile
    case bs ^? _Just . imgDim of
        Just (imgType, w, h) -> when (w >= minW && h >= minH) $ do
            logInfo $ display outName
            traverse_ (BS.writeFile outName) bs
          where
            outName = outputFile imgType
        _ -> return ()
  where
    bs = msg ^? entities . transferDecodedBytes . _right
    -- To satisfy `AsTransferEncodingError e => ... (Either e ByteString)`
    _right = _Right :: Prism' (Either EncodingError ByteString) ByteString
    outputFile imgType = cs $ format spec (n + 1) $ cs $ toLower <$> show imgType
    spec = lpadded 3 '0' int % "." % text

    {- writeImageFile :: Fold MIMEMessage (RIO AppEnv ())
    writeImageFile =
        runFold
            $ BS.writeFile
            <$> Fold (outputFile . to toString)
            <*> Fold rawData

    outputFileF :: Fold MIMEMessage RelFile
    outputFileF = runFold $ mkOutputFileName <$> Fold fileBase <*> Fold fileExt

    rawData :: Fold MIMEMessage ByteString
    rawData =
        entities
            . transferDecodedBytes
            -- Type annotation is needed to satisfy
            -- `AsTransferEncodingError e => ... (Either e ByteString)`
            -- in the type annotation for `transferDecodedBytes`
            -- although it is discarded with `_Right`.
            . (_Right :: Prism' (Either EncodingError ByteString) ByteString)

    mkOutputFileName :: RelFile -> String -> RelFile
    mkOutputFileName =
        addExtension . mapFileName (cs (format counter (n + 1)) <>)
      where
        counter = lpadded 3 '0' int % "-"

    fileExt :: Getter MIMEMessage String
    fileExt =
        contentType
            . ctSubtype
            . to (foldedCase ⋙ jpegToJpg ⋙ cs) -}

    {--
    case bs ^? _Just . imgDim of
        Just (imgType, w, h) -> when (w >= minW && h >= minH) $ do
            logInfo $ display outName
            traverse_ (BS.writeFile outName) bs
          where
            outName = outputFile imgType
        _ -> return ()

    img = elemt ^? attr "src" . _Just . decodeDataUrl
    decodeDataUrl = to (cs :: Text -> ByteString) . parsed imgBase64Parser
    --  fileExt = foldedCase $ img ^. _Just . _1 . ctSubtype . to jpegToJpg
    outputFile imgType = cs $ format spec (n + 1) $ cs $ toLower <$> show imgType
    spec = lpadded 3 '0' int % "." % text
    bs = img ^? _Just . _2 . clonePrism contentTransferEncodingBase64
--}


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
