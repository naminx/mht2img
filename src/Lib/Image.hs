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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Image {- (
                     ImageFormat (..),
                     imgDim,
                 ) -}
where

import Control.Lens (Prism', prism', makeFieldsNoPrefix)
import Data.Binary.Get
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import RIO hiding (ASetter, ASetter', Getting, Lens, Lens', SimpleGetter, lens, over, preview, set, sets, to, view, (%~), (.~), (^.), (^..), (^?))
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL


-- | Supported image formats
data ImageFormat = JPG | PNG | GIF | WEBP | AVIF | ICO
    deriving (Show, Eq)


data ImageInfoData = ImageInfoData
    { _imgType :: ImageFormat
    , _imgWidth :: Int
    , _imgHeight :: Int
    , _imgSrc :: Text
    , _imgData :: ByteString
    }


makeFieldsNoPrefix ''ImageInfoData


-- | Prism for extracting image dimensions
imgDim :: Prism' ByteString (ImageFormat, Int, Int)
imgDim = prism' encodeDimensions extractDimensions
  where
    extractDimensions bs =
        detectPng bs <|> detectJpeg bs <|> detectGif bs <|> detectWebp bs <|> detectAvif bs <|> detectIco bs

    -- Placeholder implementation since we can't meaningfully create an image
    encodeDimensions _ = BS.empty


-- | Extract dimensions from a PNG image
detectPng :: ByteString -> Maybe (ImageFormat, Int, Int)
detectPng bs
    | not (BS.isPrefixOf "\137PNG\r\n\26\n" bs) = Nothing
    | otherwise =
        case runGetOrFail parsePng (BL.fromStrict bs) of
            Left _ -> Nothing
            Right (_, _, (w, h)) -> Just (PNG, w, h)
  where
    parsePng = do
        skip 8 -- Skip signature
        skip 4 -- Skip chunk length
        chunkType <- getByteString 4
        if chunkType /= "IHDR"
            then fail "Not an IHDR chunk"
            else do
                width <- getWord32be
                height <- getWord32be
                return (fromIntegral width, fromIntegral height)


-- | Extract dimensions from a GIF image
detectGif :: ByteString -> Maybe (ImageFormat, Int, Int)
detectGif bs
    | not (BS.isPrefixOf "GIF87a" bs || BS.isPrefixOf "GIF89a" bs) = Nothing
    | BS.length bs < 10 = Nothing
    | otherwise =
        case runGetOrFail parseGif (BL.fromStrict bs) of
            Left _ -> Nothing
            Right (_, _, (w, h)) -> Just (GIF, w, h)
  where
    parseGif = do
        skip 6 -- Skip signature
        width <- getWord16le
        height <- getWord16le
        return (fromIntegral width, fromIntegral height)


-- | Extract dimensions from an ICO image
detectIco :: ByteString -> Maybe (ImageFormat, Int, Int)
detectIco bs
    | not (BS.isPrefixOf "\0\0\1\0" bs) = Nothing
    | BS.length bs < 8 = Nothing
    | otherwise = do
        let w = fromIntegral $ BS.index bs 6
            h = fromIntegral $ BS.index bs 7
            width = if w == 0 then 256 else w
            height = if h == 0 then 256 else h
        return (ICO, width, height)


-- | Extract width and height from a WebP image
-- Returns Nothing if the ByteString is not a valid WebP image
-- or if the dimensions cannot be determined
detectWebp :: ByteString -> Maybe (ImageFormat, Int, Int)
detectWebp bs
    | BS.length bs < 12 = Nothing -- Not enough bytes for basic header
    | not (BS.isPrefixOf "RIFF" bs && BS.isPrefixOf "WEBP" (BS.drop 8 bs)) = Nothing -- Not a WebP file
    | otherwise =
        let vp8Type = BS.take 4 (BS.drop 12 bs)
         in case vp8Type of
                "VP8 " -> getVP8Dimensions (BS.drop 20 bs) -- Skip to VP8 data
                "VP8L" -> getVP8LDimensions (BS.drop 20 bs) -- Skip to VP8L data
                "VP8X" -> getVP8XDimensions bs -- VP8X dimensions are at fixed locations
                _ -> Nothing -- Unknown VP8 type


-- | Extract dimensions from VP8 data (lossy format)
getVP8Dimensions :: ByteString -> Maybe (ImageFormat, Int, Int)
getVP8Dimensions bs
    | BS.length bs < 10 = Nothing -- VP8 frame header is 10 bytes
    | otherwise =
        let
            -- Check for VP8 signature
            b0 = fromIntegral $ BS.index bs 0
            b1 = fromIntegral $ BS.index bs 1
            b2 = fromIntegral $ BS.index bs 2
         in
            if (b0, b1, b2) == (0x9d, 0x01, 0x2a) -- VP8 signature
                then
                    let
                        -- Extract width and height (16-bit values at bytes 6-9)
                        width =
                            fromIntegral (BS.index bs 6)
                                .|. (fromIntegral (BS.index bs 7) `shiftL` 8)
                        height =
                            fromIntegral (BS.index bs 8)
                                .|. (fromIntegral (BS.index bs 9) `shiftL` 8)
                     in
                        Just (WEBP, width .&. 0x3fff, height .&. 0x3fff) -- Mask with 0x3fff (14 bits)
                else Nothing


-- | Extract dimensions from VP8L data (lossless format)
getVP8LDimensions :: ByteString -> Maybe (ImageFormat, Int, Int)
getVP8LDimensions bs
    | BS.length bs < 5 = Nothing -- VP8L frame header is 5 bytes
    | otherwise =
        let signature = BS.index bs 0
         in if signature == 0x2f -- VP8L signature (magic byte)
                then
                    let
                        -- Width and height are encoded in the 4 bytes after signature
                        b0 = fromIntegral $ BS.index bs 1
                        b1 = fromIntegral $ BS.index bs 2
                        b2 = fromIntegral $ BS.index bs 3
                        b3 = fromIntegral $ BS.index bs 4
                        -- Extract width and height using bit manipulation
                        width = 1 + (((b1 .&. 0x3F) `shiftL` 8) .|. b0)
                        height =
                            1
                                + ( ((b3 .&. 0xF) `shiftL` 10)
                                        .|. (b2 `shiftL` 2)
                                        .|. ((b1 .&. 0xC0) `shiftR` 6)
                                  )
                     in
                        Just (WEBP, width, height)
                else Nothing


-- | Extract dimensions from VP8X extended data
getVP8XDimensions :: ByteString -> Maybe (ImageFormat, Int, Int)
getVP8XDimensions bs
    | BS.length bs < 30 = Nothing -- Need at least 30 bytes for VP8X
    | otherwise =
        let
            -- Width is at bytes 24-26, height at bytes 27-29 (24-bit values)
            width =
                fromIntegral (BS.index bs 24)
                    .|. (fromIntegral (BS.index bs 25) `shiftL` 8)
                    .|. (fromIntegral (BS.index bs 26) `shiftL` 16)
            height =
                fromIntegral (BS.index bs 27)
                    .|. (fromIntegral (BS.index bs 28) `shiftL` 8)
                    .|. (fromIntegral (BS.index bs 29) `shiftL` 16)
         in
            Just (WEBP, width + 1, height + 1) -- Adding 1 based on format specification


-- | Extract dimensions from a JPEG image
detectJpeg :: ByteString -> Maybe (ImageFormat, Int, Int)
detectJpeg bs'
    | BS.length bs' < 2 = Nothing
    | not (BS.isPrefixOf "\xFF\xD8" bs') = Nothing -- JPEG signature (SOI marker)'
    | otherwise = findSOF bs' 2
  where
    findSOF :: ByteString -> Int -> Maybe (ImageFormat, Int, Int)
    findSOF bs offset
        | offset + 2 >= BS.length bs = Nothing
        | BS.index bs offset /= 0xFF = findSOF bs (offset + 1)
        | otherwise =
            let marker = BS.index bs (offset + 1)
             in case marker of
                    -- SOF markers (0xC0 through 0xCF, except 0xC4, 0xC8, 0xCC)
                    m
                        | m >= 0xC0 && m <= 0xCF && m /= 0xC4 && m /= 0xC8 && m /= 0xCC ->
                            if offset + 9 < BS.length bs
                                then
                                    let height = fromIntegral (readUInt16BE bs (offset + 5))
                                        width = fromIntegral (readUInt16BE bs (offset + 7))
                                     in Just (JPG, width, height)
                                else Nothing
                    -- Skip other markers
                    _ ->
                        if offset + 4 < BS.length bs
                            then
                                let length' = fromIntegral (readUInt16BE bs (offset + 2))
                                 in if length' < 2 || offset + 2 + length' >= BS.length bs
                                        then Nothing
                                        else findSOF bs (offset + 2 + length')
                            else Nothing


-- \| Read a big-endian 16-bit unsigned integer
readUInt16BE :: ByteString -> Int -> Word16
readUInt16BE bs offset
    | offset + 1 >= BS.length bs = 0
    | otherwise =
        let b0 = fromIntegral (BS.index bs offset)
            b1 = fromIntegral (BS.index bs (offset + 1))
         in (b0 `shiftL` 8) .|. b1


-- | Extract dimensions from an AVIF image by recursively searching for the ISPE box
detectAvif :: ByteString -> Maybe (ImageFormat, Int, Int)
detectAvif bs
    | not (BS.isInfixOf "ftypavif" (BS.take 32 bs)) = Nothing
    | otherwise =
        -- Use a larger buffer to ensure we can find deeply nested boxes
        let limitedBs = BS.take 32768 bs -- 32KB should be enough for metadata
         in case findISPERecursively limitedBs 0 100 of
                Just (width, height) -> Just (AVIF, width, height)
                Nothing -> Nothing


-- | Recursively find the ISPE box anywhere in the box hierarchy
-- Arguments: bytestring, current offset, max recursion depth
findISPERecursively :: ByteString -> Int -> Int -> Maybe (Int, Int)
findISPERecursively _bs _offset 0 = Nothing -- Maximum recursion depth reached
findISPERecursively bs offset depth
    | offset + 8 > BS.length bs = Nothing -- Not enough bytes for a box header
    | otherwise =
        let boxSize = readUInt32BE bs offset
            boxType = BS.take 4 (BS.drop (offset + 4) bs)
         in if boxSize < 8
                then Nothing -- Invalid box size
                else
                    if boxType == "ispe"
                        then -- Found ISPE box, extract dimensions

                            if offset + 20 > BS.length bs
                                then Nothing -- Not enough bytes
                                else
                                    let width = readUInt32BE bs (offset + 12) -- Skip size(4) + type(4) + version/flags(4)
                                        height = readUInt32BE bs (offset + 16)
                                     in Just (fromIntegral width, fromIntegral height)
                        else -- Not an ISPE box, continue searching

                            let nextOffset = offset + fromIntegral boxSize
                             in if nextOffset > BS.length bs
                                    then Nothing -- Box extends beyond data
                                    else
                                        if boxType `elem` ["meta", "iprp", "ipco", "mdat", "moov", "trak", "mdia", "minf", "dinf", "stbl"]
                                            then -- Container box: search inside first, then try next box if needed

                                                let
                                                    -- Skip version and flags for FullBox containers
                                                    contentOffset = if boxType == "meta" then offset + 12 else offset + 8
                                                    boxEnd = offset + fromIntegral boxSize
                                                 in
                                                    -- First try to find ISPE inside this container
                                                    case findISPEInContainer bs contentOffset boxEnd (depth - 1) of
                                                        Just dims -> Just dims
                                                        Nothing ->
                                                            -- If not found in this container, continue with next box
                                                            findISPERecursively bs nextOffset (depth - 1)
                                            else -- Not a container box, skip to next box
                                                findISPERecursively bs nextOffset (depth - 1)


-- | Helper to find ISPE within a container with boundaries
findISPEInContainer :: ByteString -> Int -> Int -> Int -> Maybe (Int, Int)
findISPEInContainer _bs _offset _containerEnd 0 = Nothing -- Recursion limit reached
findISPEInContainer bs offset containerEnd depth
    | offset + 8 > containerEnd || offset + 8 > BS.length bs = Nothing -- Not enough bytes
    | otherwise =
        let boxSize = readUInt32BE bs offset
            boxType = BS.take 4 (BS.drop (offset + 4) bs)
         in if boxSize < 8 || offset + fromIntegral boxSize > containerEnd
                then Nothing -- Invalid box
                else
                    if boxType == "ispe"
                        then -- Found ISPE box, extract dimensions

                            if offset + 20 > BS.length bs
                                then Nothing -- Not enough bytes
                                else
                                    let width = readUInt32BE bs (offset + 12) -- Skip size(4) + type(4) + version/flags(4)
                                        height = readUInt32BE bs (offset + 16)
                                     in Just (fromIntegral width, fromIntegral height)
                        else -- Not an ISPE box

                            let nextOffset = offset + fromIntegral boxSize
                             in if boxType `elem` ["meta", "iprp", "ipco", "mdat", "moov", "trak", "mdia", "minf", "dinf", "stbl"]
                                    then -- Container box: search inside first, then try next box if needed

                                        let
                                            -- Skip version and flags for FullBox containers
                                            contentOffset = if boxType == "meta" then offset + 12 else offset + 8
                                            boxEnd = offset + fromIntegral boxSize
                                         in
                                            -- First try to find ISPE inside this nested container
                                            case findISPEInContainer bs contentOffset boxEnd (depth - 1) of
                                                Just dims -> Just dims
                                                Nothing ->
                                                    -- If not found in this nested container, continue with next box at same level
                                                    findISPEInContainer bs nextOffset containerEnd (depth - 1)
                                    else -- Not a container box, skip to next box at same level
                                        findISPEInContainer bs nextOffset containerEnd (depth - 1)


-- | Read a big-endian 32-bit unsigned integer
readUInt32BE :: ByteString -> Int -> Word32
readUInt32BE bs offset
    | offset + 3 >= BS.length bs = 0
    | otherwise =
        let b0 = fromIntegral (BS.index bs offset)
            b1 = fromIntegral (BS.index bs (offset + 1))
            b2 = fromIntegral (BS.index bs (offset + 2))
            b3 = fromIntegral (BS.index bs (offset + 3))
         in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
