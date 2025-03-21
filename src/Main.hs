{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Lens
import Data.Attoparsec.ByteString hiding (option)
import Data.MIME hiding (value)
import Data.String.Conversions
import Data.Version
import Import
import Options.Applicative.Simple hiding (header)
import qualified RIO.ByteString as BS
import RIO.Process
import Text.Read (readMaybe)
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import Run
import System.Path (file)


main :: IO ()
main = do
    (options, ()) <-
        simpleOptions
            $(simpleVersion $ makeVersion [0, 1, 0, 0])
            "Header for command line arguments "
            "Program description, also for command line arguments"
            ( CliOptions
                <$> switch
                    ( long "verbose"
                        <> short 'v'
                        <> help "Verbose output?"
                    )
                <*> ( T.split (== ',')
                        . T.toLower
                        <$> strOption
                            ( short 'f'
                                <> long "format"
                                <> metavar "jpeg,..."
                                <> help "Extract only images in the specified formats"
                                <> value "avif,ico,jpeg,png,webp"
                                <> showDefault
                            )
                    )
                <*> option auto
                    ( short 'w' 
                        <> long "min-width"
                        <> metavar "WIDTH" 
                        <> help "Extract only images wider than WIDTH"
                        <> value 600
                        <> showDefault
                    )
                <*> option auto
                    ( short 'h'  
                        <> long "min-height"
                        <> metavar "HEIGHT" 
                        <> help "Extract only images higher than HEIGHT"
                        <> value 848
                        <> showDefault
                    )
                <*> ( file
                        <$> strArgument
                            ( metavar "FILE.mht"
                                <> help "MHT file"
                            )
                    )
            )
            empty
    lo <- logOptionsHandle stderr (options ^. verbose)
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
        let env =
                AppEnv
                    { _logFunc = lf
                    , _processContext = pc
                    , _cliOptions = options
                    }
         in runRIO env run
