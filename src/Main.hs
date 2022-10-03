{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Control.Lens
import Data.MIME
import Import
import Options.Applicative.Simple hiding (header)
import qualified Paths_mht2img
import qualified RIO.ByteString as BS
import RIO.Process
import Run


main :: IO ()
main = do
    (options, ()) <-
        simpleOptions
            $(simpleVersion Paths_mht2img.version)
            "Header for command line arguments "
            "Program description, also for command line arguments"
            ( CliOptions
                <$> switch
                    ( long "verbose"
                        <> short 'v'
                        <> help "Verbose output?"
                    )
                <*> some
                    ( strArgument
                        ( metavar "FILES ..."
                            <> help "Input file(s)"
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
