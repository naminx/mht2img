{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Control.Lens
import RIO
import RIO.Process
import System.Path


-- | Command line arguments
data CliOptions = CliOptions
  { _verbose :: !Bool
  , _imageTypes :: ![Text]
  , _mhtFiles :: !AbsRelFile
  }


makeFieldsNoPrefix ''CliOptions


data AppEnv = AppEnv
  { _logFunc :: !LogFunc
  , _processContext :: !ProcessContext
  , _cliOptions :: !CliOptions
  -- Add other app-specific configuration information here
  }


makeClassy ''AppEnv


instance HasLogFunc AppEnv where
  logFuncL = logFunc


instance HasProcessContext AppEnv where
  processContextL = processContext
