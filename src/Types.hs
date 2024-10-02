{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where


import Lens.Micro.TH
import qualified Data.Aeson as A
import GHC.Generics

data Operation = Add | Subtract | Multiply | Divide
    deriving (Show, Generic)

instance A.FromJSON Operation
instance A.ToJSON Operation

type NumberOrErr = Either String Double

data Result = Result
    { _value  :: Maybe Double
    , _errors :: [String]
    } deriving Show

makeLenses ''Result

customOptions :: A.Options
customOptions = A.defaultOptions
    { A.fieldLabelModifier = drop 1
    }

data Job = Job
    { _jobId      :: Int
    , _inFile     :: String
    , _outFile    :: String
    , _operations :: [Operation]
    } deriving (Show, Generic)

makeLenses ''Job

instance A.FromJSON Job where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON Job where
    toJSON = A.genericToJSON customOptions

data Config = Config
    { _jobs            :: [Job]
    , _numberOfThreads :: Int
    , _logFile         :: String
    } deriving (Show, Generic)

makeLenses ''Config

instance A.FromJSON Config where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON Config where
    toJSON = A.genericToJSON customOptions

