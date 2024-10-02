{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where


import Lens.Micro.TH
import qualified Data.Aeson as A
import GHC.Generics
import Control.Monad.Reader
import Control.Concurrent.STM

data Operation = Add | Subtract | Multiply | Divide
    deriving (Eq, Show, Generic)

instance A.FromJSON Operation
instance A.ToJSON Operation

type NumberOrErr = Either String Double

data Result = Result
    { _value  :: Maybe Double
    , _errors :: [String]
    } deriving (Eq, Show)

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
    } deriving (Eq, Show, Generic)

makeLenses ''Job

instance A.FromJSON Job where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON Job where
    toJSON = A.genericToJSON customOptions

data Config = Config
    { _jobs            :: [Job]
    , _numberOfThreads :: Int
    , _logFile         :: String
    } deriving (Eq, Show, Generic)

makeLenses ''Config

instance A.FromJSON Config where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON Config where
    toJSON = A.genericToJSON customOptions

data Env = Env
    { envLogVar        :: TVar Log
    , envConfig        :: Config
    , envJobsStarted   :: TVar Int
    , envJobsCompleted :: TVar Int
    , envJobsFailed    :: TVar Int
    }

type Log = [String]
type AppM = ReaderT Env IO

