module Config
    ( readConfigFromFile
    , getBatches
    ) where

import Types
import Helper (chunk)
import Lens.Micro
import qualified Data.Aeson as A


-- | Read a config from file path
readConfigFromFile :: FilePath -> IO (Maybe Config)
readConfigFromFile filePath = do
    result <- A.eitherDecodeFileStrict filePath
    case result of
        Left err -> do
            putStrLn $ "Error parsing JSON: " ++ err
            pure Nothing
        Right job -> pure (Just job)

-- | Split's up the jobs provided in the config into an array of batches
-- | the size of each batch is defined by the number of threads in the config
getBatches :: Config -> [[Job]]
getBatches conf = chunk (conf ^. numberOfThreads) $ conf ^. jobs

