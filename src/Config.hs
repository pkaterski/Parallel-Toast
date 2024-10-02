module Config
    ( readConfigFromFile
    , getBatches
    ) where

import Types
import Helper (chunk)
import Lens.Micro
import qualified Data.Aeson as A


readConfigFromFile :: FilePath -> IO (Maybe Config)
readConfigFromFile filePath = do
    result <- A.eitherDecodeFileStrict filePath
    case result of
        Left err -> do
            putStrLn $ "Error parsing JSON: " ++ err
            pure Nothing
        Right job -> pure (Just job)

getBatches :: Config -> [[Job]]
getBatches conf = chunk (conf ^. numberOfThreads) $ conf ^. jobs

