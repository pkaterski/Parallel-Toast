{-# LANGUAGE OverloadedStrings #-}

module Streaming 
    ( processNumbers
    , processRest
    , parseWords
    , parseNumber
    )where

import Types
import Helper (popLast, applyOperation)
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Control.Monad.IO.Class()
import Control.Monad.Trans.Resource (ResourceT)
import Lens.Micro

processNumbers :: [Operation] -> ConduitT NumberOrErr Void (ResourceT IO) Result
processNumbers ops = do
    mFirstNum <- await
    case mFirstNum of
        Nothing          -> pure $ Result Nothing ["No numbers in input file."]
        Just (Right num) -> processRest (Result (Just num) []) ops
        Just (Left err)  -> processRest (Result Nothing [err]) ops

processRest :: Result -> [Operation] -> ConduitT NumberOrErr Void (ResourceT IO) Result
processRest acc [] = pure acc
processRest acc ops = do
    mNextNum <- await
    case mNextNum of
        Nothing  -> pure acc
        Just (Right num) -> do
            case (acc ^. value) of
                Just curr -> do
                    let (op:ops') = ops
                    case applyOperation op curr num of
                        Left e    -> pure $ acc & errors %~ (e:)
                        Right val -> processRest (acc & value .~ Just val ) ops'
                Nothing -> processRest (acc & value .~ Just num) ops
        Just (Left err) -> processRest (acc & errors %~ (err:)) ops

parseNumber :: Monad m => ConduitT T.Text NumberOrErr m ()
parseNumber = do
    mTxt <- await
    case mTxt of
        Just txt -> case TR.double txt of
            Right (num, "") -> do
                yield $ Right num
                -- continue the processing
                parseNumber
            Right (num, l) -> do
                yield $ Right num
                yield $ Left $ "Failed to parse number: " <> T.unpack txt <> " Unparsed Leftover: " <> T.unpack l
                -- stop the processing
                pure ()
            Left err -> do
                yield $ Left $ "Failed to parse number: " <> T.unpack txt <> " Error: " <> err
                -- stop the processing
                pure ()
        Nothing -> pure ()

-- this parsing handles leftover from the previous stream
parseWords :: Monad m => ConduitT T.Text T.Text m ()
parseWords = go Nothing
  where
    go leftoverStr = do
        str <- await
        case str of
            Nothing -> yieldLeftover leftoverStr
            Just s  -> handleNextChunck leftoverStr s

    mergeLeftover Nothing s  = s
    mergeLeftover (Just l) s = l <> s

    yieldLeftover Nothing  = pure ()
    yieldLeftover (Just l) = yield l

    handleNextChunck leftoverStr s = do
        let (ws, l') = popLast $ T.words $ mergeLeftover leftoverStr s
        C.yieldMany ws
        if T.last s == '\n'
            then do
                yieldLeftover l'
                go Nothing
            else go l'

