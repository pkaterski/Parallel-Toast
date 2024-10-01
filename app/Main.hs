module Main (main) where

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Environment (getArgs)
import Control.Monad.IO.Class()
import Control.Monad.Trans.Resource (ResourceT)

data Operation = Add | Subtract | Multiply | Divide
    deriving Show

applyOperation :: Operation -> Double -> Double -> Either String Double
applyOperation op x y = case op of
    Add      -> pure $ x + y
    Subtract -> pure $ x - y
    Multiply -> pure $ x * y
    Divide   -> if y == 0
                then Left "Division by zero"
                else pure $ x / y

operations :: [Operation]
operations = cycle [Add, Subtract, Multiply, Divide]

parseNumber :: MonadFail m => T.Text -> m Double
parseNumber txt = case TR.double txt of
    Right (num, _) -> pure num
    Left err       -> fail $ "Failed to parse number: " ++ T.unpack txt ++ " Error: " ++ err

processNumbers :: ConduitT Double Void (ResourceT IO) Double
processNumbers = do
    mFirstNum <- await
    case mFirstNum of
        Nothing       -> fail "No numbers in input file."
        Just firstNum -> processRest firstNum operations

processRest :: Double -> [Operation] -> ConduitT Double Void (ResourceT IO) Double
processRest _ [] = fail "Ran out of operations."
processRest acc ops = do
    mNextNum <- await
    case mNextNum of
        Nothing  -> pure acc
        Just num -> do
            let (op:ops') = ops
            acc' <- either fail pure $ applyOperation op acc num
            processRest acc' ops'

main :: IO ()
main = do
    args <- getArgs
    case args of
        [_, inputFile, outputFile] -> processFiles inputFile outputFile
        _                          -> putStrLn "Usage: program inputFile outputFile"
    putStrLn "Done."

processFiles :: FilePath -> FilePath -> IO ()
processFiles inputFile outputFile = do
    result <- runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        .| C.concatMap T.words
        .| C.mapM parseNumber
        .| processNumbers
    writeFile outputFile (show result)


