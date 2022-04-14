module Main where

import HW3.Action
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)

import Control.Monad.Trans
import Data.Set
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    input <- getInputLine "% "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just str ->
        case (parse str) of
          (Left parseError) -> do
            outputStrLn (errorBundlePretty parseError)
            loop
          (Right expr) ->
            do
              eitherVal <- liftIO
                (runHIO (eval expr)
                (fromList [AllowRead, AllowWrite, AllowTime]))
              case eitherVal of
                (Left hiError) -> do
                  outputStrLn (show hiError)
                  loop
                (Right hiValue) -> do
                  outputStrLn $ show (prettyValue hiValue)
                  loop
