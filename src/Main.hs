module Main where

import Lexer.Support
import Lexer
import Debug.Trace (traceM)
import Control.Monad.RWS

main :: IO ()
main = do
  putStrLn "hello world"

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case tok of
    TkEOF -> pure ()
    x -> do
      traceM (show x)
      lexAll