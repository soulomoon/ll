module Main where

import Lexer.Support
import Lexer
import Parser
import Debug.Trace (traceM)
import Control.Monad.RWS

main :: IO ()
main = do
  putStrLn "hello world"
  x <- runLexer parseDecl <$> readFile "that-code-from-before"
  print x
  

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case tok of
    TkEOF -> pure ()
    x -> do
      traceM (show x)
      lexAll