{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where


import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit
import Lexer.Support (runLexer)
import Parser (parseDecl)
import Syntax
import Interpret 


test2 ::  TestTree
test2 = testCase  "simple parse" $ do
  actual <- runLexer parseDecl <$> readFile "./test/data/that-code-from-before"
  let expect = Right (Decl {declName = "foo",
    declRhs = Let [Decl {declName = "x", declRhs = Let [Decl {declName = "y", declRhs = Var "z", declWhere = Nothing}] (Var "y"), declWhere = Nothing}]
    (Var "x"), declWhere = Nothing})
  case actual of
    Left err -> assertFailure err
    Right decl ->  do
      print $ decl
      print $ declToE $ decl
      print $ declRhsE $ declToE $ decl
      showSteps $ declRhsE $ declToE $ decl
      -- print $ smallStep (declRhsE $ declToE decl) >>= smallStep
      -- print $ smallStep (declRhsE $ declToE decl) >>= smallStep >>= smallStep
      assertEqual "error" (NeedEval (VarE "z")) $ smallStep (declRhsE $ declToE decl) >>= smallStep >>= smallStep
  assertFailure "fail"
  assertEqual "simple parse equal" expect actual


main :: IO ()
main = defaultMain test2
tests :: TestTree
tests = testGroup "Server client test" [
  -- test2
  -- testSingleClient
  -- , testMultipleClients
  -- , testClientDrop
  ]

