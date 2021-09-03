{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lexer.Support where

import Data.Word
import Data.List (uncons)
import Data.Char (ord)
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Control.Monad.Except

data Token
  = TkIdent String -- identifiers

  -- Keywords
  | TkLet | TkIn | TkWhere

  -- Punctuation
  | TkEqual | TkOpen | TkSemi | TkClose
  | TkLParen | TkRParen
  | TkBackslash | TkArrow

  -- Layout punctuation
  | TkVOpen | TkVSemi | TkVClose
  | TkEOF
  deriving (Eq, Show)

data AlexInput
  = Input { inpLine   :: {-# UNPACK #-} !Int
          , inpColumn :: {-# UNPACK #-} !Int
          , inpLast   :: {-# UNPACK #-} !Char
          , inpStream :: String
          }
  deriving (Eq, Show)

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = inpLast

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@Input{inpStream = str} = advance <$> uncons str where
  advance ('\n', rest) =
    ( fromIntegral (ord '\n')
    , Input { inpLine = inpLine inp + 1
            , inpColumn = 0
            , inpLast = '\n'
            , inpStream = rest }
    )
  advance (c, rest) =
    ( fromIntegral (ord c)
    , Input { inpLine = inpLine inp
            , inpColumn = inpColumn inp + 1
            , inpLast = c
            , inpStream = rest }
    )

newtype Lexer a = Lexer { _getLexer :: StateT LexerState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState LexerState, MonadError String)

data Layout = ExplicitLayout | LayoutColumn Int
  deriving (Eq, Show, Ord)

data LexerState
  = LS { lexerInput      :: {-# UNPACK #-} !AlexInput
       , lexerStartCodes :: {-# UNPACK #-} !(NonEmpty Int)
       , lexerLayout     :: [Layout]
       }
  deriving (Eq, Show)

startCode :: Lexer Int
startCode = gets (NE.head . lexerStartCodes)

pushStartCode :: Int -> Lexer ()
pushStartCode i = modify' $ \st ->
  st { lexerStartCodes = NE.cons i (lexerStartCodes st)
     }

popStartCode :: Lexer ()
popStartCode = modify' $ \st ->
  st { lexerStartCodes =
         case lexerStartCodes st of
           _ :| [] -> 0 :| []
           _ :| (x:xs) -> x :| xs
     }

layout :: Lexer (Maybe Layout)
layout = gets (fmap fst . uncons . lexerLayout)

pushLayout :: Layout -> Lexer ()
pushLayout i = modify' $ \st ->
  st { lexerLayout = i:lexerLayout st }

popLayout :: Lexer ()
popLayout = modify' $ \st ->
  st { lexerLayout =
         case lexerLayout st of
           _:xs -> xs
           [] -> []
     }

initState :: String -> LexerState
initState str = LS { lexerInput      = Input 0 1 '\n' str
                   , lexerStartCodes = 0 :| []
                   , lexerLayout     = []
                   }

emit :: (String -> Token) -> String -> Lexer Token
emit = (pure .)

token :: Token -> String -> Lexer Token
token = const . pure

runLexer :: Lexer a -> String -> Either String a
runLexer act s = fst <$> runStateT (_getLexer act) (initState s) 
