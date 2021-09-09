{-# LANGUAGE InstanceSigs #-}

module LambdaParser where

import LambdaDefinition

import Control.Applicative
import Data.Char

newtype Parser tok a =
  Parser { runParser :: [tok] ->  Maybe ([tok],a) }
 
instance Functor (Parser tok) where
  fmap :: (a -> b) -> Parser tok a -> Parser tok b
  fmap g (Parser p) = Parser $ (fmap . fmap . fmap) g p
  
instance Applicative (Parser tok) where
  pure :: a -> Parser tok a
  pure x = Parser $ \s -> Just (s, x)
  (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
  Parser u <*> Parser v = Parser f where
    f xs = case u xs of 
      Nothing       -> Nothing
      Just (xs', g) -> case v xs' of 
        Nothing        -> Nothing
        Just (xs'', x) -> Just (xs'', g x)

instance Alternative (Parser tok) where
  empty :: Parser tok a
  empty = Parser $ \_ -> Nothing
  (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  Parser u <|> Parser v = Parser f where 
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

instance Read Expr where
  readsPrec _ = readExpr

readExpr :: ReadS Expr
readExpr xs = case runParser parse xs of
  Just (s, e) -> [(e, s)]
  Nothing -> []

parse :: Parser Char Expr 
parse = parseLam <|> parseAp <|> parseVar

parseExprPar :: Parser Char Expr
parseExprPar = Parser f where
  f xs = case lex xs of
    [("(", xs')] -> case runParser parse xs' of
      Nothing -> Nothing
      Just (xs'', e) -> case lex xs'' of
        [(")", xs''')] -> Just (xs''', e)
        _              -> Nothing
    _            -> Nothing

parseVar :: Parser Char Expr
parseVar = Parser f where
  f xs = case lex xs of
    [("", _)] -> Nothing
    [("\\", _)] -> Nothing
    [("->", _)] -> Nothing
    [("(", _)] -> Nothing
    [(")", _)] -> Nothing
    [(x, s)] -> Just (s, Var x)

parseAp :: Parser Char Expr
parseAp = Parser f where
  f xs = case runParser (some $ parseExprPar <|> parseVar) xs of
    Nothing -> Nothing
    Just (s, ys) -> Just (s, foldl1 (\xs x -> xs :@ x) ys)

parseLam :: Parser Char Expr
parseLam = Parser f where
  f xs = case lex xs of
    [("\\", xs')] -> case runParser (some parseVar) xs' of
      Nothing -> Nothing
      Just (xs'', vs) -> case lex xs'' of
        [("->", xs''')] -> case runParser parse xs''' of
          Nothing -> Nothing
          Just (xs'''', e) -> Just (xs'''', foldr (\(Var x) xs -> Lam x xs) e vs)
      Nothing         -> Nothing
    _             -> Nothing

instance Show Expr where
  showsPrec _ = showExpr

showExpr :: Expr -> ShowS
showExpr (Var x) = showString x
showExpr (l@(Lam x m) :@ (Var n)) = showString "(" . showExpr l . showString (") " ++ n)
showExpr (l@(Lam x m) :@ n) = showString "(" . showExpr l . showString ") (" . showExpr n . showString ")"
showExpr (m :@ (Var n)) = showExpr m . showString (" " ++ n)
showExpr (m :@ n) = showExpr m . showString " (" . showExpr n . showString ")"
showExpr (Lam x m) = showString ("\\" ++ x ++ " -> ") . showExpr m 
