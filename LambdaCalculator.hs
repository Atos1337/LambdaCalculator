module LambdaCalculator where

import LambdaDefinition

import Data.List
import Data.Maybe

freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (m :@ n) = unionBy (==) (freeVars m) (freeVars n)
freeVars (Lam x m) = delete x (freeVars m)

getNewSymb :: [Symb] -> Symb -> Symb
getNewSymb xs x | elem x xs = getNewSymb xs (x ++ "'")
                | otherwise = x

-- вместо v - n в M
subst :: Symb -> Expr -> Expr -> Expr 
subst v n (Var x) | v == x    = n
                  | otherwise = Var x
subst v n (m :@ n') = (subst v n m) :@ (subst v n n')
subst v n (Lam x m) | v == x    = Lam x m
                    | elem x (freeVars n) = Lam z (subst v n (subst x (Var z) m))
                    | otherwise = Lam x (subst v n m)
                    where z = getNewSymb (unionBy (==) (freeVars n) (freeVars m)) x


infix 1 `alphaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (m :@ n) (m' :@ n') = (alphaEq m m') && (alphaEq n n')
alphaEq (Lam x m) (Lam y n) = alphaEq (subst x (Var z) m) (subst y (Var z) n)
                              where z = getNewSymb (unionBy (==) (freeVars m) (freeVars n)) x
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Lam x m)         = fmap (Lam x) (reduceOnce m)
reduceOnce (Var x)           = Nothing
reduceOnce ((Lam x m) :@ n)  = Just (subst x n m)
reduceOnce (m :@ n)          = case reduceOnce m of
                               Just y  -> Just (y :@ n)
                               Nothing -> fmap (m :@) (reduceOnce n)

nf :: Expr -> Expr 
nf x = if z == Nothing then x else nf (fromJust z)
  where z = reduceOnce x

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool 
betaEq m n = alphaEq (nf m) (nf n)
