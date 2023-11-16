module Q3.Interpreter where

import Q3.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]
type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp -> do
    value <- eval context exp
    Right (update context (getStr id) value)
  SBlock [] -> Right context
  SBlock (s : stms) -> case execute context s of
    Left err -> Left err
    Right newContext -> execute newContext (SBlock stms)
  SWhile exp stm -> case eval context exp of
    Left err -> Left err
    Right cond -> if cond /= 0
      then case execute context stm of
        Left err -> Left err
        Right newContext -> execute newContext (SWhile exp stm)
      else Right context

eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
  EAdd exp0 exp -> (+) <$> eval context exp0 <*> eval context exp
  ESub exp0 exp -> (-) <$> eval context exp0 <*> eval context exp
  EMul exp0 exp -> (*) <$> eval context exp0 <*> eval context exp
  EDiv exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    if ve2 == 0
      then Left "divisao por 0"
      else Right (ve1 `div` ve2)
  EInt n -> Right n
  EVar id -> Right (lookup context (getStr id))

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv
