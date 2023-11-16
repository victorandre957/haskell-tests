module Q4.Interpreter where

import Q4.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]
type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = case execute context stm of
  Left err -> Left err
  Right res -> Right res

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context (SAss id exp) = do
  value <- eval context exp
  Right $ update context (getStr id) value
execute context (SBlock []) = Right context
execute context (SBlock (s : stms)) = do
  res <- execute context s
  execute res (SBlock stms)
execute context (SWhile exp stm) = do
  expValue <- eval context exp
  if expValue /= 0
    then do
      res <- execute context stm
      execute res (SWhile exp stm)
    else Right context
execute context (STry stmsT stmsC stmsF) = case executeTry context stmsT of
  Left _ -> case executeCatch context stmsC of
    Left err -> Left err
    Right res -> executeFinally (update res "soma" 0) stmsF
  Right res -> executeFinally res stmsF

eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context (EAdd exp0 exp) = (+) <$> eval context exp0 <*> eval context exp
eval context (ESub exp0 exp) = (-) <$> eval context exp0 <*> eval context exp
eval context (EMul exp0 exp) = (*) <$> eval context exp0 <*> eval context exp
eval context (EDiv exp0 exp) = do
  expValue <- eval context exp
  if expValue /= 0
    then div <$> eval context exp0 <*> pure expValue
    else Left "Divisão por zero"
eval _ (EInt n) = Right n
eval context (EVar id) = Right (lookup context (getStr id))

executeTry :: RContext -> [Stm] -> Either ErrorMessage RContext
executeTry context [] = Right context
executeTry context (s : stms) = case execute context s of
  Left err -> Left err
  Right res -> executeTry res stms

executeCatch :: RContext -> [Stm] -> Either ErrorMessage RContext
executeCatch context stms = execute context (SBlock stms)

executeFinally :: RContext -> [Stm] -> Either ErrorMessage RContext
executeFinally context [] = Right context
executeFinally context (stm : stms) = case execute context stm of
  Left err -> Left err
  Right res -> executeFinally res stms

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup [] _ = error "Variável não encontrada"
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv