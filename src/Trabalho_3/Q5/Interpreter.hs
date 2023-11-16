module Q5.Interpreter where

import Q5.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Valor)]
type ErrorMessage = String

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool
  deriving (Eq, Show)

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

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
  if expValue /= ValorInt 0
    then do
      res <- execute context stm
      execute res (SWhile exp stm)
    else Right context
execute context (STry stmsT stmsC stmsF) = case executeTry context stmsT of
  Left _ -> case executeCatch context stmsC of
    Left err -> Left err
    Right res -> executeFinally (update res "soma" (ValorInt 0)) stmsF
  Right res -> executeFinally res stmsF

eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context (EAdd exp0 exp) = do
  val1 <- eval context exp0
  val2 <- eval context exp
  case (val1, val2) of
    (ValorInt i1, ValorInt i2) -> Right (ValorInt (i1 + i2))
    (ValorStr s1, ValorStr s2) -> Right (ValorStr (s1 ++ s2))
    _ -> Left "Operação inválida"
eval context (ESub exp0 exp) = do
  val1 <- eval context exp0
  val2 <- eval context exp
  case (val1, val2) of
    (ValorInt i1, ValorInt i2) -> Right (ValorInt (i1 - i2))
    _ -> Left "Operação inválida"
eval context (EMul exp0 exp) = do
  val1 <- eval context exp0
  val2 <- eval context exp
  case (val1, val2) of
    (ValorInt i1, ValorInt i2) -> Right (ValorInt (i1 * i2))
    _ -> Left "Operação inválida"
eval context (EDiv exp0 exp) = do
  val1 <- eval context exp0
  val2 <- eval context exp
  case (val1, val2) of
    (ValorInt i1, ValorInt i2) -> if i2 == 0
      then Left "Divisão por zero"
      else Right (ValorInt (i1 `div` i2))
    _ -> Left "Operação inválida"
eval _ (EInt n) = Right (ValorInt n)
eval _ (EStr s) = Right (ValorStr s)
eval context (EVar id) = case lookup context (getStr id) of
  Just val -> Right val
  Nothing -> Left $ "Undefined variable: " ++ getStr id

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

lookup :: RContext -> String -> Maybe Valor
lookup [] _ = Nothing
lookup ((i, v) : cs) s
  | i == s = Just v
  | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv