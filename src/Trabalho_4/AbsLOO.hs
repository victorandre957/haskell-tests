-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language LOO2.

module AbsLOO where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Prog [ClassDeclaration]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ClassDeclaration = ClassD Ident Extends [MemberDeclaration]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Extends = ExtId Ident | ExtObject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MemberDeclaration = Attr Decl | Mth Type Ident [Decl] [Stm]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl = Dec Type Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stm
    = SDec Decl
    | SConstInit Decl Exp
    | SExp Exp
    | SAss Ident Exp
    | SBlock [Stm]
    | SWhile Exp Stm
    | SReturn Exp
    | SIf Exp Stm Stm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EOr Exp Exp
    | EAnd Exp Exp
    | ENot Exp
    | ECon Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EMthCall Ident Ident [Exp]
    | ECast Ident Exp
    | ENew Ident
    | EInt Integer
    | EVar Ident
    | EStr String
    | ETrue
    | EFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = Tbool | Tint | Tvoid | TStr | TClass Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

