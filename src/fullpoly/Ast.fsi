
/// Syntax trees and associated support functions.
module Ast

type Ty =
  | TyVar of int * int
  | TyId of string
  | TyArr of Ty * Ty
  | TyString
  | TyUnit
  | TyRecord of (string * Ty) list
  | TyBool
  | TyFloat
  | TyNat
  | TySome of string * Ty
  | TyAll of string * Ty

type Term =
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmLet of Info * string * Term * Term
  | TmFix of Info * Term
  | TmString of Info * string
  | TmUnit of Info
  | TmAscribe of Info * Term * Ty
  | TmRecord of Info * (string * Term) list
  | TmProj of Info * Term * string
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term
  | TmFloat of Info * float
  | TmTimesfloat of Info * Term * Term
  | TmZero of Info
  | TmSucc of Info * Term
  | TmPred of Info * Term
  | TmIsZero of Info * Term
  | TmInert of Info * Ty
  | TmPack of Info * Ty * Term * Ty
  | TmUnpack of Info * string * string * Term * Term
  | TmTAbs of Info * string * Term
  | TmTApp of Info * Term * Ty

type Binding =
  | NameBind
  | TyVarBind
  | VarBind of Ty
  | TyAbbBind of Ty
  | TmAbbBind of Term * Ty option

type Context = (string * Binding) list

type Command =
  | Eval of Info * Term
  | Bind of Info * string * Binding
  | SomeBind of Info * string * string * Term

val tmInfo : t : Term -> Info

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val isName : ctx : Context -> x : string -> bool

val termSubstTop : s : Term -> t : Term -> Term

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val typeSubstTop : tyS : Ty -> tyT : Ty -> Ty

val termShift : d : int -> t : Term -> Term

val typeShift : d : int -> tyT : Ty -> Ty

val tyTermSubstTop : tyS : Ty -> t : Term -> Term

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val printTy : ctx : Context -> tyT : Ty -> unit

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit
