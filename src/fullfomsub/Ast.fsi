
/// Syntax trees and associated support functions.
module Ast

type Kind = | KnStar | KnArr of Kind * Kind

type Ty =
  | TyVar of int * int
  | TyId of string
  | TyTop
  | TyArr of Ty * Ty
  | TyBool
  | TyRecord of (string * Ty) list
  | TyString
  | TyUnit
  | TyFloat
  | TyAll of string * Ty * Ty
  | TyNat
  | TySome of string * Ty * Ty
  | TyAbs of string * Kind * Ty
  | TyApp of Ty * Ty

type Term =
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term
  | TmRecord of Info * (string * Term) list
  | TmProj of Info * Term * string
  | TmLet of Info * string * Term * Term
  | TmFix of Info * Term
  | TmString of Info * string
  | TmUnit of Info
  | TmAscribe of Info * Term * Ty
  | TmFloat of Info * float
  | TmTimesfloat of Info * Term * Term
  | TmTAbs of Info * string * Ty * Term
  | TmTApp of Info * Term * Ty
  | TmZero of Info
  | TmSucc of Info * Term
  | TmPred of Info * Term
  | TmIsZero of Info * Term
  | TmInert of Info * Ty
  | TmPack of Info * Ty * Term * Ty
  | TmUnpack of Info * string * string * Term * Term

type Binding =
  | NameBind
  | TyVarBind of Ty
  | VarBind of Ty
  | TyAbbBind of Ty * Kind option
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

val index2Name : fi : Info -> ctx : Context -> x : int -> string

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val isNameBound : ctx : Context -> x : string -> bool

val termSubstTop : s : Term -> t : Term -> Term

val makeTop : k : Kind -> Ty

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val typeSubstTop : tyS : Ty -> tyT : Ty -> Ty

val termShift : d : int -> t : Term -> Term

val typeShift : d : int -> tyT : Ty -> Ty

val tyTermSubstTop : tyS : Ty -> t : Term -> Term

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val printTy : ctx : Context -> tyT : Ty -> unit

val printKn : ctx : Context -> knK : Kind -> unit

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit
