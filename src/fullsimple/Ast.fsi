
/// Syntax trees and associated support functions.
module Ast

type Ty =
  | TyVar of int * int
  | TyId of string
  | TyArr of Ty * Ty
  | TyUnit
  | TyRecord of (string * Ty) list
  | TyVariant of (string * Ty) list
  | TyBool
  | TyString
  | TyFloat
  | TyNat

type Term =
    | TmTrue of Info
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term
    | TmCase of Info * Term * (string * (string * Term)) list
    | TmTag of Info * string * Term * Ty
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
    | TmFloat of Info * float
    | TmTimesfloat of Info * Term * Term
    | TmZero of Info
    | TmSucc of Info * Term
    | TmPred of Info * Term
    | TmIsZero of Info * Term
    | TmInert of Info * Ty

type Binding =
  | NameBind
  | TyVarBind
  | VarBind of Ty
  | TmAbbBind of Term * Ty option
  | TyAbbBind of Ty

type Context = (string * Binding) list

type Command = | Eval of Info * Term | Bind of Info * string * Binding

val tmInfo : t : Term -> Info

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val isNameBound : ctx : Context -> x : string -> bool

val termSubstTop : s : Term -> t : Term -> Term

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val typeShift : d : int -> tyT : Ty -> Ty

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val printTy : ctx : Context -> tyT : Ty -> unit

val printTerm : outer  : bool -> ctx : Context -> t : Term -> unit
