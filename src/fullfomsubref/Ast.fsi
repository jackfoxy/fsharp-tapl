
/// Syntax trees and associated support functions.
module Ast

type Kind = | KnStar | KnArr of Kind * Kind

type Ty =
  | TyId of string
  | TyVar of int * int
  | TyBool
  | TyTop
  | TyBot
  | TyArr of Ty * Ty
  | TyRecord of (string * Ty) list
  | TyVariant of (string * Ty) list
  | TyRef of Ty
  | TyFloat
  | TyString
  | TyUnit
  | TyAll of string * Ty * Ty
  | TyNat
  | TySome of string * Ty * Ty
  | TySource of Ty
  | TySink of Ty
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
  | TmFloat of Info * float
  | TmTimesfloat of Info * Term * Term
  | TmAscribe of Info * Term * Ty
  | TmString of Info * string
  | TmUnit of Info
  | TmFix of Info * Term
  | TmCase of Info * Term * (string * (string * Term)) list
  | TmTag of Info * string * Term * Ty
  | TmLoc of Info * int
  | TmRef of Info * Term
  | TmDeref of Info * Term
  | TmAssign of Info * Term * Term
  | TmError of Info
  | TmTry of Info * Term * Term
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
  | Import of string
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

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val typeSubstTop : tyS : Ty -> tyT : Ty -> Ty

val typeShift : d : int -> tyT : Ty -> Ty

val makeTop : k : Kind -> Ty

val tyTermSubstTop : tyS : Ty -> t : Term -> Term

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val termShift : d : int -> t : Term -> Term

val printTy : ctx : Context -> tyT : Ty -> unit

val printKn : ctx : Context -> knK : Kind -> unit

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit
