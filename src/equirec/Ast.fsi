
/// Syntax trees and associated support functions.
module Ast

type Ty =
  | TyId of string
  | TyVar of int * int
  | TyArr of Ty * Ty
  | TyRec of string * Ty

type Term =
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term

type Binding = | NameBind | VarBind of Ty | TyVarBind

type Context = (string * Binding) list

type Command = | Eval of Info * Term | Bind of Info * string * Binding

val tmInfo : t : Term -> Info

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val isNameBound : ctx : Context -> x : string -> bool

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val termSubstTop : s : Term -> t : Term -> Term

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val typeSubstTop : tyS : Ty -> tyT : Ty -> Ty

val typeShift : d : int -> tyT : Ty -> Ty

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

val printTy : ctx : Context -> tyT : Ty -> unit
