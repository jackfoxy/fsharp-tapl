
/// Syntax trees and associated support functions.
module Ast

type Kind = | KnStar | KnArr of Kind * Kind

type Ty =
  | TyVar of int * int
  | TyArr of Ty * Ty
  | TyAll of string * Kind * Ty
  | TyAbs of string * Kind * Ty
  | TyApp of Ty * Ty

type Term =
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmTAbs of Info * string * Kind * Term
  | TmTApp of Info * Term * Ty

type Binding = | NameBind | TyVarBind of Kind | VarBind of Ty

type Context = (string * Binding) list

type Command = | Eval of Info * Term | Bind of Info * string * Binding

val tmInfo : t : Term -> Info

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val index2Name : fi : Info -> ctx : Context -> x : int -> string

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val termSubstTop : s : Term -> t : Term -> Term

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val typeSubstTop : tyS : Ty -> tyT : Ty -> Ty

val typeShift : d : int -> tyT : Ty -> Ty

val tyTermSubstTop : tyS : Ty -> t : Term -> Term

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

val printTy : ctx : Context -> tyT : Ty -> unit

val printKind : ctx : Context -> knK : Kind -> unit