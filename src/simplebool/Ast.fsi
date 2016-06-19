
/// Syntax trees and associated support functions.
module Ast

type Ty = 
    | TyArr of Ty * Ty 
    | TyBool

type Term =
    | TmVar of Info * int * int
    | TmAbs of Info * string * Ty * Term
    | TmApp of Info * Term * Term
    | TmTrue of Info
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term

type Binding = | NameBind | VarBind of Ty

type Command =
    | Eval of Info * Term 
    | Bind of Info * string * Binding

type Context = (string * Binding) list

val tmInfo : t : Term -> Info

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val termSubstTop : s : Term -> t : Term -> Term

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val printTy : tyT : Ty -> unit

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit
