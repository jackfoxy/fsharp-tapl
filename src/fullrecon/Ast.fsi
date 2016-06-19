
/// Syntax trees and associated support functions.
module Ast

type Ty = | TyBool | TyNat | TyArr of Ty * Ty | TyId of string

type Term =
    | TmVar of Info * int * int
    | TmLet of Info * string * Term * Term
    | TmTrue of Info
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term
    | TmZero of Info
    | TmSucc of Info * Term
    | TmPred of Info * Term
    | TmIsZero of Info * Term
    | TmAbs of Info * string * Ty option * Term
    | TmApp of Info * Term * Term

type Binding = 
    | NameBind 
    | VarBind of Ty

type Context = (string * Binding) list

type Command = 
    | Eval of Info * Term 
    | Bind of Info * string * Binding

val tmInfo : t : Term -> Info

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val termSubstTop : s : Term -> t : Term -> Term

val getTypeFromContext : fi : Info -> ctx : Context -> i : int -> Ty

val printTyType : outer : bool -> tyT : Ty -> unit

val  printTy : tyT : Ty -> unit

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

val prBinding : ctx : Context -> b : Binding -> unit
