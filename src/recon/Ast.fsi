
/// Syntax trees and associated support functions.
module Ast

type Ty = 
    | TyBool 
    | TyNat 
    | TyArr of Ty * Ty 
    | TyId of string

type Term =
    | TmTrue of Info
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term
    | TmZero of Info
    | TmSucc of Info * Term
    | TmPred of Info * Term
    | TmIsZero of Info * Term
    | TmVar of Info * int * int
    | TmAbs of Info * string * Ty * Term
    | TmApp of Info * Term * Term

type Binding = 
    | NameBind 
    | VarBind of Ty

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

//val typeSubstTop : tyS : Ty -> tyT : Ty -> Ty
//
//val typeShift : d : int -> tyT : Ty -> Ty
//
//val tyTermSubstTop : tyS : Ty -> t : Term -> Term
//
//val getBinding : fi : Info -> ctx : Context -> i : int -> Binding
//
val printTy : tyT : Ty -> unit

val printTyType : outer : bool -> tyT : Ty -> unit

val printTerm : outer : bool ->  ctx : Context -> t : Term -> unit

val prBinding : ctx : Context -> b : Binding -> unit
