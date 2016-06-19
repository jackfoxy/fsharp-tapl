
/// Syntax trees and associated support functions.
module Ast

type Term =
    | TmTrue of Info
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term
    | TmZero of Info
    | TmSucc of Info * Term
    | TmPred of Info * Term
    | TmIsZero of Info * Term

type Ty = 
    | TyBool 
    | TyNat

type Command =
    | Eval of Info * Term

val tmInfo : t : Term -> Info

val printTy : tyT : Ty -> unit

val printTerm : outer : bool -> t : Term -> unit
