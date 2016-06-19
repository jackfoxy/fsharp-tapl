/// Core evaluation 
module Core

open Ast

type Store = Term list

/// Evaluation
val eval : ctx : Context -> store : Store -> t : Term -> Term * Store

val emptyStore : Store

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

val evalBinding : ctx : Context -> store : Store -> b : Binding -> Binding * Store

val shiftStore : i : int -> store : Store -> Store

val kindOf : ctx : Context -> tyT : Ty -> Kind

val subType : ctx : Context -> tyS : Ty -> tyT : Ty -> bool

val lcst : ctx : Context -> tyS : Ty -> Ty
