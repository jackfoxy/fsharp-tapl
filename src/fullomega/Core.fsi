/// Core evaluation 
module Core

open Ast

type Store = Term list

/// Evaluation
val eval : ctx : Context -> store : Store -> Term -> Term * Store

val emptyStore : Store

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

val simplifyTy : ctx : Context -> tyT : Ty -> Ty

val shiftStore : i : int -> store : Store -> Store

val kindOf : ctx : Context -> tyT : Ty -> Kind

val evalBinding : ctx : Context ->  store : Store -> b : Binding -> Binding * Store

val tyEqv : ctx : Context -> tyS : Ty -> tyT : Ty -> bool