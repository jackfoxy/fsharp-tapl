/// Core evaluation 
module Core

open Ast

type Store = Term list

/// Evaluation
val eval : ctx : Context -> store : Term list -> t : Term -> Term * Term list

/// Evaluate binding
val evalBinding : ctx : Context -> store : Term list ->  b : Binding -> Binding * Term list

val emptyStore : Term list

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

val subType : ctx : Context -> tyS : Ty -> tyT : Ty -> bool

val shiftStore : i : int -> store : Store -> Store

