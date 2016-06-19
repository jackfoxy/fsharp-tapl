/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

val evalBinding : ctx : Context -> b : Binding -> Binding

val tyEqv : ctx : Context -> tyS : Ty -> tyT : Ty -> bool
