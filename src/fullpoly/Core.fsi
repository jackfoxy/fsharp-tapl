/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : ctx : Context -> Term -> Term 

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

val simplifyTy : ctx : Context -> tyT : Ty -> Ty

val evalBinding : ctx : Context -> b : Binding -> Binding

val tyEqv : ctx : Context -> tyS : Ty -> tyT : Ty -> bool