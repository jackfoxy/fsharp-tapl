/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

/// Evaluate binding
val evalBinding : ctx : Context ->  b : Binding -> Binding

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

val kindOf : ctx : Context -> tyT : Ty -> Kind

val subType : ctx : Context -> tyS : Ty -> tyT : Ty -> bool

val lcst : ctx : Context -> tyS : Ty -> Ty
