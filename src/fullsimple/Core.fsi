﻿/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

/// Evaluate binding
val evalBinding : ctx : Context ->  b : Binding -> Binding

/// Type of term
val typeOf : ctx : Context -> t : Term -> Ty

/// Types are equivalent
val tyEqv : ctx : Context -> tyS : Ty -> tyT : Ty -> bool
