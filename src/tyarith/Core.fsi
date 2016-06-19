/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : t : Term -> Term

/// Type of term
val typeOf : t : Term -> Ty
