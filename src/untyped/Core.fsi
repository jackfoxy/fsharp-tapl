/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : ctx : Context -> t : Term -> Term
