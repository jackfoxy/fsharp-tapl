/// Core evaluation 
module Core

open Ast

val eval : ctx : Context -> t : Term -> Term

val typeOf : ctx : Context -> t : Term -> Ty
