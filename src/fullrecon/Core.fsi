/// Core evaluation 
module Core

open Ast

type NextUvar =
  | NextUVar of string * UvarGenerator

and UvarGenerator = unit -> NextUvar

val emptyConstr : (Ty * Ty) list

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

val recon : ctx : Context -> nextUvar : (unit -> NextUvar) -> t  : Term -> Ty * (unit -> NextUvar) * (Ty * Ty) list

/// Unification
val unify : fi : Info -> ctx : Context -> msg : string -> constr : (Ty * Ty) list -> (Ty * Ty) list

val combineConstr : (Ty * Ty) list -> (Ty * Ty) list -> (Ty * Ty) list

val applySubst : constr : (Ty * Ty) list -> tyT : Ty -> Ty

val uvargen : (unit -> NextUvar)
