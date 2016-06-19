/// Core evaluation 
module Core

open Ast

type Constr = (Ty * Ty) list

type NextUVar =
  | NextUVar of string * UvarGenerator

and UvarGenerator = unit -> NextUVar

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

val combineConstr : (Constr -> Constr -> Constr)

val applySubst : constr : (Ty * Ty) list -> tyT : Ty -> Ty

val recon : ctx : Context -> nextuvar : (unit -> NextUVar) -> t : Term -> Ty * (unit -> NextUVar) * Constr

val uvargen : (unit -> NextUVar)

val emptyConstr : Constr

val unify : fi : Info -> ctx : Context -> msg : string -> constr : Constr -> Constr