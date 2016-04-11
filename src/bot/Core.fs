(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Core typechecking and evaluation functions.
module Core

open Ast
open TaplCommon
(* ------------------------   EVALUATION  ------------------------ *)
let rec isval _ t =
    match t with
    | TmAbs (_) -> true
    | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2
      TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) ->
    let t1' = eval1 ctx t1
    TmApp (fi, t1', t2)
  | _ ->
    raise Common.NoRuleAppliesException
  
let rec eval ctx t =
    try let t' = eval1 ctx t
        eval ctx t'
    with Common.NoRuleAppliesException -> t
  
(* ------------------------   SUBTYPING  ------------------------ *)
let rec subtype tyS tyT =
  (tyS = tyT) ||
    (match (tyS, tyT) with
     | (_, TyTop) -> true
     | (TyBot, _) -> true
     | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
         (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
     | (_) -> false)
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeof ctx t =
  match t with
  | TmVar (fi, i, _) ->
    getTypeFromContext fi ctx i
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in TyArr (tyT1, tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2
      in
        (match tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subtype tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | TyBot -> TyBot
         | _ -> error fi "arrow type expected")

