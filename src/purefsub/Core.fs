﻿(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Core typechecking and evaluation functions.
module Core

open Ast
open FSharpTapl

(* ------------------------   EVALUATION  ------------------------ *)
let rec isval _ t =
  match t with
  | TmTAbs (_) -> true
  | TmAbs (_) -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmTApp (_, (TmTAbs (_, _, _, t11)), tyT2) -> tyTermSubstTop tyT2 t11
  | TmTApp (fi, t1, tyT2) -> let t1' = eval1 ctx t1 in TmTApp (fi, t1', tyT2)
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   SUBTYPING  ------------------------ *)
let promote ctx t =
  match t with
  | TyVar (i, _) ->
      (match getBinding dummyinfo ctx i with
       | TyVarBind tyT -> tyT
       | _ -> raise Common.NoRuleAppliesException)
  | _ -> raise Common.NoRuleAppliesException
  
let rec subType ctx tyS tyT =
  (tyS = tyT) ||
    (match (tyS, tyT) with
     | (TyVar (_), _) -> subType ctx (promote ctx tyS) tyT
     | (TyAll (tyX1, tyS1, tyS2), TyAll (_, tyT1, tyT2)) ->
         ((subType ctx tyS1 tyT1) && (subType ctx tyT1 tyS1)) &&
           (let ctx1 = addBinding ctx tyX1 (TyVarBind tyT1)
            in subType ctx1 tyS2 tyT2)
     | (_, TyTop) -> true
     | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
         (subType ctx tyT1 tyS1) && (subType ctx tyS2 tyT2)
     | (_) -> false)
  
(* ------------------------   TYPING  ------------------------ *)
let rec lcst ctx tyS =
  try lcst ctx (promote ctx tyS) with | Common.NoRuleAppliesException -> tyS
  
let rec typeOf ctx t =
  match t with
  | TmTAbs (_, tyX, tyT1, t2) ->
      let ctx = addBinding ctx tyX (TyVarBind tyT1) in
      let tyT2 = typeOf ctx t2 in TyAll (tyX, tyT1, tyT2)
  | TmTApp (fi, t1, tyT2) ->
      let tyT1 = typeOf ctx t1
      in
        (match lcst ctx tyT1 with
         | TyAll (_, tyT11, tyT12) ->
             (if not (subType ctx tyT2 tyT11)
              then error fi "type parameter type mismatch"
              else ();
              typeSubstTop tyT2 tyT12)
         | _ -> error fi "universal type expected")
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addBinding ctx x (VarBind tyT1) in
      let tyT2 = typeOf ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let tyT2 = typeOf ctx t2
      in
        (match lcst ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subType ctx tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | _ -> error fi "arrow type expected")
  

