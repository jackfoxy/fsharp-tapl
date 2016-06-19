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
open FSharpTapl

(* ------------------------   EVALUATION  ------------------------ *)
let rec isval _ t = match t with | TmAbs (_) -> true | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
let rec computety _ tyT =
  match tyT with
  | (TyRec (_, tyS1) as tyS) -> typeSubstTop tyS tyS1
  | _ -> raise Common.NoRuleAppliesException
  
let rec simplifyTy ctx tyT =
  try let tyT' = computety ctx tyT in simplifyTy ctx tyT'
  with | Common.NoRuleAppliesException -> tyT
  
let rec private tyEqv' seen ctx tyS tyT =
  (List.exists (fun x -> x = (tyS, tyT)) seen) ||
    (match (tyS, tyT) with
     | (TyRec (_, tyS1), _) ->
         tyEqv' ((tyS, tyT) :: seen) ctx (typeSubstTop tyS tyS1) tyT
     | (_, TyRec (_, tyT1)) ->
         tyEqv' ((tyS, tyT) :: seen) ctx tyS (typeSubstTop tyT tyT1)
     | (TyId b1, TyId b2) -> b1 = b2
     | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
         (tyEqv' seen ctx tyS1 tyT1) && (tyEqv' seen ctx tyS2 tyT2)
     | _ -> false)
  
let tyEqv (ctx : Context) tyS tyT = tyEqv' [] ctx tyS tyT
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeOf (ctx : Context) t =
  match t with
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addBinding ctx x (VarBind tyT1) in
      let tyT2 = typeOf ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let tyT2 = typeOf ctx t2
      in
        (match simplifyTy ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if tyEqv ctx tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | _ -> error fi "arrow type expected")
  
