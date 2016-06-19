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
let rec isval _ t =
  match t with
  | TmAbs (_) -> true
  | TmTAbs (_) -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | TmTApp (_, (TmTAbs (_, _, _, t11)), tyT2) -> tyTermSubstTop tyT2 t11
  | TmTApp (fi, t1, tyT2) -> let t1' = eval1 ctx t1 in TmTApp (fi, t1', tyT2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   KINDING  ------------------------ *)
let rec computety _ tyT =
  match tyT with
  | TyApp ((TyAbs (_, _, tyT12)), tyT2) -> typeSubstTop tyT2 tyT12
  | _ -> raise Common.NoRuleAppliesException
  
let rec simplifyTy ctx tyT =
  let tyT =
    match tyT with
    | TyApp (tyT1, tyT2) -> TyApp (simplifyTy ctx tyT1, tyT2)
    | tyT -> tyT
  in
    try let tyT' = computety ctx tyT in simplifyTy ctx tyT'
    with | Common.NoRuleAppliesException -> tyT
  
let rec tyEqv ctx tyS tyT =
  let tyS = simplifyTy ctx tyS in
  let tyT = simplifyTy ctx tyT
  in
    match (tyS, tyT) with
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
        (tyEqv ctx tyS1 tyT1) && (tyEqv ctx tyS2 tyT2)
    | (TyTop, TyTop) -> true
    | (TyVar (i, _), TyVar (j, _)) -> i = j
    | (TyAll (tyX1, tyS1, tyS2), TyAll (_, tyT1, tyT2)) ->
        let ctx1 = addName ctx tyX1
        in (tyEqv ctx tyS1 tyT1) && (tyEqv ctx1 tyS2 tyT2)
    | (TyAbs (tyX1, knKS1, tyS2), TyAbs (_, knKT1, tyT2)) ->
        (knKS1 = knKT1) &&
          (let ctx = addName ctx tyX1 in tyEqv ctx tyS2 tyT2)
    | (TyApp (tyS1, tyS2), TyApp (tyT1, tyT2)) ->
        (tyEqv ctx tyS1 tyT1) && (tyEqv ctx tyS2 tyT2)
    | _ -> false
  
let rec getkind fi ctx i =
  match getBinding fi ctx i with
  | TyVarBind tyT -> kindOf ctx tyT
  | _ ->
      error fi
        ("getkind: Wrong kind of binding for variable " +
           (index2Name fi ctx i))
and kindOf ctx tyT =
  match tyT with
  | TyVar (i, _) -> let knK = getkind dummyinfo ctx i in knK
  | TyAll (tyX, tyT1, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind tyT1)
      in
        (if (kindOf ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | TyAbs (tyX, knK1, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind (makeTop knK1)) in
      let knK2 = kindOf ctx' tyT2 in KnArr (knK1, knK2)
  | TyApp (tyT1, tyT2) ->
      let knK1 = kindOf ctx tyT1 in
      let knK2 = kindOf ctx tyT2
      in
        (match knK1 with
         | KnArr (knK11, knK12) ->
             if knK2 = knK11
             then knK12
             else error dummyinfo "parameter kind mismatch"
         | _ -> error dummyinfo "arrow kind expected")
  | TyArr (tyT1, tyT2) ->
      (if (kindOf ctx tyT1) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       if (kindOf ctx tyT2) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | _ -> KnStar
  
let checkkindstar fi ctx tyT =
  let k = kindOf ctx tyT
  in if k = KnStar then () else error fi "Kind * expected"
  
(* ------------------------   SUBTYPING  ------------------------ *)
let rec promote ctx t =
  match t with
  | TyVar (i, _) ->
      (match getBinding dummyinfo ctx i with
       | TyVarBind tyT -> tyT
       | _ -> raise Common.NoRuleAppliesException)
  | TyApp (tyS, tyT) -> TyApp (promote ctx tyS, tyT)
  | _ -> raise Common.NoRuleAppliesException
  
let rec subType ctx tyS tyT =
  (tyEqv ctx tyS tyT) ||
    (let tyS = simplifyTy ctx tyS in
     let tyT = simplifyTy ctx tyT
     in
       match (tyS, tyT) with
       | (_, TyTop) -> true
       | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
           (subType ctx tyT1 tyS1) && (subType ctx tyS2 tyT2)
       | (TyVar (_), _) -> subType ctx (promote ctx tyS) tyT
       | (TyAll (tyX1, tyS1, tyS2), TyAll (_, tyT1, tyT2)) ->
           ((subType ctx tyS1 tyT1) && (subType ctx tyT1 tyS1)) &&
             (let ctx1 = addBinding ctx tyX1 (TyVarBind tyT1)
              in subType ctx1 tyS2 tyT2)
       | (TyAbs (tyX, knKS1, tyS2), TyAbs (_, knKT1, tyT2)) ->
           (knKS1 = knKT1) &&
             (let ctx = addBinding ctx tyX (TyVarBind (makeTop knKS1))
              in subType ctx tyS2 tyT2)
       | (TyApp (_), _) -> subType ctx (promote ctx tyS) tyT
       | (_) -> false)
  
(* ------------------------   TYPING  ------------------------ *)
let rec lcst ctx tyS =
  let tyS = simplifyTy ctx tyS
  in try lcst ctx (promote ctx tyS) with | Common.NoRuleAppliesException -> tyS
  
let rec typeOf ctx t =
  match t with
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (fi, x, tyT1, t2) ->
      (checkkindstar fi ctx tyT1;
       let ctx' = addBinding ctx x (VarBind tyT1) in
       let tyT2 = typeOf ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2))
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
  

