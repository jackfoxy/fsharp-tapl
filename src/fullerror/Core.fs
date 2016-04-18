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
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmAbs (_) -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmIf (_, (TmTrue _), t2,_) -> t2
  | TmIf (_, (TmFalse _), _, t3) -> t3
  | TmVar (fi, n, _) ->
      (match getbinding fi ctx n with
       | TmAbbBind (t, _) -> t
       | _ -> raise Common.NoRuleAppliesException)
  | TmApp (_, (TmError fi), _) -> TmError fi
  | TmApp (_, v1, (TmError fi)) when isval ctx v1 -> TmError fi
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | TmIf (_, (TmError fi), _, _) -> TmError fi
  | TmIf (fi, t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (fi, t1', t2, t3)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   SUBTYPING  ------------------------ *)
let evalbinding ctx b =
  match b with
  | TmAbbBind (t, tyT) -> let t' = eval ctx t in TmAbbBind (t', tyT)
  | bind -> bind
  
let istyabb ctx i =
  match getbinding dummyinfo ctx i with | TyAbbBind _ -> true | _ -> false
  
let gettyabb ctx i =
  match getbinding dummyinfo ctx i with
  | TyAbbBind tyT -> tyT
  | _ -> raise Common.NoRuleAppliesException
  
let rec computety ctx tyT =
  match tyT with
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise Common.NoRuleAppliesException
  
let rec simplifyty ctx tyT =
  try let tyT' = computety ctx tyT in simplifyty ctx tyT'
  with | Common.NoRuleAppliesException -> tyT
  
let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT
  in
    match (tyS, tyT) with
    | (TyTop, TyTop) -> true
    | (TyBot, TyBot) -> true
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
        (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
    | (TyBool, TyBool) -> true
    | (TyVar (i, _), _) when istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
    | (_, TyVar (i, _)) when istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
    | (TyVar (i, _), TyVar (j, _)) -> i = j
    | _ -> false
  
let rec subtype ctx tyS tyT =
  (tyeqv ctx tyS tyT) ||
    (let tyS = simplifyty ctx tyS in
     let tyT = simplifyty ctx tyT
     in
       match (tyS, tyT) with
       | (_, TyTop) -> true
       | (TyBot, _) -> true
       | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
           (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
       | (_) -> false)
  
let rec join ctx tyS tyT =
  if subtype ctx tyS tyT
  then tyT
  else
    if subtype ctx tyT tyS
    then tyS
    else
      (let tyS = simplifyty ctx tyS in
       let tyT = simplifyty ctx tyT
       in
         match (tyS, tyT) with
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             TyArr (meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
         | _ -> TyTop)
and meet ctx tyS tyT =
  if subtype ctx tyS tyT
  then tyS
  else
    if subtype ctx tyT tyS
    then tyT
    else
      (let tyS = simplifyty ctx tyS in
       let tyT = simplifyty ctx tyT
       in
         match (tyS, tyT) with
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             TyArr (join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
         | _ -> TyBot)
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeof ctx t =
  match t with
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2
      in
        (match simplifyty ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subtype ctx tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | TyBot -> TyBot
         | _ -> error fi "arrow type expected")
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if subtype ctx (typeof ctx t1) TyBool
      then join ctx (typeof ctx t2) (typeof ctx t3)
      else error fi "guard of conditional not a boolean"
  | TmError _ -> TyBot
  | TmTry (_, t1, t2) -> join ctx (typeof ctx t1) (typeof ctx t2)
  

