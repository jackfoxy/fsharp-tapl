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
let rec isval ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | TmRecord (fi, fields) ->
      let rec evalafield l =
        (match l with
         | [] -> raise Common.NoRuleAppliesException
         | (l, vi) :: rest when isval ctx vi ->
             let rest' = evalafield rest in (l, vi) :: rest'
         | (l, ti) :: rest -> let ti' = eval1 ctx ti in (l, ti') :: rest) in
      let fields' = evalafield fields in TmRecord (fi, fields')
  | TmProj (_, ((TmRecord (_, fields) as v1)), l) when isval ctx v1 ->
        match List.assoc l fields with 
        | Some x -> x
        | None -> raise Common.NoRuleAppliesException
  | TmProj (fi, t1, l) -> let t1' = eval1 ctx t1 in TmProj (fi, t1', l)
  | TmIf (_, (TmTrue _), t2,_) -> t2
  | TmIf (_, (TmFalse _), _, t3) -> t3
  | TmIf (fi, t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (fi, t1', t2, t3)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   SUBTYPING  ------------------------ *)
let rec subType tyS tyT =
  (tyS = tyT) ||
    (match (tyS, tyT) with
     | (_, TyTop) -> true
     | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
         (subType tyT1 tyS1) && (subType tyS2 tyT2)
     | (TyRecord fS, TyRecord fT) ->
         List.forall
           (fun (li, tyTi) ->
              match List.assoc li fS with
              | Some tySi ->  subType tySi tyTi
              | None -> false)
           fT
     | (_) -> false)
  
let rec join tyS tyT = (* Write me *) assert false
and meet tyS tyT = (* Write me *) assert false
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeOf ctx t =
  match t with
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addBinding ctx x (VarBind tyT1)
      let tyT2 = typeOf ctx' t2
      TyArr (tyT1, tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeOf ctx t1
      let tyT2 = typeOf ctx t2
      in
        (match tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subType tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | _ -> error fi "arrow type expected")
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, (typeOf ctx ti))) fields
      in TyRecord fieldtys
  | TmProj (fi, t1, l) ->
      (match typeOf ctx t1 with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some x -> x
            | None -> error fi ("label " + (l + " not found"))
       | _ -> error fi "Expected record type")
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (_) -> raise <| System.NotImplementedException ()
  

