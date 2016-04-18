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
let rec isval ctx t =
  match t with
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
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   SUBTYPING  ------------------------ *)
let rec subtype tyS tyT =
  (tyS = tyT) ||
    (match (tyS, tyT) with
     | (_, TyTop) -> true
     | (TyBot, _) -> true
     | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
         (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
     | (TyRecord fS, TyRecord fT) ->
         List.forall
           (fun (li, tyTi) ->
                match List.assoc li fS with
                | Some tySi -> subtype tySi tyTi
                | None -> false)
           fT
     | (_) -> false)
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeof ctx t =
  match t with
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, (typeof ctx ti))) fields
      in TyRecord fieldtys
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
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
  | TmProj (fi, t1, l) ->
      (match typeof ctx t1 with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some x -> x
            | None -> error fi ("label " ^ (l ^ " not found"))
       | TyBot -> TyBot
       | _ -> error fi "Expected record type")
  

