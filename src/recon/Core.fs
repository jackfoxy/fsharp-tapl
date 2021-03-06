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
open Compatability

(* ------------------------   EVALUATION  ------------------------ *)
  
let rec isnumericval ctx t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval ctx t1
  | _ -> false
  
let rec isval ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmIf (_, (TmTrue _), t2,_) -> t2
  | TmIf (_, (TmFalse _), _, t3) -> t3
  | TmIf (fi, t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (fi, t1', t2, t3)
  | TmSucc (fi, t1) -> let t1' = eval1 ctx t1 in TmSucc (fi, t1')
  | TmPred (_, (TmZero _)) -> TmZero dummyinfo
  | TmPred (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 -> nv1
  | TmPred (fi, t1) -> let t1' = eval1 ctx t1 in TmPred (fi, t1')
  | TmIsZero (_, (TmZero _)) -> TmTrue dummyinfo
  | TmIsZero (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 ->
      TmFalse dummyinfo
  | TmIsZero (fi, t1) -> let t1' = eval1 ctx t1 in TmIsZero (fi, t1')
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   TYPING  ------------------------ *)
type Constr = (Ty * Ty) list

let emptyConstr : Constr = []
  
let combineConstr : (Constr -> Constr -> Constr) = List.append
  
let prconstr constr =
  let pc (tyS, tyT) =
    (printTyType false tyS; pr "="; printTyType false tyT) in
  let rec f l =
    match l with
    | [] -> ()
    | [ c ] -> pc c
    | c :: rest -> (pc c; pr ", "; f rest)
  in (pr "{"; f constr; pr "}")
  
type NextUVar =
  | NextUVar of string * UvarGenerator

and UvarGenerator = unit -> NextUVar

let uvargen : (unit -> NextUVar) =
    let rec f n () =
        NextUVar ("?X" + (string n), f (n + 1))    
    f 0
  
let rec recon ctx nextuvar t =
  match t with
  | TmVar (fi, i, _) ->
      let tyT = getTypeFromContext fi ctx i in (tyT, nextuvar, [])
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addBinding ctx x (VarBind tyT1) in
      let (tyT2, nextuvar2, constr2) = recon ctx' nextuvar t2
      in ((TyArr (tyT1, tyT2)), nextuvar2, constr2)
  | TmApp (_, t1, t2) ->
      let (tyT1, nextuvar1, constr1) = recon ctx nextuvar t1 in
      let (tyT2, nextuvar2, constr2) = recon ctx nextuvar1 t2 in
      let (NextUVar (tyX, nextuvar')) = nextuvar2 () in
      let newconstr = [ (tyT1, (TyArr (tyT2, TyId tyX))) ]
      in
        ((TyId tyX), nextuvar',
         (List.concat [ newconstr; constr1; constr2 ]))
  | TmZero _ -> (TyNat, nextuvar, [])
  | TmSucc (_, t1) ->
      let (tyT1, nextuvar1, constr1) = recon ctx nextuvar t1
      in (TyNat, nextuvar1, ((tyT1, TyNat) :: constr1))
  | TmPred (_, t1) ->
      let (tyT1, nextuvar1, constr1) = recon ctx nextuvar t1
      in (TyNat, nextuvar1, ((tyT1, TyNat) :: constr1))
  | TmIsZero (_, t1) ->
      let (tyT1, nextuvar1, constr1) = recon ctx nextuvar t1
      in (TyBool, nextuvar1, ((tyT1, TyNat) :: constr1))
  | TmTrue _ -> (TyBool, nextuvar, [])
  | TmFalse _ -> (TyBool, nextuvar, [])
  | TmIf (_, t1, t2, t3) ->
      let (tyT1, nextuvar1, constr1) = recon ctx nextuvar t1 in
      let (tyT2, nextuvar2, constr2) = recon ctx nextuvar1 t2 in
      let (tyT3, nextuvar3, constr3) = recon ctx nextuvar2 t3 in
      let newconstr = [ (tyT1, TyBool); (tyT2, tyT3) ]
      in
        (tyT3, nextuvar3,
         (List.concat [ newconstr; constr1; constr2; constr3 ]))
  
let substinty tyX tyT tyS =
  let rec f tyS =
    match tyS with
    | TyArr (tyS1, tyS2) -> TyArr (f tyS1, f tyS2)
    | TyNat -> TyNat
    | TyBool -> TyBool
    | TyId s -> if s = tyX then tyT else TyId s
  in f tyS
  
let applySubst constr tyT =
  List.fold
    (fun tyS -> function | (TyId tyX, tyC2) -> substinty tyX tyC2 tyS | _ -> invalidArg "can't get here" "") tyT
    (List.rev constr)
  
let substinconstr tyX tyT constr =
  List.map
    (fun (tyS1, tyS2) -> ((substinty tyX tyT tyS1), (substinty tyX tyT tyS2)))
    constr
  
let occursin tyX tyT =
  let rec o tyT =
    match tyT with
    | TyArr (tyT1, tyT2) -> (o tyT1) || (o tyT2)
    | TyNat -> false
    | TyBool -> false
    | TyId s -> s = tyX
  in o tyT
  
let unify fi (_ : Context) msg constr =
  let rec u constr =
    match constr with
    | [] -> []
    | (tyS, TyId tyX) :: rest ->
        if tyS = (TyId tyX)
        then u rest
        else
          if occursin tyX tyS
          then error fi (msg + ": circular constraints")
          else
            List.append (u (substinconstr tyX tyS rest))
              [ ((TyId tyX), tyS) ]
    | (TyId tyX, tyT) :: rest ->
        if tyT = (TyId tyX)
        then u rest
        else
          if occursin tyX tyT
          then error fi (msg + ": circular constraints")
          else
            List.append (u (substinconstr tyX tyT rest))
              [ ((TyId tyX), tyT) ]
    | (TyNat, TyNat) :: rest -> u rest
    | (TyBool, TyBool) :: rest -> u rest
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) :: rest ->
        u ((tyS1, tyT1) :: (tyS2, tyT2) :: rest)
    | (_) :: _ -> error fi "Unsolvable constraints"
  in u constr
  

