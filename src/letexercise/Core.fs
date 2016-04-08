﻿(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Core typechecking and evaluation functions.
module Core

open FSharp.Compatibility.OCaml
open Ast

(* ------------------------   EVALUATION  ------------------------ *)
let rec isval ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmAbs (_, _, _, _) -> true
  | _ -> false
  
exception NoRuleApplies
  
let rec eval1 ctx t =
    match t with
    | (* Insert case(s) for TmLet here *) _ ->
        raise <| System.NotImplementedException ()
    | TmApp (fi, (TmAbs (_, x, tyT11, t12)), v2) when isval ctx v2 ->
        termSubstTop v2 t12
    | TmApp (fi, v1, t2) when isval ctx v1 ->
        let t2' = eval1 ctx t2
        TmApp (fi, v1, t2')
    | TmApp (fi, t1, t2) ->
        let t1' = eval1 ctx t1
        TmApp (fi, t1', t2)
    | TmIf (_, (TmTrue _), t2, t3) ->
        t2
    | TmIf (_, (TmFalse _), t2, t3) ->
        t3
    | TmIf (fi, t1, t2, t3) ->
        let t1' = eval1 ctx t1
        TmIf (fi, t1', t2, t3)
    | _ ->
        raise NoRuleApplies
  
let rec eval ctx t =
    try let t' = eval1 ctx t
        eval ctx t'
    with NoRuleApplies -> t
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeof ctx t =
    match t with
    | TmVar (fi, i, _) ->
        getTypeFromContext fi ctx i
    | (* Insert case(s) for TmLet here *) _ ->
        raise <| System.NotImplementedException ()
    | TmAbs (fi, x, tyT1, t2) ->
        let ctx' = addbinding ctx x (VarBind tyT1)
        let tyT2 = typeof ctx' t2
        TyArr (tyT1, tyT2)
    | TmApp (fi, t1, t2) ->
        let tyT1 = typeof ctx t1
        let tyT2 = typeof ctx t2
        match tyT1 with
        | TyArr (tyT11, tyT12) ->
            if tyT2 = tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ ->
            error fi "arrow type expected"
    | TmTrue fi ->
        TyBool
    | TmFalse fi ->
        TyBool
    | TmIf (fi, t1, t2, t3) ->
        if (typeof ctx t1) = TyBool then
            let tyT2 = typeof ctx t2
            if tyT2 = (typeof ctx t3) then tyT2
            else error fi "arms of conditional have different types"
        else error fi "guard of conditional not a boolean"
  

