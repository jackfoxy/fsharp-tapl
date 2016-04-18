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
  
let rec isnumericval ctx t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval ctx t1
  | _ -> false
  
let rec isval ctx t =
  match t with
  | TmString _ -> true
  | TmUnit _ -> true
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | TmPack (_, _, v1, _) when isval ctx v1 -> true
  | TmTAbs (_) -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | TmLet (_, _, v1, t2) when isval ctx v1 -> termSubstTop v1 t2
  | TmLet (fi, x, t1, t2) -> let t1' = eval1 ctx t1 in TmLet (fi, x, t1', t2)
  | (TmFix (_, v1) as t) when isval ctx v1 ->
      (match v1 with
       | TmAbs (_, _, _, t12) -> termSubstTop t t12
       | _ -> raise Common.NoRuleAppliesException)
  | TmFix (fi, t1) -> let t1' = eval1 ctx t1 in TmFix (fi, t1')
  | TmAscribe (_, v1, _) when isval ctx v1 -> v1
  | TmAscribe (fi, t1, tyT) ->
      let t1' = eval1 ctx t1 in TmAscribe (fi, t1', tyT)
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
  | TmTimesfloat (fi, (TmFloat (_, f1)), (TmFloat (_, f2))) ->
      TmFloat (fi, f1 * f2)
  | TmTimesfloat (fi, ((TmFloat (_) as t1)), t2) ->
      let t2' = eval1 ctx t2 in TmTimesfloat (fi, t1, t2')
  | TmTimesfloat (fi, t1, t2) ->
      let t1' = eval1 ctx t1 in TmTimesfloat (fi, t1', t2)
  | TmSucc (fi, t1) -> let t1' = eval1 ctx t1 in TmSucc (fi, t1')
  | TmPred (_, (TmZero _)) -> TmZero dummyinfo
  | TmPred (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 -> nv1
  | TmPred (fi, t1) -> let t1' = eval1 ctx t1 in TmPred (fi, t1')
  | TmIsZero (_, (TmZero _)) -> TmTrue dummyinfo
  | TmIsZero (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 ->
      TmFalse dummyinfo
  | TmIsZero (fi, t1) -> let t1' = eval1 ctx t1 in TmIsZero (fi, t1')
  | TmUnpack (_, _, _, (TmPack (_, tyT11, v12, _)), t2) when isval ctx v12
      -> tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let t1' = eval1 ctx t1 in TmUnpack (fi, tyX, x, t1', t2)
  | TmPack (fi, tyT1, t2, tyT3) ->
      let t2' = eval1 ctx t2 in TmPack (fi, tyT1, t2', tyT3)
  | TmVar (fi, n, _) ->
      (match getbinding fi ctx n with
       | TmAbbBind (t, _) -> t
       | _ -> raise Common.NoRuleAppliesException)
  | TmTApp (_, (TmTAbs (_, _, t11)), tyT2) -> tytermSubstTop tyT2 t11
  | TmTApp (fi, t1, tyT2) -> let t1' = eval1 ctx t1 in TmTApp (fi, t1', tyT2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
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
    | (TyString, TyString) -> true
    | (TyUnit, TyUnit) -> true
    | (TyId b1, TyId b2) -> b1 = b2
    | (TyFloat, TyFloat) -> true
    | (TyVar (i, _), _) when istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
    | (_, TyVar (i, _)) when istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
    | (TyVar (i, _), TyVar (j, _)) -> i = j
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
        (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
    | (TyBool, TyBool) -> true
    | (TyNat, TyNat) -> true
    | (TySome (tyX1, tyS2), TySome (_, tyT2)) ->
        let ctx1 = addname ctx tyX1 in tyeqv ctx1 tyS2 tyT2
    | (TyRecord fields1, TyRecord fields2) ->
        ((List.length fields1) = (List.length fields2)) &&
          (List.forall
             (fun (li2, tyTi2) ->
                match List.assoc li2 fields1 with
                | Some tyTi1 -> tyeqv ctx tyTi1 tyTi2
                | None -> false)
             fields2)
    | (TyAll (tyX1, tyS2), TyAll (_, tyT2)) ->
        let ctx1 = addname ctx tyX1 in tyeqv ctx1 tyS2 tyT2
    | _ -> false
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeof ctx t =
  match t with
  | TmInert (_, tyT) -> tyT
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
             if tyeqv ctx tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | _ -> error fi "arrow type expected")
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x (VarBind tyT1)
      in typeShift (-1) (typeof ctx' t2)
  | TmFix (fi, t1) ->
      let tyT1 = typeof ctx t1
      in
        (match simplifyty ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if tyeqv ctx tyT12 tyT11
             then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmString _ -> TyString
  | TmUnit _ -> TyUnit
  | TmAscribe (fi, t1, tyT) ->
      if tyeqv ctx (typeof ctx t1) tyT
      then tyT
      else error fi "body of as-term does not have the expected type"
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, (typeof ctx ti))) fields
      in TyRecord fieldtys
  | TmProj (fi, t1, l) ->
      (match simplifyty ctx (typeof ctx t1) with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some x -> x
            | None -> error fi ("label " ^ (l ^ " not found"))
       | _ -> error fi "Expected record type")
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if tyeqv ctx (typeof ctx t1) TyBool
      then
        (let tyT2 = typeof ctx t2
         in
           if tyeqv ctx tyT2 (typeof ctx t3)
           then tyT2
           else error fi "arms of conditional have different types")
      else error fi "guard of conditional not a boolean"
  | TmFloat _ -> TyFloat
  | TmTimesfloat (fi, t1, t2) ->
      if
        (tyeqv ctx (typeof ctx t1) TyFloat) &&
          (tyeqv ctx (typeof ctx t2) TyFloat)
      then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmZero _ -> TyNat
  | TmSucc (fi, t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat
      then TyNat
      else error fi "argument of succ is not a number"
  | TmPred (fi, t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat
      then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero (fi, t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat
      then TyBool
      else error fi "argument of iszero is not a number"
  | TmPack (fi, tyT1, t2, tyT) ->
      (match simplifyty ctx tyT with
       | TySome (_, tyT2) ->
           let tyU = typeof ctx t2 in
           let tyU' = typeSubstTop tyT1 tyT2
           in
             if tyeqv ctx tyU tyU'
             then tyT
             else error fi "doesn't match declared type"
       | _ -> error fi "existential type expected")
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let tyT1 = typeof ctx t1
      in
        (match simplifyty ctx tyT1 with
         | TySome (_, tyT11) ->
             let ctx' = addbinding ctx tyX TyVarBind in
             let ctx'' = addbinding ctx' x (VarBind tyT11) in
             let tyT2 = typeof ctx'' t2 in typeShift (-2) tyT2
         | _ -> error fi "existential type expected")
  | TmTAbs (_, tyX, t2) ->
      let ctx = addbinding ctx tyX TyVarBind in
      let tyT2 = typeof ctx t2 in TyAll (tyX, tyT2)
  | TmTApp (fi, t1, tyT2) ->
      let tyT1 = typeof ctx t1
      in
        (match simplifyty ctx tyT1 with
         | TyAll (_, tyT12) -> typeSubstTop tyT2 tyT12
         | _ -> error fi "universal type expected")
  
let evalbinding ctx b =
  match b with
  | TmAbbBind (t, tyT) -> let t' = eval ctx t in TmAbbBind (t', tyT)
  | bind -> bind
  

