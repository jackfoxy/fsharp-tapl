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
  
let rec isnumericval ctx t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval ctx t1
  | _ -> false
  
let rec isval ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmString _ -> true
  | TmUnit _ -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) ->
      List.forall (fun (_, (_, ti)) -> isval ctx ti) fields
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
  | TmIf (_, (TmTrue _), t2,_) -> t2
  | TmIf (_, (TmFalse _), _, t3) -> t3
  | TmIf (fi, t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (fi, t1', t2, t3)
  | TmRecord (fi, fields) ->
      let rec evalafield l =
        (match l with
         | [] -> raise Common.NoRuleAppliesException
         | (l, (vari, vi)) :: rest when isval ctx vi ->
             let rest' = evalafield rest in (l, (vari, vi)) :: rest'
         | (l, (vari, ti)) :: rest ->
             let ti' = eval1 ctx ti in (l, (vari, ti')) :: rest) in
      let fields' = evalafield fields in TmRecord (fi, fields')
  | TmProj (_, ((TmRecord (_, fields) as v1)), l) when isval ctx v1 ->
        match List.assoc l fields with
        | Some (_, ti) -> ti
        | None ->  raise Common.NoRuleAppliesException
  | TmProj (fi, t1, l) -> let t1' = eval1 ctx t1 in TmProj (fi, t1', l)
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
  | TmTimesfloat (fi, (TmFloat (_, f1)), (TmFloat (_, f2))) ->
      TmFloat (fi, f1 * f2)
  | TmTimesfloat (fi, ((TmFloat (_) as t1)), t2) ->
      let t2' = eval1 ctx t2 in TmTimesfloat (fi, t1, t2')
  | TmTimesfloat (fi, t1, t2) ->
      let t1' = eval1 ctx t1 in TmTimesfloat (fi, t1', t2)
  | TmTApp (_, (TmTAbs (_, _, _, t11)), tyT2) -> tyTermSubstTop tyT2 t11
  | TmTApp (fi, t1, tyT2) -> let t1' = eval1 ctx t1 in TmTApp (fi, t1', tyT2)
  | TmSucc (fi, t1) -> let t1' = eval1 ctx t1 in TmSucc (fi, t1')
  | TmPred (_, (TmZero _)) -> TmZero dummyinfo
  | TmPred (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 -> nv1
  | TmPred (fi, t1) -> let t1' = eval1 ctx t1 in TmPred (fi, t1')
  | TmIsZero (_, (TmZero _)) -> TmTrue dummyinfo
  | TmIsZero (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 ->
      TmFalse dummyinfo
  | TmIsZero (fi, t1) -> let t1' = eval1 ctx t1 in TmIsZero (fi, t1')
  | TmUnpack (_, _, _, (TmPack (_, tyT11, v12, _)), t2) when isval ctx v12
      -> tyTermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let t1' = eval1 ctx t1 in TmUnpack (fi, tyX, x, t1', t2)
  | TmPack (fi, tyT1, t2, tyT3) ->
      let t2' = eval1 ctx t2 in TmPack (fi, tyT1, t2', tyT3)
  | TmVar (fi, n, _) ->
      (match getBinding fi ctx n with
       | TmAbbBind (t, _) -> t
       | _ -> raise Common.NoRuleAppliesException)
  | TmUpdate (_, (TmRecord (_, fields)), l, v2) ->
      let newfields =
        List.map
          (fun (((li, (vari, _)) as f)) ->
             if li = l then (li, (vari, v2)) else f)
          fields
      in TmRecord (dummyinfo, newfields)
  | TmUpdate (fi, v1, l, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmUpdate (fi, v1, l, t2')
  | TmUpdate (fi, t1, l, t2) ->
      let t1' = eval1 ctx t1 in TmUpdate (fi, t1', l, t2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
(* ------------------------   KINDING  ------------------------ *)
let istyabb ctx i =
  match getBinding dummyinfo ctx i with
  | TyAbbBind (_) -> true
  | _ -> false
  
let gettyabb ctx i =
  match getBinding dummyinfo ctx i with
  | TyAbbBind (tyT, _) -> tyT
  | _ -> raise Common.NoRuleAppliesException
  
let rec computety ctx tyT =
  match tyT with
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
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
    | (TyString, TyString) -> true
    | (TyTop, TyTop) -> true
    | (TyUnit, TyUnit) -> true
    | (TyId b1, TyId b2) -> b1 = b2
    | (TyFloat, TyFloat) -> true
    | (TyVar (i, _), _) when istyabb ctx i -> tyEqv ctx (gettyabb ctx i) tyT
    | (_, TyVar (i, _)) when istyabb ctx i -> tyEqv ctx tyS (gettyabb ctx i)
    | (TyVar (i, _), TyVar (j, _)) -> i = j
    | (TyBool, TyBool) -> true
    | (TyNat, TyNat) -> true
    | (TySome (tyX1, tyS1, tyS2), TySome (_, tyT1, tyT2)) ->
        let ctx1 = addName ctx tyX1
        in (tyEqv ctx tyS1 tyT1) && (tyEqv ctx1 tyS2 tyT2)
    | (TyRecord fields1, TyRecord fields2) ->
        ((List.length fields1) = (List.length fields2)) &&
          (List.forall
             (fun (li2, (varTi2, tyTi2)) ->
                match List.assoc li2 fields1 with
                | Some (varTi1, tyTi1) -> 
                    (varTi1 = varTi2) && (tyEqv ctx tyTi1 tyTi2)
                | None -> false)
             fields2)
    | (TyAll (tyX1, tyS1, tyS2), TyAll (_, tyT1, tyT2)) ->
        let ctx1 = addName ctx tyX1
        in (tyEqv ctx tyS1 tyT1) && (tyEqv ctx1 tyS2 tyT2)
    | (TyAbs (tyX1, knKS1, tyS2), TyAbs (_, knKT1, tyT2)) ->
        (knKS1 = knKT1) &&
          (let ctx = addName ctx tyX1 in tyEqv ctx tyS2 tyT2)
    | (TyApp (tyS1, tyS2), TyApp (tyT1, tyT2)) ->
        (tyEqv ctx tyS1 tyT1) && (tyEqv ctx tyS2 tyT2)
    | _ -> false
  
let evalBinding ctx b =
  match b with
  | TmAbbBind (t, tyT) -> let t' = eval ctx t in TmAbbBind (t', tyT)
  | bind -> bind
  
let rec getkind fi ctx i =
  match getBinding fi ctx i with
  | TyVarBind tyT -> kindOf ctx tyT
  | TyAbbBind (_, (Some knK)) -> knK
  | TyAbbBind (_, None) ->
      error fi ("No kind recorded for variable " + (index2Name fi ctx i))
  | _ ->
      error fi
        ("getkind: Wrong kind of binding for variable " +
           (index2Name fi ctx i))
and kindOf ctx tyT =
  match tyT with
  | TyVar (i, _) -> let knK = getkind dummyinfo ctx i in knK
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
  | TySome (tyX, tyT1, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind tyT1)
      in
        (if (kindOf ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | TyAll (tyX, tyT1, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind tyT1)
      in
        (if (kindOf ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | TyArr (tyT1, tyT2) ->
      (if (kindOf ctx tyT1) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       if (kindOf ctx tyT2) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | TyRecord fldtys ->
      (List.iter
         (fun (_, (_, tyS)) ->
            if (kindOf ctx tyS) <> KnStar
            then error dummyinfo "Kind * expected"
            else ())
         fldtys;
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
       | (TyRecord fS, TyRecord fT) ->
           List.forall
             (fun (li, (varTi, tyTi)) ->
                match List.assoc li fS with
                | Some (varSi, tySi) ->
                    ((varSi = Invariant) || (varTi = Covariant)) &&
                      (subType ctx tySi tyTi)
                | None -> false)
             fT
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
       | (TySome (tyX1, tyS1, tyS2), TySome (_, tyT1, tyT2)) ->
           ((subType ctx tyS1 tyT1) && (subType ctx tyT1 tyS1)) &&
             (let ctx1 = addBinding ctx tyX1 (TyVarBind tyT1)
              in subType ctx1 tyS2 tyT2)
       | (_) -> false)
  
let rec join ctx tyS tyT =
  if subType ctx tyS tyT
  then tyT
  else
    if subType ctx tyT tyS
    then tyS
    else
      (let tyS = simplifyTy ctx tyS in
       let tyT = simplifyTy ctx tyT
       in
         match (tyS, tyT) with
         | (TyRecord fS, TyRecord fT) ->
             let labelsS = List.map (fun (li, _) -> li) fS in
             let labelsT = List.map (fun (li, _) -> li) fT in
             let commonLabels =
               List.filter (fun l -> List.exists (fun x -> x = l) labelsT) labelsS in
             let commonFields =
               List.map
                 (fun li ->
                    let (vSi, tySi) = 
                        match List.assoc li fS with
                        | Some x -> x
                        | None -> raise Common.NotFoundException
                    let (vTi, tyTi) = 
                        match List.assoc li fT with
                        | Some x -> x
                        | None -> raise Common.NotFoundException
                    let vi = if vSi = vTi then vSi else Invariant
                    (li, (vi, (join ctx tySi tyTi))))
                 commonLabels
             in TyRecord commonFields
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             (try TyArr (meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
              with | Common.NotFoundException -> TyTop)
         | (TyAll (tyX, tyS1, _), TyAll (_, tyT1, tyT2)) ->
             if not ((subType ctx tyS1 tyT1) && (subType ctx tyT1 tyS1))
             then TyTop
             else
               (let ctx' = addBinding ctx tyX (TyVarBind tyT1)
                in TyAll (tyX, tyS1, join ctx' tyT1 tyT2))
         | _ -> TyTop)
and meet ctx tyS tyT =
  if subType ctx tyS tyT
  then tyS
  else
    if subType ctx tyT tyS
    then tyT
    else
      (let tyS = simplifyTy ctx tyS in
       let tyT = simplifyTy ctx tyT
       in
         match (tyS, tyT) with
         | (TyRecord fS, TyRecord fT) ->
             let labelsS = List.map (fun (li, _) -> li) fS in
             let labelsT = List.map (fun (li, _) -> li) fT in
             let allLabels =
               List.append labelsS
                 (List.filter (fun l -> not (List.exists (fun x -> x = l) labelsS)) labelsT) in
             let allFields =
               List.map
                 (fun li ->
                    if List.exists (fun x-> x = li) allLabels
                    then
                      (let (vSi, tySi) = 
                        match List.assoc li fS with
                        | Some x -> x
                        | None -> raise Common.NotFoundException
                       let (vTi, tyTi) = 
                        match List.assoc li fT with
                        | Some x -> x
                        | None -> raise Common.NotFoundException
                       let vi = if vSi = vTi then vSi else Covariant
                       (li, (vi, (meet ctx tySi tyTi))))
                    else
                      if List.exists (fun x -> x = li) labelsS
                      then (li, (match List.assoc li fS with
                                    | Some x -> x
                                    | None -> raise Common.NotFoundException))
                      else (li, (match List.assoc li fT with
                                    | Some x -> x
                                    | None -> raise Common.NotFoundException)))
                 allLabels
             in TyRecord allFields
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             TyArr (join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
         | (TyAll (tyX, tyS1, _), TyAll (_, tyT1, tyT2)) ->
             if not ((subType ctx tyS1 tyT1) && (subType ctx tyT1 tyS1))
             then raise Common.NotFoundException
             else
               (let ctx' = addBinding ctx tyX (TyVarBind tyT1)
                in TyAll (tyX, tyS1, meet ctx' tyT1 tyT2))
         | _ -> raise Common.NotFoundException)
  
(* ------------------------   TYPING  ------------------------ *)
let rec lcst ctx tyS =
  let tyS = simplifyTy ctx tyS
  in try lcst ctx (promote ctx tyS) with | Common.NoRuleAppliesException -> tyS
  
let rec typeOf ctx t =
  match t with
  | TmInert (_, tyT) -> tyT
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
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if subType ctx (typeOf ctx t1) TyBool
      then join ctx (typeOf ctx t2) (typeOf ctx t3)
      else error fi "guard of conditional not a boolean"
  | TmRecord (_, fields) ->
      let fieldtys =
        List.map (fun (li, (vari, ti)) -> (li, (vari, (typeOf ctx ti))))
          fields
      in TyRecord fieldtys
  | TmProj (fi, t1, l) ->
      (match lcst ctx (typeOf ctx t1) with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some (_, tyTi) -> tyTi
            | None-> error fi ("label " + (l + " not found"))
       | _ -> error fi "Expected record type")
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let ctx' = addBinding ctx x (VarBind tyT1)
      in typeShift (-1) (typeOf ctx' t2)
  | TmFix (fi, t1) ->
      let tyT1 = typeOf ctx t1
      in
        (match lcst ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subType ctx tyT12 tyT11
             then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmString _ -> TyString
  | TmUnit _ -> TyUnit
  | TmAscribe (fi, t1, tyT) ->
      (checkkindstar fi ctx tyT;
       if subType ctx (typeOf ctx t1) tyT
       then tyT
       else error fi "body of as-term does not have the expected type")
  | TmFloat _ -> TyFloat
  | TmTimesfloat (fi, t1, t2) ->
      if
        (subType ctx (typeOf ctx t1) TyFloat) &&
          (subType ctx (typeOf ctx t2) TyFloat)
      then TyFloat
      else error fi "argument of timesfloat is not a number"
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
  | TmZero _ -> TyNat
  | TmSucc (fi, t1) ->
      if subType ctx (typeOf ctx t1) TyNat
      then TyNat
      else error fi "argument of succ is not a number"
  | TmPred (fi, t1) ->
      if subType ctx (typeOf ctx t1) TyNat
      then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero (fi, t1) ->
      if subType ctx (typeOf ctx t1) TyNat
      then TyBool
      else error fi "argument of iszero is not a number"
  | TmPack (fi, tyT1, t2, tyT) ->
      (checkkindstar fi ctx tyT;
       (match simplifyTy ctx tyT with
        | TySome (_, tyBound, tyT2) ->
            (if not (subType ctx tyT1 tyBound)
             then error fi "hidden type not a subType of bound"
             else ();
             let tyU = typeOf ctx t2 in
             let tyU' = typeSubstTop tyT1 tyT2
             in
               if subType ctx tyU tyU'
               then tyT
               else error fi "doesn't match declared type")
        | _ -> error fi "existential type expected"))
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let tyT1 = typeOf ctx t1
      in
        (match lcst ctx tyT1 with
         | TySome (_, tyBound, tyT11) ->
             let ctx' = addBinding ctx tyX (TyVarBind tyBound) in
             let ctx'' = addBinding ctx' x (VarBind tyT11) in
             let tyT2 = typeOf ctx'' t2 in typeShift (-2) tyT2
         | _ -> error fi "existential type expected")
  | TmUpdate (fi, t1, l, t2) ->
      let tyT1 = typeOf ctx t1 in
      let tyT2 = typeOf ctx t2
      in
        (match lcst ctx tyT1 with
         | TyRecord fieldtys ->
                match List.assoc l fieldtys with
                | Some (varTi, tyTi) ->
                  if varTi <> Invariant then error fi "field not invariant"
                  else ()
                  if subType ctx tyT2 tyTi then tyT1
                  else error fi "type of new field value doesn't match"
                | None -> error fi ("label " + (l + " not found"))
         | _ -> error fi "Expected record type")
  

