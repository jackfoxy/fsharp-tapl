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
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmString _ -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | TmTag (_, _, t1, _) -> isval ctx t1
  | TmUnit _ -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmIf (_, (TmTrue _), t2,_) -> t2
  | TmIf (_, (TmFalse _), _, t3) -> t3
  | TmIf (fi, t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (fi, t1', t2, t3)
  | TmVar (fi, n, _) ->
      (match getBinding fi ctx n with
       | TmAbbBind (t, _) -> t
       | _ -> raise Common.NoRuleAppliesException)
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
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
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
  | TmTag (fi, l, t1, tyT) ->
      let t1' = eval1 ctx t1 in TmTag (fi, l, t1', tyT)
  | TmCase (_, (TmTag (_, li, v11, _)), branches) when isval ctx v11 ->
        match List.assoc li branches with
        | Some (_, body) -> termSubstTop v11 body
        | None -> raise Common.NoRuleAppliesException
  | TmCase (fi, t1, branches) ->
      let t1' = eval1 ctx t1 in TmCase (fi, t1', branches)
  | TmLet (_, _, v1, t2) when isval ctx v1 -> termSubstTop v1 t2
  | TmLet (fi, x, t1, t2) -> let t1' = eval1 ctx t1 in TmLet (fi, x, t1', t2)
  | (TmFix (_, v1) as t) when isval ctx v1 ->
      (match v1 with
       | TmAbs (_, _, _, t12) -> termSubstTop t t12
       | _ -> raise Common.NoRuleAppliesException)
  | TmFix (fi, t1) -> let t1' = eval1 ctx t1 in TmFix (fi, t1')
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
let evalBinding ctx b =
  match b with
  | TmAbbBind (t, tyT) -> let t' = eval ctx t in TmAbbBind (t', tyT)
  | bind -> bind
  
let istyabb ctx i =
  match getBinding dummyinfo ctx i with | TyAbbBind _ -> true | _ -> false
  
let gettyabb ctx i =
  match getBinding dummyinfo ctx i with
  | TyAbbBind tyT -> tyT
  | _ -> raise Common.NoRuleAppliesException
  
let rec computety ctx tyT =
  match tyT with
  | (TyRec (_, tyS1) as tyS) -> typeSubstTop tyS tyS1
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise Common.NoRuleAppliesException
  
let rec simplifyTy ctx tyT =
  try let tyT' = computety ctx tyT in simplifyTy ctx tyT'
  with | Common.NoRuleAppliesException -> tyT
  
let rec private tyEqv' seen ctx tyS tyT =
  (List.exists (fun x -> x = (tyS, tyT)) seen) ||
    (match (tyS, tyT) with
     | (TyString, TyString) -> true
     | (TyFloat, TyFloat) -> true
     | (TyRec (_, tyS1), _) ->
         tyEqv' ((tyS, tyT) :: seen) ctx (typeSubstTop tyS tyS1) tyT
     | (_, TyRec (_, tyT1)) ->
         tyEqv' ((tyS, tyT) :: seen) ctx tyS (typeSubstTop tyT tyT1)
     | (TyId b1, TyId b2) -> b1 = b2
     | (TyVar (i, _), _) when istyabb ctx i ->
         tyEqv' seen ctx (gettyabb ctx i) tyT
     | (_, TyVar (i, _)) when istyabb ctx i ->
         tyEqv' seen ctx tyS (gettyabb ctx i)
     | (TyVar (i, _), TyVar (j, _)) -> i = j
     | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
         (tyEqv' seen ctx tyS1 tyT1) && (tyEqv' seen ctx tyS2 tyT2)
     | (TyBool, TyBool) -> true
     | (TyNat, TyNat) -> true
     | (TyRecord fields1, TyRecord fields2) ->
         ((List.length fields1) = (List.length fields2)) &&
           (List.forall
              (fun (li2, tyTi2) ->
                    match List.assoc li2 fields1 with
                    | Some tyTi1 -> tyEqv' seen ctx tyTi1 tyTi2
                    | None -> false)
              fields2)
     | (TyVariant fields1, TyVariant fields2) ->
         ((List.length fields1) = (List.length fields2)) &&
           (List.forall2
              (fun (li1, tyTi1) (li2, tyTi2) ->
                 (li1 = li2) && (tyEqv' seen ctx tyTi1 tyTi2))
              fields1 fields2)
     | (TyUnit, TyUnit) -> true
     | _ -> false)
  
let tyEqv (ctx : Context) tyS tyT = tyEqv' [] ctx tyS tyT
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeOf ctx t =
  match t with
  | TmInert (_, tyT) -> tyT
  | TmString _ -> TyString
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAscribe (fi, t1, tyT) ->
      if tyEqv ctx (typeOf ctx t1) tyT
      then tyT
      else error fi "body of as-term does not have the expected type"
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, (typeOf ctx ti))) fields
      in TyRecord fieldtys
  | TmProj (fi, t1, l) ->
      (match simplifyTy ctx (typeOf ctx t1) with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some x -> x
            | None -> error fi ("label " + (l + " not found"))
       | _ -> error fi "Expected record type")
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
  | TmFloat _ -> TyFloat
  | TmTimesfloat (fi, t1, t2) ->
      if
        (tyEqv ctx (typeOf ctx t1) TyFloat) &&
          (tyEqv ctx (typeOf ctx t2) TyFloat)
      then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if tyEqv ctx (typeOf ctx t1) TyBool
      then
        (let tyT2 = typeOf ctx t2
         in
           if tyEqv ctx tyT2 (typeOf ctx t3)
           then tyT2
           else error fi "arms of conditional have different types")
      else error fi "guard of conditional not a boolean"
  | TmZero _ -> TyNat
  | TmSucc (fi, t1) ->
      if tyEqv ctx (typeOf ctx t1) TyNat
      then TyNat
      else error fi "argument of succ is not a number"
  | TmPred (fi, t1) ->
      if tyEqv ctx (typeOf ctx t1) TyNat
      then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero (fi, t1) ->
      if tyEqv ctx (typeOf ctx t1) TyNat
      then TyBool
      else error fi "argument of iszero is not a number"
  | TmCase (fi, t, cases) ->
      (match simplifyTy ctx (typeOf ctx t) with
       | TyVariant fieldtys ->
           (List.iter
              (fun (li, (_)) ->
                    match List.assoc li fieldtys with
                    | Some _ -> ()
                    | None -> error fi ("label " + (li + " not in type")))
              cases;
            let casetypes =
              List.map
                (fun (li, (xi, ti)) ->
                    match List.assoc li fieldtys with
                    | Some tyTi ->
                       let ctx' = addBinding ctx xi (VarBind tyTi)
                       typeShift (-1) (typeOf ctx' ti)
                    | None -> error fi ("label " + (li + " not found")))
                cases in
            let tyT1 = List.head casetypes in
            let restTy = List.tail casetypes
            in
              (List.iter
                 (fun tyTi ->
                    if not (tyEqv ctx tyTi tyT1)
                    then error fi "fields do not have the same type"
                    else ())
                 restTy;
               tyT1))
       | _ -> error fi "Expected variant type")
  | TmTag (fi, li, ti, tyT) ->
       match simplifyTy ctx tyT with
       | TyVariant fieldtys ->
            match List.assoc li fieldtys with
            | Some tyTiExpected ->
                let tyTi = typeOf ctx ti
                if tyEqv ctx tyTi tyTiExpected then tyT
                else error fi "field does not have expected type"
            | None -> error fi ("label " + (li + " not found"))
       | _ -> error fi "Annotation is not a variant type"
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let ctx' = addBinding ctx x (VarBind tyT1)
      in typeShift (-1) (typeOf ctx' t2)
  | TmUnit _ -> TyUnit
  | TmFix (fi, t1) ->
      let tyT1 = typeOf ctx t1
      in
        (match simplifyTy ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if tyEqv ctx tyT12 tyT11
             then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  
