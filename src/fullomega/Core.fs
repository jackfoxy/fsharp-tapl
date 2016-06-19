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
  | TmLoc (_) -> true
  | TmFloat _ -> true
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | TmPack (_, _, v1, _) when isval ctx v1 -> true
  | TmTAbs (_) -> true
  | _ -> false
  
type Store = Term list

let emptyStore : Store = []
  
let extendstore store v = ((List.length store), (List.append store [ v ]))
  
let lookuploc store l = List.item l store
  
let updatestore store n v =
  let rec f s =
    match s with
    | (0, _ :: rest) -> v :: rest
    | (n, v' :: rest) -> v' :: (f ((n - 1), rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in f (n, store)
  
let shiftStore i store = List.map (fun t -> termShift i t) store
  
let rec eval1 ctx store t =
  match t with
  | TmAscribe (_, v1, _) when isval ctx v1 -> (v1, store)
  | TmAscribe (fi, t1, tyT) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmAscribe (fi, t1', tyT)), store')
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      ((termSubstTop v2 t12), store)
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let (t2', store') = eval1 ctx store t2
      in ((TmApp (fi, v1, t2')), store')
  | TmApp (fi, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmApp (fi, t1', t2)), store')
  | TmRecord (fi, fields) ->
      let rec evalafield l =
        (match l with
         | [] -> raise Common.NoRuleAppliesException
         | (l, vi) :: rest when isval ctx vi ->
             let (rest', store') = evalafield rest
             in (((l, vi) :: rest'), store')
         | (l, ti) :: rest ->
             let (ti', store') = eval1 ctx store ti
             in (((l, ti') :: rest), store')) in
      let (fields', store') = evalafield fields
      in ((TmRecord (fi, fields')), store')
  | TmProj (_, ((TmRecord (_, fields) as v1)), l) when isval ctx v1 ->
        match List.assoc l fields with
        | Some x -> (x, store)
        | None -> raise Common.NoRuleAppliesException
  | TmProj (fi, t1, l) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmProj (fi, t1', l)), store')
  | TmRef (fi, t1) ->
      if not (isval ctx t1)
      then
        (let (t1', store') = eval1 ctx store t1
         in ((TmRef (fi, t1')), store'))
      else
        (let (l, store') = extendstore store t1
         in ((TmLoc (dummyinfo, l)), store'))
  | TmDeref (fi, t1) ->
      if not (isval ctx t1)
      then
        (let (t1', store') = eval1 ctx store t1
         in ((TmDeref (fi, t1')), store'))
      else
        (match t1 with
         | TmLoc (_, l) -> ((lookuploc store l), store)
         | _ -> raise Common.NoRuleAppliesException)
  | TmAssign (fi, t1, t2) ->
      if not (isval ctx t1)
      then
        (let (t1', store') = eval1 ctx store t1
         in ((TmAssign (fi, t1', t2)), store'))
      else
        if not (isval ctx t2)
        then
          (let (t2', store') = eval1 ctx store t2
           in ((TmAssign (fi, t1, t2')), store'))
        else
          (match t1 with
           | TmLoc (_, l) -> ((TmUnit dummyinfo), (updatestore store l t2))
           | _ -> raise Common.NoRuleAppliesException)
  | TmTimesfloat (fi, (TmFloat (_, f1)), (TmFloat (_, f2))) ->
      ((TmFloat (fi, f1 * f2)), store)
  | TmTimesfloat (fi, ((TmFloat (_) as t1)), t2) ->
      let (t2', store') = eval1 ctx store t2
      in ((TmTimesfloat (fi, t1, t2')), store')
  | TmTimesfloat (fi, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmTimesfloat (fi, t1', t2)), store')
  | TmLet (_, _, v1, t2) when isval ctx v1 -> ((termSubstTop v1 t2), store)
  | TmLet (fi, x, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmLet (fi, x, t1', t2)), store')
  | TmIf (_, (TmTrue _), t2, _) -> (t2, store)
  | TmIf (_, (TmFalse _), _, t3) -> (t3, store)
  | TmIf (fi, t1, t2, t3) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmIf (fi, t1', t2, t3)), store')
  | TmSucc (fi, t1) ->
      let (t1', store') = eval1 ctx store t1 in ((TmSucc (fi, t1')), store')
  | TmPred (_, (TmZero _)) -> ((TmZero dummyinfo), store)
  | TmPred (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 -> (nv1, store)
  | TmPred (fi, t1) ->
      let (t1', store') = eval1 ctx store t1 in ((TmPred (fi, t1')), store')
  | TmIsZero (_, (TmZero _)) -> ((TmTrue dummyinfo), store)
  | TmIsZero (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 ->
      ((TmFalse dummyinfo), store)
  | TmIsZero (fi, t1) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmIsZero (fi, t1')), store')
  | (TmFix (_, v1) as t) when isval ctx v1 ->
      (match v1 with
       | TmAbs (_, _, _, t12) -> ((termSubstTop t t12), store)
       | _ -> raise Common.NoRuleAppliesException)
  | TmFix (fi, t1) ->
      let (t1', store') = eval1 ctx store t1 in ((TmFix (fi, t1')), store')
  | TmTApp (_, (TmTAbs (_, _, _, t11)), tyT2) ->
      ((tyTermSubstTop tyT2 t11), store)
  | TmTApp (fi, t1, tyT2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmTApp (fi, t1', tyT2)), store')
  | TmUnpack (_, _, _, (TmPack (_, tyT11, v12, _)), t2) when isval ctx v12
      -> ((tyTermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)), store)
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmUnpack (fi, tyX, x, t1', t2)), store')
  | TmPack (fi, tyT1, t2, tyT3) ->
      let (t2', store') = eval1 ctx store t2
      in ((TmPack (fi, tyT1, t2', tyT3)), store')
  | TmVar (fi, n, _) ->
      (match getBinding fi ctx n with
       | TmAbbBind (t, _) -> (t, store)
       | _ -> raise Common.NoRuleAppliesException)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx (store : Store) t =
  try let (t', store') = eval1 ctx store t in eval ctx store' t'
  with | Common.NoRuleAppliesException -> (t, store)
  
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
    | (TyString, TyString) -> true
    | (TyId b1, TyId b2) -> b1 = b2
    | (TyUnit, TyUnit) -> true
    | (TyRef tyT1, TyRef tyT2) -> tyEqv ctx tyT1 tyT2
    | (TyFloat, TyFloat) -> true
    | (TyVar (i, _), _) when istyabb ctx i -> tyEqv ctx (gettyabb ctx i) tyT
    | (_, TyVar (i, _)) when istyabb ctx i -> tyEqv ctx tyS (gettyabb ctx i)
    | (TyVar (i, _), TyVar (j, _)) -> i = j
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
        (tyEqv ctx tyS1 tyT1) && (tyEqv ctx tyS2 tyT2)
    | (TyBool, TyBool) -> true
    | (TyNat, TyNat) -> true
    | (TyRecord fields1, TyRecord fields2) ->
        ((List.length fields1) = (List.length fields2)) &&
          (List.forall
             (fun (li2, tyTi2) ->
                match List.assoc li2 fields1 with
                | Some tyTi1 -> tyEqv ctx tyTi1 tyTi2
                | None -> false)
             fields2)
    | (TySome (tyX1, knK1, tyS2), TySome (_, knK1', tyT2)) ->
        (knK1 = knK1') &&
          (let ctx1 = addName ctx tyX1 in tyEqv ctx1 tyS2 tyT2)
    | (TyAll (tyX1, knK1, tyS2), TyAll (_, knK2, tyT2)) ->
        let ctx1 = addName ctx tyX1
        in (knK1 = knK2) && (tyEqv ctx1 tyS2 tyT2)
    | (TyAbs (tyX1, knKS1, tyS2), TyAbs (_, knKT1, tyT2)) ->
        (knKS1 = knKT1) &&
          (let ctx = addName ctx tyX1 in tyEqv ctx tyS2 tyT2)
    | (TyApp (tyS1, tyS2), TyApp (tyT1, tyT2)) ->
        (tyEqv ctx tyS1 tyT1) && (tyEqv ctx tyS2 tyT2)
    | _ -> false
  
let getkind fi ctx i =
  match getBinding fi ctx i with
  | TyVarBind knK -> knK
  | TyAbbBind (_, (Some knK)) -> knK
  | TyAbbBind (_, None) ->
      error fi ("No kind recorded for variable " + (index2Name fi ctx i))
  | _ ->
      error fi
        ("getkind: Wrong kind of binding for variable " +
           (index2Name fi ctx i))
  
let rec kindOf ctx tyT =
  match tyT with
  | TyArr (tyT1, tyT2) ->
      (if (kindOf ctx tyT1) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       if (kindOf ctx tyT2) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | TyVar (i, _) -> let knK = getkind dummyinfo ctx i in knK
  | TyRecord fldtys ->
      (List.iter
         (fun (_, tyS) ->
            if (kindOf ctx tyS) <> KnStar
            then error dummyinfo "Kind * expected"
            else ())
         fldtys;
       KnStar)
  | TyAll (tyX, knK1, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind knK1)
      in
        (if (kindOf ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | TyAbs (tyX, knK1, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind knK1) in
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
  | TySome (tyX, knK, tyT2) ->
      let ctx' = addBinding ctx tyX (TyVarBind knK)
      in
        (if (kindOf ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | _ -> KnStar
  
let checkkindstar fi ctx tyT =
  let k = kindOf ctx tyT
  in if k = KnStar then () else error fi "Kind * expected"
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeOf ctx t =
  match t with
  | TmInert (_, tyT) -> tyT
  | TmAscribe (fi, t1, tyT) ->
      (checkkindstar fi ctx tyT;
       if tyEqv ctx (typeOf ctx t1) tyT
       then tyT
       else error fi "body of as-term does not have the expected type")
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (fi, x, tyT1, t2) ->
      (checkkindstar fi ctx tyT1;
       let ctx' = addBinding ctx x (VarBind tyT1) in
       let tyT2 = typeOf ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2))
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
  | TmString _ -> TyString
  | TmUnit _ -> TyUnit
  | TmRef (_, t1) -> TyRef (typeOf ctx t1)
  | TmLoc (fi, _) ->
      error fi "locations are not supposed to occur in source programs!"
  | TmDeref (fi, t1) ->
      (match simplifyTy ctx (typeOf ctx t1) with
       | TyRef tyT1 -> tyT1
       | _ -> error fi "argument of ! is not a Ref")
  | TmAssign (fi, t1, t2) ->
      (match simplifyTy ctx (typeOf ctx t1) with
       | TyRef tyT1 ->
           if tyEqv ctx (typeOf ctx t2) tyT1
           then TyUnit
           else error fi "arguments of := are incompatible"
       | _ -> error fi "argument of ! is not a Ref")
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
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let ctx' = addBinding ctx x (VarBind tyT1)
      in typeShift (-1) (typeOf ctx' t2)
  | TmFloat _ -> TyFloat
  | TmTimesfloat (fi, t1, t2) ->
      if
        (tyEqv ctx (typeOf ctx t1) TyFloat) &&
          (tyEqv ctx (typeOf ctx t2) TyFloat)
      then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmFix (fi, t1) ->
      let tyT1 = typeOf ctx t1
      in
        (match simplifyTy ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if tyEqv ctx tyT12 tyT11
             then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmTAbs (_, tyX, knK1, t2) ->
      let ctx = addBinding ctx tyX (TyVarBind knK1) in
      let tyT2 = typeOf ctx t2 in TyAll (tyX, knK1, tyT2)
  | TmTApp (fi, t1, tyT2) ->
      let knKT2 = kindOf ctx tyT2 in
      let tyT1 = typeOf ctx t1
      in
        (match simplifyTy ctx tyT1 with
         | TyAll (_, knK11, tyT12) ->
             (if knK11 <> knKT2
              then error fi "Type argument has wrong kind"
              else ();
              typeSubstTop tyT2 tyT12)
         | _ -> error fi "universal type expected")
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
  | TmPack (fi, tyT1, t2, tyT) ->
      (checkkindstar fi ctx tyT;
       (match simplifyTy ctx tyT with
        | TySome (_, k, tyT2) ->
            (if (kindOf ctx tyT1) <> k
             then error fi "type component does not have expected kind"
             else ();
             let tyU = typeOf ctx t2 in
             let tyU' = typeSubstTop tyT1 tyT2
             in
               if tyEqv ctx tyU tyU'
               then tyT
               else error fi "doesn't match declared type")
        | _ -> error fi "existential type expected"))
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let tyT1 = typeOf ctx t1
      in
        (match simplifyTy ctx tyT1 with
         | TySome (_, k, tyT11) ->
             let ctx' = addBinding ctx tyX (TyVarBind k) in
             let ctx'' = addBinding ctx' x (VarBind tyT11) in
             let tyT2 = typeOf ctx'' t2 in typeShift (-2) tyT2
         | _ -> error fi "existential type expected")
  
let evalBinding ctx store b =
  match b with
  | TmAbbBind (t, tyT) ->
      let (t', store') = eval ctx store t in ((TmAbbBind (t', tyT)), store')
  | bind -> (bind, store)
  

