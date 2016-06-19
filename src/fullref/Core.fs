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
  | TmTag (_, _, t1, _) -> isval ctx t1
  | TmString _ -> true
  | TmUnit _ -> true
  | TmLoc (_) -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | _ -> false
  
type Store = Term list

let emptyStore : Term list  = []
  
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
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      ((termSubstTop v2 t12), store)
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let (t2', store') = eval1 ctx store t2
      in ((TmApp (fi, v1, t2')), store')
  | TmApp (fi, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmApp (fi, t1', t2)), store')
  | TmIf (_, (TmTrue _), t2, _) -> (t2, store)
  | TmIf (_, (TmFalse _), _, t3) -> (t3, store)
  | TmIf (fi, t1, t2, t3) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmIf (fi, t1', t2, t3)), store')
  | TmLet (_, _, v1, t2) when isval ctx v1 -> ((termSubstTop v1 t2), store)
  | TmLet (fi, x, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmLet (fi, x, t1', t2)), store')
  | (TmFix (_, v1) as t) when isval ctx v1 ->
      (match v1 with
       | TmAbs (_, _, _, t12) -> ((termSubstTop t t12), store)
       | _ -> raise Common.NoRuleAppliesException)
  | TmFix (fi, t1) ->
      let (t1', store') = eval1 ctx store t1 in ((TmFix (fi, t1')), store')
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
  | TmTag (fi, l, t1, tyT) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmTag (fi, l, t1', tyT)), store')
  | TmCase (_, (TmTag (_, li, v11, _)), branches) when isval ctx v11 ->
        match List.assoc li branches with
        | Some (_, body) -> ((termSubstTop v11 body), store)
        | None -> raise Common.NotFoundException
  | TmCase (fi, t1, branches) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmCase (fi, t1', branches)), store')
  | TmAscribe (_, v1, _) when isval ctx v1 -> (v1, store)
  | TmAscribe (fi, t1, tyT) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmAscribe (fi, t1', tyT)), store')
  | TmVar (fi, n, _) ->
      (match getBinding fi ctx n with
       | TmAbbBind (t, _) -> (t, store)
       | _ -> raise Common.NoRuleAppliesException)
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
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval (ctx : Context) store t =
  try let (t', store') = eval1 ctx store t in eval ctx store' t'
  with | Common.NoRuleAppliesException -> (t, store)
  
(* ------------------------   SUBTYPING  ------------------------ *)
let evalBinding (ctx : Context) store b =
  match b with
  | TmAbbBind (t, tyT) ->
      let (t', store') = eval ctx store t in ((TmAbbBind (t', tyT)), store')
  | bind -> (bind, store)
  
let istyabb ctx i =
  match getBinding dummyinfo ctx i with | TyAbbBind _ -> true | _ -> false
  
let gettyabb ctx i =
  match getBinding dummyinfo ctx i with
  | TyAbbBind tyT -> tyT
  | _ -> raise Common.NoRuleAppliesException
  
let rec computety ctx tyT =
  match tyT with
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise Common.NoRuleAppliesException
  
let rec simplifyTy ctx tyT =
  try let tyT' = computety ctx tyT in simplifyTy ctx tyT'
  with | Common.NoRuleAppliesException -> tyT
  
let rec tyEqv ctx tyS tyT =
  let tyS = simplifyTy ctx tyS in
  let tyT = simplifyTy ctx tyT
  in
    match (tyS, tyT) with
    | (TyTop, TyTop) -> true
    | (TyBot, TyBot) -> true
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
        (tyEqv ctx tyS1 tyT1) && (tyEqv ctx tyS2 tyT2)
    | (TyString, TyString) -> true
    | (TyId b1, TyId b2) -> b1 = b2
    | (TyFloat, TyFloat) -> true
    | (TyUnit, TyUnit) -> true
    | (TyRef tyT1, TyRef tyT2) -> tyEqv ctx tyT1 tyT2
    | (TySource tyT1, TySource tyT2) -> tyEqv ctx tyT1 tyT2
    | (TySink tyT1, TySink tyT2) -> tyEqv ctx tyT1 tyT2
    | (TyVar (i, _), _) when istyabb ctx i -> tyEqv ctx (gettyabb ctx i) tyT
    | (_, TyVar (i, _)) when istyabb ctx i -> tyEqv ctx tyS (gettyabb ctx i)
    | (TyVar (i, _), TyVar (j, _)) -> i = j
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
    | (TyVariant fields1, TyVariant fields2) ->
        ((List.length fields1) = (List.length fields2)) &&
          (List.forall2
             (fun (li1, tyTi1) (li2, tyTi2) ->
                (li1 = li2) && (tyEqv ctx tyTi1 tyTi2))
             fields1 fields2)
    | _ -> false
  
let rec subType (ctx : Context) tyS tyT =
  (tyEqv ctx tyS tyT) ||
    (let tyS = simplifyTy ctx tyS in
     let tyT = simplifyTy ctx tyT
     in
       match (tyS, tyT) with
       | (_, TyTop) -> true
       | (TyBot, _) -> true
       | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
           (subType ctx tyT1 tyS1) && (subType ctx tyS2 tyT2)
       | (TyRecord fS, TyRecord fT) ->
           List.forall
             (fun (li, tyTi) ->
                match List.assoc li fS with
                | Some tySi -> subType ctx tySi tyTi
                | None -> false)
             fT
       | (TyVariant fS, TyVariant fT) ->
           List.forall
             (fun (li, tySi) ->
                match List.assoc li fT with
                | Some tyTi -> subType ctx tySi tyTi
                | None -> false)
             fS
       | (TyRef tyT1, TyRef tyT2) ->
           (subType ctx tyT1 tyT2) && (subType ctx tyT2 tyT1)
       | (TyRef tyT1, TySource tyT2) -> subType ctx tyT1 tyT2
       | (TySource tyT1, TySource tyT2) -> subType ctx tyT1 tyT2
       | (TyRef tyT1, TySink tyT2) -> subType ctx tyT2 tyT1
       | (TySink tyT1, TySink tyT2) -> subType ctx tyT2 tyT1
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
                    let tySi = 
                        match List.assoc li fS with
                        | Some x -> x
                        | None -> raise Common.NotFoundException
                    match List.assoc li fT with
                    | Some tyTi ->  (li, (join ctx tySi tyTi))
                    | None -> raise Common.NotFoundException)
                 commonLabels
             in TyRecord commonFields
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             TyArr (meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
         | (TyRef tyT1, TyRef tyT2) ->
             if (subType ctx tyT1 tyT2) && (subType ctx tyT2 tyT1)
             then TyRef tyT1
             else (* Warning: this is incomplete... *)
               TySource (join ctx tyT1 tyT2)
         | (TySource tyT1, TySource tyT2) -> TySource (join ctx tyT1 tyT2)
         | (TyRef tyT1, TySource tyT2) -> TySource (join ctx tyT1 tyT2)
         | (TySource tyT1, TyRef tyT2) -> TySource (join ctx tyT1 tyT2)
         | (TySink tyT1, TySink tyT2) -> TySink (meet ctx tyT1 tyT2)
         | (TyRef tyT1, TySink tyT2) -> TySink (meet ctx tyT1 tyT2)
         | (TySink tyT1, TyRef tyT2) -> TySink (meet ctx tyT1 tyT2)
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
                    if List.exists (fun x -> x = li) allLabels
                    then
                      (let tySi = 
                        match List.assoc li fS with
                        | Some x -> x
                        | None -> raise Common.NotFoundException
                       match List.assoc li fT with
                       | Some tyTi ->
                            (li, (meet ctx tySi tyTi))
                       | None -> raise Common.NotFoundException)
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
         | (TyRef tyT1, TyRef tyT2) ->
             if (subType ctx tyT1 tyT2) && (subType ctx tyT2 tyT1)
             then TyRef tyT1
             else (* Warning: this is incomplete... *)
               TySource (meet ctx tyT1 tyT2)
         | (TySource tyT1, TySource tyT2) -> TySource (meet ctx tyT1 tyT2)
         | (TyRef tyT1, TySource tyT2) -> TySource (meet ctx tyT1 tyT2)
         | (TySource tyT1, TyRef tyT2) -> TySource (meet ctx tyT1 tyT2)
         | (TySink tyT1, TySink tyT2) -> TySink (join ctx tyT1 tyT2)
         | (TyRef tyT1, TySink tyT2) -> TySink (join ctx tyT1 tyT2)
         | (TySink tyT1, TyRef tyT2) -> TySink (join ctx tyT1 tyT2)
         | _ -> TyBot)
  
(* ------------------------   TYPING  ------------------------ *)
let rec typeOf ctx t =
  match t with
  | TmInert (_, tyT) -> tyT
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addBinding ctx x (VarBind tyT1) in
      let tyT2 = typeOf ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let tyT2 = typeOf ctx t2
      in
        (match simplifyTy ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subType ctx tyT2 tyT11
             then tyT12
             else error fi "parameter type mismatch"
         | TyBot -> TyBot
         | _ -> error fi "arrow type expected")
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if subType ctx (typeOf ctx t1) TyBool
      then join ctx (typeOf ctx t2) (typeOf ctx t3)
      else error fi "guard of conditional not a boolean"
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeOf ctx t1 in
      let ctx' = addBinding ctx x (VarBind tyT1)
      in typeShift (-1) (typeOf ctx' t2)
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, (typeOf ctx ti))) fields
      in TyRecord fieldtys
  | TmProj (fi, t1, l) ->
      (match simplifyTy ctx (typeOf ctx t1) with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some x -> x
            | None -> error fi ("label " + (l + " not found"))
       | TyBot -> TyBot
       | _ -> error fi "Expected record type")
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
                    let ctx' =
                        match List.assoc li fieldtys with
                        | Some tyTi -> addBinding ctx xi (VarBind tyTi)
                        | None -> error fi ("label " + (li + " not found"))
                    in typeShift (-1) (typeOf ctx' ti))
                cases
            in List.fold (join ctx) TyBot casetypes)
       | TyBot -> TyBot
       | _ -> error fi "Expected variant type")
  | TmFix (fi, t1) ->
      let tyT1 = typeOf ctx t1
      in
        (match simplifyTy ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subType ctx tyT12 tyT11
             then tyT12
             else error fi "result of body not compatible with domain"
         | TyBot -> TyBot
         | _ -> error fi "arrow type expected")
  | TmTag (fi, li, ti, tyT) ->
      (match simplifyTy ctx tyT with
       | TyVariant fieldtys ->
           
            match List.assoc li fieldtys with
            | Some tyTiExpected  -> 
                let tyTi = typeOf ctx ti

                if subType ctx tyTi tyTiExpected
                then tyT
                else error fi "field does not have expected type"
            | None -> error fi ("label " + (li + " not found"))
       | _ -> error fi "Annotation is not a variant type")
  | TmAscribe (fi, t1, tyT) ->
      if subType ctx (typeOf ctx t1) tyT
      then tyT
      else error fi "body of as-term does not have the expected type"
  | TmString _ -> TyString
  | TmUnit _ -> TyUnit
  | TmRef (_, t1) -> TyRef (typeOf ctx t1)
  | TmLoc (fi, _) ->
      error fi "locations are not supposed to occur in source programs!"
  | TmDeref (fi, t1) ->
      (match simplifyTy ctx (typeOf ctx t1) with
       | TyRef tyT1 -> tyT1
       | TyBot -> TyBot
       | TySource tyT1 -> tyT1
       | _ -> error fi "argument of ! is not a Ref or Source")
  | TmAssign (fi, t1, t2) ->
      (match simplifyTy ctx (typeOf ctx t1) with
       | TyRef tyT1 ->
           if subType ctx (typeOf ctx t2) tyT1
           then TyUnit
           else error fi "arguments of := are incompatible"
       | TyBot -> 
                typeOf ctx t2 |> ignore
                TyBot
       | TySink tyT1 ->
           if subType ctx (typeOf ctx t2) tyT1
           then TyUnit
           else error fi "arguments of := are incompatible"
       | _ -> error fi "argument of ! is not a Ref or Sink")
  | TmFloat _ -> TyFloat
  | TmTimesfloat (fi, t1, t2) ->
      if
        (subType ctx (typeOf ctx t1) TyFloat) &&
          (subType ctx (typeOf ctx t2) TyFloat)
      then TyFloat
      else error fi "argument of timesfloat is not a number"
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
  

