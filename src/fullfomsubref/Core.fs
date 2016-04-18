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
  | TmFloat _ -> true
  | TmString _ -> true
  | TmUnit _ -> true
  | TmTag (_, _, t1, _) -> isval ctx t1
  | TmLoc (_) -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | TmPack (_, _, v1, _) when isval ctx v1 -> true
  | TmTAbs (_) -> true
  | _ -> false
  
exception ErrorEncounteredException
  
type Store = Term list

let emptystore = []
  
let extendstore store v = ((List.length store), (List.append store [ v ]))
  
let lookuploc store l = List.item l store
  
let updatestore store n v =
  let rec f s =
    match s with
    | (0, _ :: rest) -> v :: rest
    | (n, v' :: rest) -> v' :: (f ((n - 1), rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in f (n, store)
  
let shiftstore i store = List.map (fun t -> termShift i t) store
  
let rec eval1 ctx store t =
  match t with
  | TmIf (_, (TmTrue _), t2, _) -> (t2, store)
  | TmIf (_, (TmFalse _), _, t3) -> (t3, store)
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
        |Some (_, body) -> ((termSubstTop v11 body), store)
        | None -> raise Common.NoRuleAppliesException
  | TmCase (fi, t1, branches) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmCase (fi, t1', branches)), store')
  | TmAscribe (_, v1, _) when isval ctx v1 -> (v1, store)
  | TmAscribe (fi, t1, tyT) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmAscribe (fi, t1', tyT)), store')
  | TmVar (fi, n, _) ->
      (match getbinding fi ctx n with
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
  | TmError _ -> raise ErrorEncounteredException
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
  | TmTApp (_, (TmTAbs (_, _, _, t11)), tyT2) ->
      ((tytermSubstTop tyT2 t11), store)
  | TmTApp (fi, t1, tyT2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmTApp (fi, t1', tyT2)), store')
  | TmApp (_, (TmAbs (_, _, _, t12)), v2) when isval ctx v2 ->
      ((termSubstTop v2 t12), store)
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let (t2', store') = eval1 ctx store t2
      in ((TmApp (fi, v1, t2')), store')
  | TmApp (fi, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmApp (fi, t1', t2)), store')
  | TmIf (fi, t1, t2, t3) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmIf (fi, t1', t2, t3)), store')
  | TmUnpack (_, _, _, (TmPack (_, tyT11, v12, _)), t2) when isval ctx v12
      -> ((tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)), store)
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let (t1', store') = eval1 ctx store t1
      in ((TmUnpack (fi, tyX, x, t1', t2)), store')
  | TmPack (fi, tyT1, t2, tyT3) ->
      let (t2', store') = eval1 ctx store t2
      in ((TmPack (fi, tyT1, t2', tyT3)), store')
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx store t =
    try let (t', store') = eval1 ctx store t
        eval ctx store' t'
    with
    | Common.NoRuleAppliesException ->
        (t, store)
    | ErrorEncounteredException ->
        (TmError dummyinfo), store
  
(* ------------------------   KINDING  ------------------------ *)
let istyabb ctx i =
  match getbinding dummyinfo ctx i with
  | TyAbbBind (_) -> true
  | _ -> false
  
let gettyabb ctx i =
  match getbinding dummyinfo ctx i with
  | TyAbbBind (tyT, _) -> tyT
  | _ -> raise Common.NoRuleAppliesException
  
let rec computety ctx tyT =
  match tyT with
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
  | TyApp ((TyAbs (_, _, tyT12)), tyT2) -> typeSubstTop tyT2 tyT12
  | _ -> raise Common.NoRuleAppliesException
  
let rec simplifyty ctx tyT =
  let tyT =
    match tyT with
    | TyApp (tyT1, tyT2) -> TyApp (simplifyty ctx tyT1, tyT2)
    | tyT -> tyT
  in
    try let tyT' = computety ctx tyT in simplifyty ctx tyT'
    with | Common.NoRuleAppliesException -> tyT
  
let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT
  in
    match (tyS, tyT) with
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
        (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
    | (TyTop, TyTop) -> true
    | (TyId b1, TyId b2) -> b1 = b2
    | (TyString, TyString) -> true
    | (TyUnit, TyUnit) -> true
    | (TyFloat, TyFloat) -> true
    | (TyVar (i, _), _) when istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
    | (_, TyVar (i, _)) when istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
    | (TyVar (i, _), TyVar (j, _)) -> i = j
    | (TyBool, TyBool) -> true
    | (TyNat, TyNat) -> true
    | (TyRecord fields1, TyRecord fields2) ->
        ((List.length fields1) = (List.length fields2)) &&
          (List.forall
             (fun (li2, tyTi2) ->
                    match List.assoc li2 fields1 with
                    | Some tyTi1 -> tyeqv ctx tyTi1 tyTi2
                    | None -> false)
             fields2)
    | (TyVariant fields1, TyVariant fields2) ->
        ((List.length fields1) = (List.length fields2)) &&
          (List.forall
             (fun (li2, tyTi2) ->
                    match List.assoc li2 fields1 with
                    | Some tyTi1 -> tyeqv ctx tyTi1 tyTi2
                    | None -> false)
             fields2)
    | (TyAll (tyX1, tyS1, tyS2), TyAll (_, tyT1, tyT2)) ->
        let ctx1 = addname ctx tyX1
        in (tyeqv ctx tyS1 tyT1) && (tyeqv ctx1 tyS2 tyT2)
    | (TySome (tyX1, tyS1, tyS2), TySome (_, tyT1, tyT2)) ->
        let ctx1 = addname ctx tyX1
        in (tyeqv ctx tyS1 tyT1) && (tyeqv ctx1 tyS2 tyT2)
    | (TyAbs (tyX1, knKS1, tyS2), TyAbs (_, knKT1, tyT2)) ->
        (knKS1 = knKT1) &&
          (let ctx = addname ctx tyX1 in tyeqv ctx tyS2 tyT2)
    | (TyApp (tyS1, tyS2), TyApp (tyT1, tyT2)) ->
        (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
    | (TyRef tyS, TyRef tyT) -> tyeqv ctx tyS tyT
    | (TySource tyS, TySource tyT) -> tyeqv ctx tyS tyT
    | (TySink tyS, TySink tyT) -> tyeqv ctx tyS tyT
    | _ -> false
  
let rec getkind fi ctx i =
  match getbinding fi ctx i with
  | TyVarBind tyT -> kindof ctx tyT
  | TyAbbBind (_, (Some knK)) -> knK
  | TyAbbBind (_, None) ->
      error fi ("No kind recorded for variable " ^ (index2name fi ctx i))
  | _ ->
      error fi
        ("getkind: Wrong kind of binding for variable " ^
           (index2name fi ctx i))
and kindof ctx tyT =
  match tyT with
  | TyRecord fldtys ->
      (List.iter
         (fun (_, tyS) ->
            if (kindof ctx tyS) <> KnStar
            then error dummyinfo "Kind * expected"
            else ())
         fldtys;
       KnStar)
  | TyVariant fldtys ->
      (List.iter
         (fun (_, tyS) ->
            if (kindof ctx tyS) <> KnStar
            then error dummyinfo "Kind * expected"
            else ())
         fldtys;
       KnStar)
  | TyVar (i, _) -> let knK = getkind dummyinfo ctx i in knK
  | TyAll (tyX, tyT1, tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind tyT1)
      in
        (if (kindof ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | TyAbs (tyX, knK1, tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind (maketop knK1)) in
      let knK2 = kindof ctx' tyT2 in KnArr (knK1, knK2)
  | TyApp (tyT1, tyT2) ->
      let knK1 = kindof ctx tyT1 in
      let knK2 = kindof ctx tyT2
      in
        (match knK1 with
         | KnArr (knK11, knK12) ->
             if knK2 = knK11
             then knK12
             else error dummyinfo "parameter kind mismatch"
         | _ -> error dummyinfo "arrow kind expected")
  | TySome (tyX, tyT1, tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind tyT1)
      in
        (if (kindof ctx' tyT2) <> KnStar
         then error dummyinfo "Kind * expected"
         else ();
         KnStar)
  | TyArr (tyT1, tyT2) ->
      (if (kindof ctx tyT1) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       if (kindof ctx tyT2) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | TyRef tyT ->
      (if (kindof ctx tyT) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | TySource tyT ->
      (if (kindof ctx tyT) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | TySink tyT ->
      (if (kindof ctx tyT) <> KnStar
       then error dummyinfo "star kind expected"
       else ();
       KnStar)
  | _ -> KnStar
  
let checkkindstar fi ctx tyT =
  let k = kindof ctx tyT
  in if k = KnStar then () else error fi "Kind * expected"
  
(* ------------------------   SUBTYPING  ------------------------ *)
let rec promote ctx t =
  match t with
  | TyVar (i, _) ->
      (match getbinding dummyinfo ctx i with
       | TyVarBind tyT -> tyT
       | _ -> raise Common.NoRuleAppliesException)
  | TyApp (tyS, tyT) -> TyApp (promote ctx tyS, tyT)
  | _ -> raise Common.NoRuleAppliesException
  
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
       | (TyRecord fS, TyRecord fT) ->
           List.forall
             (fun (li, tyTi) ->
                 match List.assoc li fS with
                 | Some tySi -> subtype ctx tySi tyTi
                 | None -> false)
             fT
       | (TyVariant fS, TyVariant fT) ->
           List.forall
             (fun (li, tySi) ->
                match List.assoc li fT with
                | Some tyTi -> subtype ctx tySi tyTi
                | None -> false)
             fS
       | (TyVar (_), _) -> subtype ctx (promote ctx tyS) tyT
       | (TyAll (tyX1, tyS1, tyS2), TyAll (_, tyT1, tyT2)) ->
           ((subtype ctx tyS1 tyT1) && (subtype ctx tyT1 tyS1)) &&
             (let ctx1 = addbinding ctx tyX1 (TyVarBind tyT1)
              in subtype ctx1 tyS2 tyT2)
       | (TyAbs (tyX, knKS1, tyS2), TyAbs (_, knKT1, tyT2)) ->
           (knKS1 = knKT1) &&
             (let ctx = addbinding ctx tyX (TyVarBind (maketop knKS1))
              in subtype ctx tyS2 tyT2)
       | (TyApp (_), _) -> subtype ctx (promote ctx tyS) tyT
       | (TySome (tyX1, tyS1, tyS2), TySome (_, tyT1, tyT2)) ->
           ((subtype ctx tyS1 tyT1) && (subtype ctx tyT1 tyS1)) &&
             (let ctx1 = addbinding ctx tyX1 (TyVarBind tyT1)
              in subtype ctx1 tyS2 tyT2)
       | (TyRef tyT1, TyRef tyT2) ->
           (subtype ctx tyT1 tyT2) && (subtype ctx tyT2 tyT1)
       | (TyRef tyT1, TySource tyT2) -> subtype ctx tyT1 tyT2
       | (TySource tyT1, TySource tyT2) -> subtype ctx tyT1 tyT2
       | (TyRef tyT1, TySink tyT2) -> subtype ctx tyT2 tyT1
       | (TySink tyT1, TySink tyT2) -> subtype ctx tyT2 tyT1
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
                    | Some tyTi -> (li, (join ctx tySi tyTi))
                    | None -> raise Common.NotFoundException)
                 commonLabels
             in TyRecord commonFields
         | (TyAll (tyX, tyS1, _), TyAll (_, tyT1, tyT2)) ->
             if not ((subtype ctx tyS1 tyT1) && (subtype ctx tyT1 tyS1))
             then TyTop
             else
               (let ctx' = addbinding ctx tyX (TyVarBind tyT1)
                in TyAll (tyX, tyS1, join ctx' tyT1 tyT2))
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             TyArr (meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
         | (TyRef tyT1, TyRef tyT2) ->
             if (subtype ctx tyT1 tyT2) && (subtype ctx tyT2 tyT1)
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
                       | Some tyTi -> (li, (meet ctx tySi tyTi))
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
         | (TyAll (tyX, tyS1, _), TyAll (_, tyT1, tyT2)) ->
             if not ((subtype ctx tyS1 tyT1) && (subtype ctx tyT1 tyS1))
             then raise Common.NotFoundException
             else
               (let ctx' = addbinding ctx tyX (TyVarBind tyT1)
                in TyAll (tyX, tyS1, meet ctx' tyT1 tyT2))
         | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
             TyArr (join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
         | (TyRef tyT1, TyRef tyT2) ->
             if (subtype ctx tyT1 tyT2) && (subtype ctx tyT2 tyT1)
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
let rec lcst ctx tyS =
  let tyS = simplifyty ctx tyS
  in try lcst ctx (promote ctx tyS) with | Common.NoRuleAppliesException -> tyS
  
let rec typeof ctx t =
  match t with
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (fi, x, tyT1, t2) ->
      (checkkindstar fi ctx tyT1;
       let ctx' = addbinding ctx x (VarBind tyT1) in
       let tyT2 = typeof ctx' t2 in TyArr (tyT1, typeShift (-1) tyT2))
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2
      in
        (match lcst ctx tyT1 with
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
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, (typeof ctx ti))) fields
      in TyRecord fieldtys
  | TmProj (fi, t1, l) ->
      (match lcst ctx (typeof ctx t1) with
       | TyRecord fieldtys ->
            match List.assoc l fieldtys with
            | Some x -> x
            | None -> error fi ("label " ^ (l ^ " not found"))
       | TyBot -> TyBot
       | _ -> error fi "Expected record type")
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x (VarBind tyT1)
      in typeShift (-1) (typeof ctx' t2)
  | TmFloat _ -> TyFloat
  | TmTimesfloat (fi, t1, t2) ->
      if
        (subtype ctx (typeof ctx t1) TyFloat) &&
          (subtype ctx (typeof ctx t2) TyFloat)
      then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmCase (fi, t, cases) ->
      (match lcst ctx (typeof ctx t) with
       | TyVariant fieldtys ->
           (List.iter
              (fun (li, (_)) ->
                match List.assoc li fieldtys with
                | Some _ -> ()
                | None -> error fi ("label " ^ (li ^ " not in type")))
              cases;
            let casetypes =
              List.map
                (fun (li, (xi, ti)) ->
                    match List.assoc li fieldtys with
                    | Some tyTi ->
                       let ctx' = addbinding ctx xi (VarBind tyTi)
                       typeShift (-1) (typeof ctx' ti)
                    | None -> error fi ("label " ^ (li ^ " not found")))
                cases
            in List.fold (join ctx) TyBot casetypes)
       | TyBot -> TyBot
       | _ -> error fi "Expected variant type")
  | TmTag (fi, li, ti, tyT) ->
      (match simplifyty ctx tyT with
       | TyVariant fieldtys ->
            match List.assoc li fieldtys with
            | Some tyTiExpected ->
                let tyTi = typeof ctx ti
                if subtype ctx tyTi tyTiExpected
                then tyT
                else error fi "field does not have expected type"
            | None -> error fi ("label " ^ (li ^ " not found"))
       | _ -> error fi "Annotation is not a variant type")
  | TmAscribe (fi, t1, tyT) ->
      (checkkindstar fi ctx tyT;
       if subtype ctx (typeof ctx t1) tyT
       then tyT
       else error fi "body of as-term does not have the expected type")
  | TmString _ -> TyString
  | TmUnit _ -> TyUnit
  | TmInert (_, tyT) -> tyT
  | TmFix (fi, t1) ->
      let tyT1 = typeof ctx t1
      in
        (match lcst ctx tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subtype ctx tyT12 tyT11
             then tyT12
             else error fi "result of body not compatible with domain"
         | TyBot -> TyBot
         | _ -> error fi "arrow type expected")
  | TmRef (_, t1) -> TyRef (typeof ctx t1)
  | TmLoc (fi, _) ->
      error fi "locations are not supposed to occur in source programs!"
  | TmDeref (fi, t1) ->
      (match lcst ctx (typeof ctx t1) with
       | TyRef tyT1 -> tyT1
       | TyBot -> TyBot
       | TySource tyT1 -> tyT1
       | _ -> error fi "argument of ! is not a Ref or Source")
  | TmAssign (fi, t1, t2) ->
      (match lcst ctx (typeof ctx t1) with
       | TyRef tyT1 ->
           if subtype ctx (typeof ctx t2) tyT1
           then TyUnit
           else error fi "arguments of := are incompatible"
       | TyBot -> let _ = typeof ctx t2 in TyBot
       | TySink tyT1 ->
           if subtype ctx (typeof ctx t2) tyT1
           then TyUnit
           else error fi "arguments of := are incompatible"
       | _ -> error fi "argument of ! is not a Ref or Sink")
  | TmError _ -> TyBot
  | TmTAbs (_, tyX, tyT1, t2) ->
      let ctx = addbinding ctx tyX (TyVarBind tyT1) in
      let tyT2 = typeof ctx t2 in TyAll (tyX, tyT1, tyT2)
  | TmTApp (fi, t1, tyT2) ->
      let tyT1 = typeof ctx t1
      in
        (match lcst ctx tyT1 with
         | TyAll (_, tyT11, tyT12) ->
             (if not (subtype ctx tyT2 tyT11)
              then error fi "type parameter type mismatch"
              else ();
              typeSubstTop tyT2 tyT12)
         | _ -> error fi "universal type expected")
  | TmTry (_, t1, t2) -> join ctx (typeof ctx t1) (typeof ctx t2)
  | TmZero _ -> TyNat
  | TmSucc (fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat
      then TyNat
      else error fi "argument of succ is not a number"
  | TmPred (fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat
      then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero (fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat
      then TyBool
      else error fi "argument of iszero is not a number"
  | TmPack (fi, tyT1, t2, tyT) ->
      (checkkindstar fi ctx tyT;
       (match simplifyty ctx tyT with
        | TySome (_, tyBound, tyT2) ->
            (if not (subtype ctx tyT1 tyBound)
             then error fi "hidden type not a subtype of bound"
             else ();
             let tyU = typeof ctx t2 in
             let tyU' = typeSubstTop tyT1 tyT2
             in
               if subtype ctx tyU tyU'
               then tyT
               else error fi "doesn't match declared type")
        | _ -> error fi "existential type expected"))
  | TmUnpack (fi, tyX, x, t1, t2) ->
      let tyT1 = typeof ctx t1
      in
        (match lcst ctx tyT1 with
         | TySome (_, tyBound, tyT11) ->
             let ctx' = addbinding ctx tyX (TyVarBind tyBound) in
             let ctx'' = addbinding ctx' x (VarBind tyT11) in
             let tyT2 = typeof ctx'' t2 in typeShift (-2) tyT2
         | _ -> error fi "existential type expected")
  
let evalbinding ctx store b =
  match b with
  | TmAbbBind (t, tyT) ->
      let (t', store') = eval ctx store t in ((TmAbbBind (t', tyT)), store')
  | bind -> (bind, store)
  

