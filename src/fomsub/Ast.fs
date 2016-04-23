﻿(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Syntax trees and associated support functions.
module Ast

open FSharpTapl.Compatability

(* ---------------------------------------------------------------------- *)
(* Datatypes *)
type Kind = | KnStar | KnArr of Kind * Kind

type Ty =
  | TyTop
  | TyVar of int * int
  | TyArr of Ty * Ty
  | TyAll of string * Ty * Ty
  | TyAbs of string * Kind * Ty
  | TyApp of Ty * Ty

type Term =
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmTAbs of Info * string * Ty * Term
  | TmTApp of Info * Term * Ty

type Binding = | NameBind | VarBind of Ty | TyVarBind of Ty

type Context = (string * Binding) list

type Command = | Eval of Info * Term | Bind of Info * string * Binding

(* ---------------------------------------------------------------------- *)
(* Context management *)
let emptycontext = []
  
let ctxlength ctx = List.length ctx
  
let addbinding ctx x bind = (x, bind) :: ctx
  
let addname ctx x = addbinding ctx x NameBind
  
let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isnamebound rest x
  
let rec pickfreshname ctx x =
  if isnamebound ctx x
  then pickfreshname ctx (x ^ "'")
  else (((x, NameBind) :: ctx), x)
  
let index2name fi ctx x =
  try let (xn, _) = List.item x ctx in xn
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg x (List.length ctx))
  
let rec name2index fi ctx x =
  match ctx with
  | [] -> error fi ("Identifier " ^ (x ^ " is unbound"))
  | (y, _) :: rest -> if y = x then 0 else 1 + (name2index fi rest x)
  
(* ---------------------------------------------------------------------- *)
(* Shifting *)
let tymap onvar c tyT =
  let rec walk c tyT =
    match tyT with
    | TyArr (tyT1, tyT2) -> TyArr (walk c tyT1, walk c tyT2)
    | TyTop -> TyTop
    | TyVar (x, n) -> onvar c x n
    | TyAll (tyX, tyT1, tyT2) -> TyAll (tyX, walk c tyT1, walk (c + 1) tyT2)
    | TyAbs (tyX, knK1, tyT2) -> TyAbs (tyX, knK1, walk (c + 1) tyT2)
    | TyApp (tyT1, tyT2) -> TyApp (walk c tyT1, walk c tyT2)
  in walk c tyT
  
let tmmap onvar ontype c t =
  let rec walk c t =
    match t with
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, tyT1, t2) ->
        TmAbs (fi, x, ontype c tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmTAbs (fi, tyX, tyT1, t2) ->
        TmTAbs (fi, tyX, ontype c tyT1, walk (c + 1) t2)
    | TmTApp (fi, t1, tyT2) -> TmTApp (fi, walk c t1, ontype c tyT2)
  in walk c t
  
let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x >= c then TyVar (x + d, n + d) else TyVar (x, n + d))
    c tyT
  
let termShiftAbove d c t =
  tmmap
    (fun fi c x n ->
       if x >= c then TmVar (fi, x + d, n + d) else TmVar (fi, x, n + d))
    (typeShiftAbove d) c t
  
let termShift d t = termShiftAbove d 0 t
  
let typeShift d tyT = typeShiftAbove d 0 tyT
  
let bindingshift d bind =
  match bind with
  | NameBind -> NameBind
  | VarBind tyT -> VarBind (typeShift d tyT)
  | TyVarBind tyS -> TyVarBind (typeShift d tyS)
  
(* ---------------------------------------------------------------------- *)
(* Substitution *)
let termSubst j s t =
  tmmap (fun fi j x n -> if x = j then termShift j s else TmVar (fi, x, n))
    (fun _ tyT -> tyT) j t
  
let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
  
let typeSubst tyS j tyT =
  tymap (fun j x n -> if x = j then typeShift j tyS else TyVar (x, n)) j tyT
  
let typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)
  
let rec tytermSubst tyS j t =
  tmmap (fun fi _ x n -> TmVar (fi, x, n)) (fun j tyT -> typeSubst tyS j tyT)
    j t
  
let tytermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
  
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)
let rec getbinding fi ctx i =
  try let (_, bind) = List.item i ctx in bindingshift (i + 1) bind
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg i (List.length ctx))
  
let getTypeFromContext fi ctx i =
  match getbinding fi ctx i with
  | VarBind tyT -> tyT
  | _ ->
      error fi
        ("getTypeFromContext: Wrong kind of binding for variable " ^
           (index2name fi ctx i))
  
let rec maketop k =
  match k with
  | KnStar -> TyTop
  | KnArr (knK1, knK2) -> TyAbs ("_", knK1, maketop knK2)
  
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
  match t with
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmTAbs (fi, _, _, _) -> fi
  | TmTApp (fi, _, _) -> fi
  
(* ---------------------------------------------------------------------- *)
(* Printing *)
(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)
let obox0 () = open_hvbox 0
let obox () = open_hvbox 2
let cbox () = close_box()
let ``break`` () = print_break 0 0
  
let small t = match t with | TmVar (_) -> true | _ -> false
  
let rec printknKind outer ctx k =
  match k with | knK -> printknArrowKind outer ctx knK
and printknArrowKind outer ctx k =
  match k with
  | KnArr (knK1, knK2) ->
      (obox0 ();
       printknAKind false ctx knK1;
       if outer then pr " " else ();
       pr "=>";
       if outer then print_space () else ``break`` ();
       printknArrowKind outer ctx knK2;
       cbox ())
  | knK -> printknAKind outer ctx knK
and printknAKind outer ctx k =
  match k with
  | KnStar -> pr "*"
  | knK -> (pr "("; printknKind outer ctx knK; pr ")")
  
let printkn ctx knK = printknKind true ctx knK
  
let prokn ctx knK =
  if knK <> KnStar then (pr "::"; printknKind false ctx knK) else ()
  
let rec printtyType outer ctx tyT =
  match tyT with
  | TyAll (tyX, tyT1, tyT2) ->
      let (ctx1, tyX) = pickfreshname ctx tyX
      in
        (obox ();
         pr "All ";
         pr tyX;
         proty ctx tyT1;
         pr ".";
         print_space ();
         printtyType outer ctx1 tyT2;
         cbox ())
  | TyAbs (tyX, knK1, tyT2) ->
      let (ctx', x') = pickfreshname ctx tyX
      in
        (obox ();
         pr "lambda ";
         pr x';
         prokn ctx knK1;
         pr ".";
         if outer then print_space () else ``break`` ();
         printtyType outer ctx' tyT2;
         cbox ())
  | tyT -> printtyArrowType outer ctx tyT
and printtyArrowType outer ctx tyT =
  match tyT with
  | TyArr (tyT1, tyT2) ->
      (obox0 ();
       printtyAppType false ctx tyT1;
       if outer then pr " " else ();
       pr "->";
       if outer then print_space () else ``break`` ();
       printtyArrowType outer ctx tyT2;
       cbox ())
  | tyT -> printtyAppType outer ctx tyT
and proty ctx tyS =
  if tyS <> TyTop then (pr "<:"; printtyType false ctx tyS) else ()
and printtyAppType outer ctx k =
  match k with
  | TyApp (tyT1, tyT2) ->
      (obox0 ();
       printtyAppType false ctx tyT1;
       print_space ();
       printtyAType false ctx tyT2;
       cbox ())
  | tyT -> printtyAType outer ctx tyT
and printtyAType outer ctx tyT =
  match tyT with
  | TyTop -> pr "Top"
  | TyVar (x, n) ->
      if (ctxlength ctx) = n
      then pr (index2name dummyinfo ctx x)
      else
        pr
          ("[bad index: " ^
             ((string x) ^
                ("/" ^
                   ((string n) ^
                      (" in {" ^
                         ((List.fold (fun s (x, _) -> s ^ (" " ^ x)) ""
                             ctx)
                            ^ " }]"))))))
  | tyT -> (pr "("; printtyType outer ctx tyT; pr ")")
  
let printty ctx tyT = printtyType true ctx tyT
  
let rec printtmTerm outer ctx t =
  match t with
  | TmAbs (_, x, tyT1, t2) ->
      let (ctx', x') = pickfreshname ctx x
      in
        (obox ();
         pr "lambda ";
         pr x';
         pr ":";
         printtyType false ctx tyT1;
         pr ".";
         if (small t2) && (not outer) then ``break`` () else print_space ();
         printtmTerm outer ctx' t2;
         cbox ())
  | TmTAbs (_, x, tyS, t) ->
      let (ctx1, x) = pickfreshname ctx x
      in
        (obox ();
         pr "lambda ";
         pr x;
         proty ctx tyS;
         pr ".";
         if (small t) && (not outer) then ``break`` () else print_space ();
         printtmTerm outer ctx1 t;
         cbox ())
  | t -> printtmAppTerm outer ctx t
and printtmAppTerm outer ctx t =
  match t with
  | TmApp (_, t1, t2) ->
      (obox0 ();
       printtmAppTerm false ctx t1;
       print_space ();
       printtmATerm false ctx t2;
       cbox ())
  | TmTApp (_, t, tyS) ->
      (obox0 ();
       printtmAppTerm false ctx t;
       print_space ();
       pr "[";
       printtyType false ctx tyS;
       pr "]";
       cbox ())
  | t -> printtmATerm outer ctx t
and printtmATerm outer ctx t =
  match t with
  | TmVar (fi, x, n) ->
      if (ctxlength ctx) = n
      then pr (index2name fi ctx x)
      else
        pr
          ("[bad index: " ^
             ((string x) ^
                ("/" ^
                   ((string n) ^
                      (" in {" ^
                         ((List.fold (fun s (x, _) -> s ^ (" " ^ x)) ""
                             ctx)
                            ^ " }]"))))))
  | t -> (pr "("; printtmTerm outer ctx t; pr ")")
  
let printtm ctx t = printtmTerm true ctx t
  
let prbinding ctx b =
  match b with
  | NameBind -> ()
  | VarBind tyT -> (pr ": "; printty ctx tyT)
  | TyVarBind tyS -> (pr "<: "; printtyType false ctx tyS)
  

