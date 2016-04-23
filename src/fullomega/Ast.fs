(*
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
  | TyVar of int * int
  | TyId of string
  | TyArr of Ty * Ty
  | TyRecord of (string * Ty) list
  | TyRef of Ty
  | TyString
  | TyUnit
  | TyBool
  | TyFloat
  | TyAll of string * Kind * Ty
  | TyNat
  | TySome of string * Kind * Ty
  | TyAbs of string * Kind * Ty
  | TyApp of Ty * Ty

type Term =
  | TmAscribe of Info * Term * Ty
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmRecord of Info * (string * Term) list
  | TmProj of Info * Term * string
  | TmString of Info * string
  | TmUnit of Info
  | TmLoc of Info * int
  | TmRef of Info * Term
  | TmDeref of Info * Term
  | TmAssign of Info * Term * Term
  | TmFloat of Info * float
  | TmTimesfloat of Info * Term * Term
  | TmLet of Info * string * Term * Term
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term
  | TmZero of Info
  | TmSucc of Info * Term
  | TmPred of Info * Term
  | TmIsZero of Info * Term
  | TmInert of Info * Ty
  | TmFix of Info * Term
  | TmTAbs of Info * string * Kind * Term
  | TmTApp of Info * Term * Ty
  | TmPack of Info * Ty * Term * Ty
  | TmUnpack of Info * string * string * Term * Term

type Binding =
  | NameBind
  | TyVarBind of Kind
  | VarBind of Ty
  | TyAbbBind of Ty * Kind option
  | TmAbbBind of Term * Ty option

type Context = (string * Binding) list

type Command =
  | Eval of Info * Term
  | Bind of Info * string * Binding
  | SomeBind of Info * string * string * Term

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
    | TyVar (x, n) -> onvar c x n
    | (TyId _ as tyT) -> tyT
    | TyString -> TyString
    | TyUnit -> TyUnit
    | TyRef tyT1 -> TyRef (walk c tyT1)
    | TyRecord fieldtys ->
        TyRecord (List.map (fun (li, tyTi) -> (li, (walk c tyTi))) fieldtys)
    | TyFloat -> TyFloat
    | TyAll (tyX, knK1, tyT2) -> TyAll (tyX, knK1, walk (c + 1) tyT2)
    | TyBool -> TyBool
    | TyNat -> TyNat
    | TyArr (tyT1, tyT2) -> TyArr (walk c tyT1, walk c tyT2)
    | TySome (tyX, knK1, tyT2) -> TySome (tyX, knK1, walk (c + 1) tyT2)
    | TyAbs (tyX, knK1, tyT2) -> TyAbs (tyX, knK1, walk (c + 1) tyT2)
    | TyApp (tyT1, tyT2) -> TyApp (walk c tyT1, walk c tyT2)
  in walk c tyT
  
let tmmap onvar ontype c t =
  let rec walk c t =
    match t with
    | TmInert (fi, tyT) -> TmInert (fi, ontype c tyT)
    | TmAscribe (fi, t1, tyT1) -> TmAscribe (fi, walk c t1, ontype c tyT1)
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, tyT1, t2) ->
        TmAbs (fi, x, ontype c tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmProj (fi, t1, l) -> TmProj (fi, walk c t1, l)
    | TmRecord (fi, fields) ->
        TmRecord (fi, List.map (fun (li, ti) -> (li, (walk c ti))) fields)
    | (TmString _ as t) -> t
    | (TmUnit _ as t) -> t
    | (TmLoc (_) as t) -> t
    | TmRef (fi, t1) -> TmRef (fi, walk c t1)
    | TmDeref (fi, t1) -> TmDeref (fi, walk c t1)
    | TmAssign (fi, t1, t2) -> TmAssign (fi, walk c t1, walk c t2)
    | (TmFloat _ as t) -> t
    | TmTimesfloat (fi, t1, t2) -> TmTimesfloat (fi, walk c t1, walk c t2)
    | TmLet (fi, x, t1, t2) -> TmLet (fi, x, walk c t1, walk (c + 1) t2)
    | (TmTrue _ as t) -> t
    | (TmFalse _ as t) -> t
    | TmIf (fi, t1, t2, t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
    | TmZero fi -> TmZero fi
    | TmSucc (fi, t1) -> TmSucc (fi, walk c t1)
    | TmPred (fi, t1) -> TmPred (fi, walk c t1)
    | TmIsZero (fi, t1) -> TmIsZero (fi, walk c t1)
    | TmFix (fi, t1) -> TmFix (fi, walk c t1)
    | TmTAbs (fi, tyX, knK1, t2) -> TmTAbs (fi, tyX, knK1, walk (c + 1) t2)
    | TmTApp (fi, t1, tyT2) -> TmTApp (fi, walk c t1, ontype c tyT2)
    | TmPack (fi, tyT1, t2, tyT3) ->
        TmPack (fi, ontype c tyT1, walk c t2, ontype c tyT3)
    | TmUnpack (fi, tyX, x, t1, t2) ->
        TmUnpack (fi, tyX, x, walk c t1, walk (c + 2) t2)
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
  | TyVarBind knK -> TyVarBind knK
  | TyAbbBind (tyT, opt) -> TyAbbBind (typeShift d tyT, opt)
  | VarBind tyT -> VarBind (typeShift d tyT)
  | TmAbbBind (t, tyTopt) ->
      let tyTopt' =
        (match tyTopt with
         | None -> None
         | Some tyT -> Some (typeShift d tyT))
      in TmAbbBind (termShift d t, tyTopt')
  
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
  | TmAbbBind (_, (Some tyT)) -> tyT
  | TmAbbBind (_, None) ->
      error fi ("No type recorded for variable " ^ (index2name fi ctx i))
  | _ ->
      error fi
        ("getTypeFromContext: Wrong kind of binding for variable " ^
           (index2name fi ctx i))
  
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
  match t with
  | TmInert (fi, _) -> fi
  | TmAscribe (fi, _, _) -> fi
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmProj (fi, _, _) -> fi
  | TmRecord (fi, _) -> fi
  | TmString (fi, _) -> fi
  | TmUnit fi -> fi
  | TmLoc (fi, _) -> fi
  | TmRef (fi, _) -> fi
  | TmDeref (fi, _) -> fi
  | TmAssign (fi, _, _) -> fi
  | TmFloat (fi, _) -> fi
  | TmTimesfloat (fi, _, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  | TmZero fi -> fi
  | TmSucc (fi, _) -> fi
  | TmPred (fi, _) -> fi
  | TmIsZero (fi, _) -> fi
  | TmFix (fi, _) -> fi
  | TmTAbs (fi, _, _, _) -> fi
  | TmTApp (fi, _, _) -> fi
  | TmPack (fi, _, _, _) -> fi
  | TmUnpack (fi, _, _, _, _) -> fi
  
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
  | TyRef tyT -> (pr "Ref "; printtyAType false ctx tyT)
  | TyAll (tyX, knK1, tyT2) ->
      let (ctx1, tyX) = pickfreshname ctx tyX
      in
        (obox ();
         pr "All ";
         pr tyX;
         prokn ctx knK1;
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
  | TyId b -> pr b
  | TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyRecord fields ->
      let pf i (li, tyTi) =
        (if li <> (string i) then (pr li; pr ":") else ();
         printtyType false ctx tyTi) in
      let rec p i l =
        (match l with
         | [] -> ()
         | [ f ] -> pf i f
         | f :: rest ->
             (pf i f;
              pr ",";
              if outer then print_space () else ``break`` ();
              p (i + 1) rest))
      in (pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox ())
  | TyBool -> pr "Bool"
  | TyFloat -> pr "Float"
  | TyNat -> pr "Nat"
  | TySome (tyX, knK1, tyT2) ->
      let (ctx1, tyX) = pickfreshname ctx tyX
      in
        (obox ();
         pr "{Some ";
         pr tyX;
         if knK1 <> KnStar
         then (pr "::"; printknKind false ctx knK1)
         else ();
         pr ",";
         if outer then print_space () else ``break`` ();
         printtyType false ctx1 tyT2;
         pr "}";
         cbox ())
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
  | TmAssign (_, t1, t2) ->
      (obox ();
       printtmAppTerm false ctx t1;
       pr " := ";
       printtmAppTerm false ctx t2;
       cbox ())
  | TmLet (_, x, t1, t2) ->
      (obox0 ();
       pr "let ";
       pr x;
       pr " = ";
       printtmTerm false ctx t1;
       print_space ();
       pr "in";
       print_space ();
       printtmTerm false (addname ctx x) t2;
       cbox ())
  | TmIf (_, t1, t2, t3) ->
      (obox0 ();
       pr "if ";
       printtmTerm false ctx t1;
       print_space ();
       pr "then ";
       printtmTerm false ctx t2;
       print_space ();
       pr "else ";
       printtmTerm false ctx t3;
       cbox ())
  | TmFix (_, t1) ->
      (obox (); pr "fix "; printtmTerm false ctx t1; cbox ())
  | TmTAbs (_, x, knK, t) ->
      let (ctx1, x) = pickfreshname ctx x
      in
        (obox ();
         pr "lambda ";
         pr x;
         prokn ctx knK;
         pr ".";
         if (small t) && (not outer) then ``break`` () else print_space ();
         printtmTerm outer ctx1 t;
         cbox ())
  | TmUnpack (_, tyX, x, t1, t2) ->
      let (ctx', tyX) = pickfreshname ctx tyX in
      let (ctx', x) = pickfreshname ctx' x
      in
        (obox ();
         pr "let {";
         pr tyX;
         pr ",";
         pr x;
         pr "} =";
         print_space ();
         printtmTerm false ctx t1;
         pr " in ";
         printtmTerm outer ctx' t2;
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
  | TmRef (_, t1) ->
      (obox (); pr "ref "; printtmATerm false ctx t1; cbox ())
  | TmDeref (_, t1) ->
      (obox (); pr "!"; printtmATerm false ctx t1; cbox ())
  | TmTimesfloat (_, _, t2) ->
      (pr "timesfloat ";
       printtmATerm false ctx t2;
       pr " ";
       printtmATerm false ctx t2)
  | TmPred (_, t1) -> (pr "pred "; printtmATerm false ctx t1)
  | TmIsZero (_, t1) -> (pr "iszero "; printtmATerm false ctx t1)
  | TmTApp (_, t, tyS) ->
      (obox0 ();
       printtmAppTerm false ctx t;
       print_space ();
       pr "[";
       printtyType false ctx tyS;
       pr "]";
       cbox ())
  | t -> printtmPathTerm outer ctx t
and printtmAscribeTerm outer ctx t =
  match t with
  | TmAscribe (_, t1, tyT1) ->
      (obox0 ();
       printtmAppTerm false ctx t1;
       print_space ();
       pr "as ";
       printtyType false ctx tyT1;
       cbox ())
  | t -> printtmATerm outer ctx t
and printtmPathTerm outer ctx t =
  match t with
  | TmProj (_, t1, l) -> (printtmATerm false ctx t1; pr "."; pr l)
  | t -> printtmAscribeTerm outer ctx t
and printtmATerm outer ctx t =
  match t with
  | TmInert (_, tyT) -> (pr "inert["; printtyType false ctx tyT; pr "]")
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
  | TmRecord (_, fields) ->
      let pf i (li, ti) =
        (if li <> (string i) then (pr li; pr "=") else ();
         printtmTerm false ctx ti) in
      let rec p i l =
        (match l with
         | [] -> ()
         | [ f ] -> pf i f
         | f :: rest ->
             (pf i f;
              pr ",";
              if outer then print_space () else ``break`` ();
              p (i + 1) rest))
      in (pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox ())
  | TmString (_, s) -> pr ("\"" ^ (s ^ "\""))
  | TmUnit _ -> pr "unit"
  | TmLoc (_, l) -> (pr "<loc #"; print_int l; pr ">")
  | TmFloat (_, s) -> pr (string s)
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | TmZero _ -> pr "0"
  | TmSucc (_, t1) ->
      let rec f n t =
        (match t with
         | TmZero _ -> pr (string n)
         | TmSucc (_, s) -> f (n + 1) s
         | _ -> (pr "(succ "; printtmATerm false ctx t1; pr ")"))
      in f 1 t1
  | TmPack (_, tyT1, t2, tyT3) ->
      (obox ();
       pr "{*";
       printtyType false ctx tyT1;
       pr ",";
       if outer then print_space () else ``break`` ();
       printtmTerm false ctx t2;
       pr "}";
       print_space ();
       pr "as ";
       printtyType outer ctx tyT3;
       cbox ())
  | t -> (pr "("; printtmTerm outer ctx t; pr ")")
  
let printtm ctx t = printtmTerm true ctx t
  
let prbinding ctx b =
  match b with
  | NameBind -> ()
  | TyVarBind knK -> (pr ":: "; printkn ctx knK)
  | VarBind tyT -> (pr ": "; printty ctx tyT)
  | TyAbbBind (tyT, _) -> (pr "= "; printty ctx tyT)
  | TmAbbBind (t, _) -> (pr "= "; printtm ctx t)
  

