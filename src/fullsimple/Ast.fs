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
type Ty =
  | TyVar of int * int
  | TyId of string
  | TyArr of Ty * Ty
  | TyUnit
  | TyRecord of (string * Ty) list
  | TyVariant of (string * Ty) list
  | TyBool
  | TyString
  | TyFloat
  | TyNat

type Term =
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term
  | TmCase of Info * Term * (string * (string * Term)) list
  | TmTag of Info * string * Term * Ty
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmLet of Info * string * Term * Term
  | TmFix of Info * Term
  | TmString of Info * string
  | TmUnit of Info
  | TmAscribe of Info * Term * Ty
  | TmRecord of Info * (string * Term) list
  | TmProj of Info * Term * string
  | TmFloat of Info * float
  | TmTimesfloat of Info * Term * Term
  | TmZero of Info
  | TmSucc of Info * Term
  | TmPred of Info * Term
  | TmIsZero of Info * Term
  | TmInert of Info * Ty

type Binding =
  | NameBind
  | TyVarBind
  | VarBind of Ty
  | TmAbbBind of Term * Ty option
  | TyAbbBind of Ty

type Context = (string * Binding) list

type Command = | Eval of Info * Term | Bind of Info * string * Binding

(* ---------------------------------------------------------------------- *)
(* Context management *)
let emptyContext : Context = []
  
let ctxLength (ctx : Context) = List.length ctx
  
let addBinding (ctx : Context) x bind = (x, bind) :: ctx
  
let addName ctx x = addBinding ctx x NameBind
  
let rec isNameBound (ctx : Context) x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isNameBound rest x
  
let rec pickfreshname ctx x =
  if isNameBound ctx x
  then pickfreshname ctx (x + "'")
  else (((x, NameBind) :: ctx), x)
  
let index2Name fi (ctx : Context) x =
  try let (xn, _) = List.item x ctx in xn
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg x (List.length ctx))
  
let rec name2Index fi (ctx : Context) x =
  match ctx with
  | [] -> error fi ("Identifier " + (x + " is unbound"))
  | (y, _) :: rest -> if y = x then 0 else 1 + (name2Index fi rest x)
  
(* ---------------------------------------------------------------------- *)
(* Shifting *)
let tymap onvar c tyT =
  let rec walk c tyT =
    match tyT with
    | TyVar (x, n) -> onvar c x n
    | (TyId _ as tyT) -> tyT
    | TyString -> TyString
    | TyUnit -> TyUnit
    | TyRecord fieldtys ->
        TyRecord (List.map (fun (li, tyTi) -> (li, (walk c tyTi))) fieldtys)
    | TyFloat -> TyFloat
    | TyBool -> TyBool
    | TyNat -> TyNat
    | TyArr (tyT1, tyT2) -> TyArr (walk c tyT1, walk c tyT2)
    | TyVariant fieldtys ->
        TyVariant (List.map (fun (li, tyTi) -> (li, (walk c tyTi))) fieldtys)
  in walk c tyT
  
let tmmap onvar ontype c t =
  let rec walk c t =
    match t with
    | TmInert (fi, tyT) -> TmInert (fi, ontype c tyT)
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, tyT1, t2) ->
        TmAbs (fi, x, ontype c tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmLet (fi, x, t1, t2) -> TmLet (fi, x, walk c t1, walk (c + 1) t2)
    | TmFix (fi, t1) -> TmFix (fi, walk c t1)
    | (TmTrue _ as t) -> t
    | (TmFalse _ as t) -> t
    | TmIf (fi, t1, t2, t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
    | (TmString _ as t) -> t
    | (TmUnit _ as t) -> t
    | TmProj (fi, t1, l) -> TmProj (fi, walk c t1, l)
    | TmRecord (fi, fields) ->
        TmRecord (fi, List.map (fun (li, ti) -> (li, (walk c ti))) fields)
    | TmAscribe (fi, t1, tyT1) -> TmAscribe (fi, walk c t1, ontype c tyT1)
    | (TmFloat _ as t) -> t
    | TmTimesfloat (fi, t1, t2) -> TmTimesfloat (fi, walk c t1, walk c t2)
    | TmZero fi -> TmZero fi
    | TmSucc (fi, t1) -> TmSucc (fi, walk c t1)
    | TmPred (fi, t1) -> TmPred (fi, walk c t1)
    | TmIsZero (fi, t1) -> TmIsZero (fi, walk c t1)
    | TmTag (fi, l, t1, tyT) -> TmTag (fi, l, walk c t1, ontype c tyT)
    | TmCase (fi, t, cases) ->
        TmCase (fi, walk c t,
          List.map (fun (li, (xi, ti)) -> (li, (xi, (walk (c + 1) ti))))
            cases)
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
  | TyVarBind -> TyVarBind
  | TmAbbBind (t, tyTopt) ->
      let tyTopt' =
        (match tyTopt with
         | None -> None
         | Some tyT -> Some (typeShift d tyT))
      in TmAbbBind (termShift d t, tyTopt')
  | VarBind tyT -> VarBind (typeShift d tyT)
  | TyAbbBind tyT -> TyAbbBind (typeShift d tyT)
  
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
  
let tyTermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
  
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)
let rec getBinding fi (ctx : Context) i =
  try let (_, bind) = List.item i ctx in bindingshift (i + 1) bind
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg i (List.length ctx))
  
let getTypeFromContext fi ctx i =
  match getBinding fi ctx i with
  | VarBind tyT -> tyT
  | TmAbbBind (_, (Some tyT)) -> tyT
  | TmAbbBind (_, None) ->
      error fi ("No type recorded for variable " + (index2Name fi ctx i))
  | _ ->
      error fi
        ("getTypeFromContext: Wrong kind of binding for variable " +
           (index2Name fi ctx i))
  
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
  match t with
  | TmInert (fi, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  | TmTag (fi, _, _, _) -> fi
  | TmCase (fi, _, _) -> fi
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  | TmFix (fi, _) -> fi
  | TmString (fi, _) -> fi
  | TmUnit fi -> fi
  | TmAscribe (fi, _, _) -> fi
  | TmProj (fi, _, _) -> fi
  | TmRecord (fi, _) -> fi
  | TmFloat (fi, _) -> fi
  | TmTimesfloat (fi, _, _) -> fi
  | TmZero fi -> fi
  | TmSucc (fi, _) -> fi
  | TmPred (fi, _) -> fi
  | TmIsZero (fi, _) -> fi
  
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
  
let rec printTyType outer ctx tyT =
  match tyT with | tyT -> printTyArrowType outer ctx tyT
and printTyArrowType outer ctx tyT =
  match tyT with
  | TyArr (tyT1, tyT2) ->
      (obox0 ();
       printTyAType false ctx tyT1;
       if outer then pr " " else ();
       pr "->";
       if outer then print_space () else ``break`` ();
       printTyArrowType outer ctx tyT2;
       cbox ())
  | tyT -> printTyAType outer ctx tyT
and printTyAType outer ctx tyT =
  match tyT with
  | TyVar (x, n) ->
      if (ctxLength ctx) = n
      then pr (index2Name dummyinfo ctx x)
      else
        pr
          ("[bad index: " +
             ((string x) +
                ("/" +
                   ((string n) +
                      (" in {" +
                         ((List.fold (fun s (x, _) -> s + (" " + x)) ""
                             ctx)
                            + " }]"))))))
  | TyId b -> pr b
  | TyBool -> pr "Bool"
  | TyVariant fields ->
      let pf i (li, tyTi) =
        (if li <> (string i) then (pr li; pr ":") else ();
         printTyType false ctx tyTi) in
      let rec p i l =
        (match l with
         | [] -> ()
         | [ f ] -> pf i f
         | f :: rest ->
             (pf i f;
              pr ",";
              if outer then print_space () else ``break`` ();
              p (i + 1) rest))
      in (pr "<"; open_hovbox 0; p 1 fields; pr ">"; cbox ())
  | TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyRecord fields ->
      let pf i (li, tyTi) =
        (if li <> (string i) then (pr li; pr ":") else ();
         printTyType false ctx tyTi) in
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
  | TyFloat -> pr "Float"
  | TyNat -> pr "Nat"
  | tyT -> (pr "("; printTyType outer ctx tyT; pr ")")
  
let printTy (ctx : Context) tyT = printTyType true ctx tyT
  
let rec printtmTerm outer ctx t =
  match t with
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
  | TmCase (_, t, cases) ->
      (obox ();
       pr "case ";
       printtmTerm false ctx t;
       pr " of";
       print_space ();
       let pc (li, (xi, ti)) =
         let (ctx', xi') = pickfreshname ctx xi
         in
           (pr "<";
            pr li;
            pr "=";
            pr xi';
            pr ">==>";
            printtmTerm false ctx' ti) in
       let rec p l =
         (match l with
          | [] -> ()
          | [ c ] -> pc c
          | c :: rest -> (pc c; print_space (); pr "| "; p rest))
       in (p cases; cbox ()))
  | TmAbs (_, x, tyT1, t2) ->
      let (ctx', x') = pickfreshname ctx x
      in
        (obox ();
         pr "lambda ";
         pr x';
         pr ":";
         printTyType false ctx tyT1;
         pr ".";
         if (small t2) && (not outer) then ``break`` () else print_space ();
         printtmTerm outer ctx' t2;
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
       printtmTerm false (addName ctx x) t2;
       cbox ())
  | TmFix (_, t1) ->
      (obox (); pr "fix "; printtmTerm false ctx t1; cbox ())
  | t -> printtmAppTerm outer ctx t
and printtmAppTerm outer ctx t =
  match t with
  | TmApp (_, t1, t2) ->
      (obox0 ();
       printtmAppTerm false ctx t1;
       print_space ();
       printTerm false ctx t2;
       cbox ())
  | TmTimesfloat (_, _, t2) ->
      (pr "timesfloat ";
       printTerm false ctx t2;
       pr " ";
       printTerm false ctx t2)
  | TmPred (_, t1) -> (pr "pred "; printTerm false ctx t1)
  | TmIsZero (_, t1) -> (pr "iszero "; printTerm false ctx t1)
  | t -> printtmPathTerm outer ctx t
and printtmAscribeTerm outer ctx t =
  match t with
  | TmAscribe (_, t1, tyT1) ->
      (obox0 ();
       printtmAppTerm false ctx t1;
       print_space ();
       pr "as ";
       printTyType false ctx tyT1;
       cbox ())
  | t -> printTerm outer ctx t
and printtmPathTerm outer ctx t =
  match t with
  | TmProj (_, t1, l) -> (printTerm false ctx t1; pr "."; pr l)
  | t -> printtmAscribeTerm outer ctx t
and printTerm outer ctx t =
  match t with
  | TmInert (_, tyT) -> (pr "inert["; printTyType false ctx tyT; pr "]")
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | TmTag (_, l, t, tyT) ->
      (obox ();
       pr "<";
       pr l;
       pr "=";
       printtmTerm false ctx t;
       pr ">";
       print_space ();
       pr "as ";
       printTyType outer ctx tyT;
       cbox ())
  | TmVar (fi, x, n) ->
      if (ctxLength ctx) = n
      then pr (index2Name fi ctx x)
      else
        pr
          ("[bad index: " +
             ((string x) +
                ("/" +
                   ((string n) +
                      (" in {" +
                         ((List.fold (fun s (x, _) -> s + (" " + x)) ""
                             ctx)
                            + " }]"))))))
  | TmString (_, s) -> pr ("\"" + (s + "\""))
  | TmUnit _ -> pr "unit"
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
  | TmFloat (_, s) -> pr (string s)
  | TmZero _ -> pr "0"
  | TmSucc (_, t1) ->
      let rec f n t =
        (match t with
         | TmZero _ -> pr (string n)
         | TmSucc (_, s) -> f (n + 1) s
         | _ -> (pr "(succ "; printTerm false ctx t1; pr ")"))
      in f 1 t1
  | t -> (pr "("; printtmTerm outer ctx t; pr ")")
  
let printtm ctx t = printtmTerm true ctx t
  
let prbinding ctx b =
  match b with
  | NameBind -> ()
  | TyVarBind -> ()
  | VarBind tyT -> (pr ": "; printTy ctx tyT)
  | TmAbbBind (t, _) -> (pr "= "; printtm ctx t)
  | TyAbbBind tyT -> (pr "= "; printTy ctx tyT)
  

