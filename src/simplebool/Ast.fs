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
type Ty = | TyArr of Ty * Ty | TyBool

type Term =
  | TmVar of Info * int * int
  | TmAbs of Info * string * Ty * Term
  | TmApp of Info * Term * Term
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term

type Binding = | NameBind | VarBind of Ty

type Context = (string * Binding) list

type Command = | Eval of Info * Term | Bind of Info * string * Binding

(* ---------------------------------------------------------------------- *)
(* Context management *)
let emptyContext : Context = []
  
let ctxLength (ctx : Context) = List.length ctx
  
let addBinding (ctx : Context) x bind = (x, bind) :: ctx
  
let addName (ctx : Context) x = addBinding ctx x NameBind
  
let rec isName (ctx : Context) x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isName rest x
  
let rec pickfreshname ctx x =
  if isName ctx x
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
let tmmap onvar c t =
  let rec walk c t =
    match t with
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, tyT1, t2) -> TmAbs (fi, x, tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | (TmTrue _ as t) -> t
    | (TmFalse _ as t) -> t
    | TmIf (fi, t1, t2, t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
  in walk c t
  
let termShiftAbove d c t =
  tmmap
    (fun fi c x n ->
       if x >= c then TmVar (fi, x + d, n + d) else TmVar (fi, x, n + d))
    c t
  
let termShift d t = termShiftAbove d 0 t
  
(* ---------------------------------------------------------------------- *)
(* Substitution *)
let termSubst j s t =
  tmmap (fun fi j x n -> if x = j then termShift j s else TmVar (fi, x, n)) j
    t
  
let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
  
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)
let rec getBinding fi (ctx : Context) i =
  try let (_, bind) = List.item i ctx in bind
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg i (List.length ctx))
  
let getTypeFromContext fi ctx i =
  match getBinding fi ctx i with
  | VarBind tyT -> tyT
  | _ ->
      error fi
        ("getTypeFromContext: Wrong kind of binding for variable " +
           (index2Name fi ctx i))
  
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
  match t with
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  
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
  
let rec printTyType outer tyT =
  match tyT with | tyT -> printTyArrowType outer tyT
and printTyArrowType outer tyT =
  match tyT with
  | TyArr (tyT1, tyT2) ->
      (obox0 ();
       printTyAType false tyT1;
       if outer then pr " " else ();
       pr "->";
       if outer then print_space () else ``break`` ();
       printTyArrowType outer tyT2;
       cbox ())
  | tyT -> printTyAType outer tyT
and printTyAType outer tyT =
  match tyT with
  | TyBool -> pr "Bool"
  | tyT -> (pr "("; printTyType outer tyT; pr ")")
  
let printTy tyT = printTyType true tyT
  
let rec printtmTerm outer ctx t =
  match t with
  | TmAbs (_, x, tyT1, t2) ->
      let (ctx', x') = pickfreshname ctx x
      in
        (obox ();
         pr "lambda ";
         pr x';
         pr ":";
         printTyType false tyT1;
         pr ".";
         if (small t2) && (not outer) then ``break`` () else print_space ();
         printtmTerm outer ctx' t2;
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
  | t -> printtmAppTerm outer ctx t
and printtmAppTerm outer ctx t =
  match t with
  | TmApp (_, t1, t2) ->
      (obox0 ();
       printtmAppTerm false ctx t1;
       print_space ();
       printTerm false ctx t2;
       cbox ())
  | t -> printTerm outer ctx t
and printTerm outer ctx t =
  match t with
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
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | t -> (pr "("; printtmTerm outer ctx t; pr ")")
  
let printtm ctx t = printtmTerm true ctx t
  
let prbinding _ b =
  match b with | NameBind -> () | VarBind tyT -> (pr ": "; printTy tyT)
  

