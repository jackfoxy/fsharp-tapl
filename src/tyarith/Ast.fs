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
type Ty = | TyBool | TyNat

type Term =
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term
  | TmZero of Info
  | TmSucc of Info * Term
  | TmPred of Info * Term
  | TmIsZero of Info * Term

type Command = | Eval of Info * Term

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
  match t with
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
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
  
let rec printtyType outer tyT =
  match tyT with | tyT -> printtyAType outer tyT
and printtyAType outer tyT =
  match tyT with
  | TyBool -> pr "Bool"
  | TyNat -> pr "Nat"
//  | tyT -> (pr "("; printtyType outer tyT; pr ")") //this rule will never be matched; from original tapl
  
let printty tyT = printtyType true tyT
  
let rec printtmTerm outer t =
  match t with
  | TmIf (_, t1, t2, t3) ->
      (obox0 ();
       pr "if ";
       printtmTerm false t1;
       print_space ();
       pr "then ";
       printtmTerm false t2;
       print_space ();
       pr "else ";
       printtmTerm false t3;
       cbox ())
  | t -> printtmAppTerm outer t
and printtmAppTerm outer t =
  match t with
  | TmPred (_, t1) -> (pr "pred "; printtmATerm false t1)
  | TmIsZero (_, t1) -> (pr "iszero "; printtmATerm false t1)
  | t -> printtmATerm outer t
and printtmATerm outer t =
  match t with
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | TmZero _ -> pr "0"
  | TmSucc (_, t1) ->
      let rec f n t =
        (match t with
         | TmZero _ -> pr (string n)
         | TmSucc (_, s) -> f (n + 1) s
         | _ -> (pr "(succ "; printtmATerm false t1; pr ")"))
      in f 1 t1
  | t -> (pr "("; printtmTerm outer t; pr ")")
  
let printtm t = printtmTerm true t
  

