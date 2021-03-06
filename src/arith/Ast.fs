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

type Term =
    | TmTrue of Info
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term
    | TmZero of Info
    | TmSucc of Info * Term
    | TmPred of Info * Term
    | TmIsZero of Info * Term

type Command =
    | Eval of Info * Term

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

let rec printTerm t =
    match t with
    | TmIf (_, t1, t2, t3) ->
        obox0 ()
        pr "if "
        printTerm t1
        print_space ()
        pr "then "
        printTerm t2
        print_space ()
        pr "else "
        printTerm t3
        cbox ()
    | t ->
        printtmAppTerm t

and printtmAppTerm t =
    match t with
    | TmPred (_, t1) ->
        pr "pred "
        printtmATerm t1
    | TmIsZero (_, t1) ->
        pr "iszero "
        printtmATerm t1
    | t ->
        printtmATerm t

and printtmATerm t =
    match t with
    | TmTrue _ ->
        pr "true"
    | TmFalse _ ->
        pr "false"
    | TmZero _ ->
        pr "0"
    | TmSucc (_, t1) ->
        let rec f n t =
            match t with
            | TmZero _ ->
                pr (string n)
            | TmSucc (_, s) ->
                f (n + 1) s
            | _ ->
                pr "(succ "
                printtmATerm t1
                pr ")"

        f 1 t1
    | t ->
        pr "("
        printTerm t
        pr ")"
 
