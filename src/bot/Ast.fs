(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Syntax trees and associated support functions.
module Ast

open FSharp.Compatibility.OCaml.Format

(* ---------------------------------------------------------------------- *)
(* Datatypes *)
type Ty =
    | TyTop
    | TyBot
    | TyArr of Ty * Ty

type Term =
    | TmVar of Info * int * int
    | TmAbs of Info * string * Ty * Term
    | TmApp of Info * Term * Term

type Binding =
    | NameBind
    | VarBind of Ty

type Context = (string * Binding) list

type Command =
    | Eval of Info * Term
    | Bind of Info * string * Binding

(* ---------------------------------------------------------------------- *)
(* Context management *)
let emptycontext = []
  
let ctxlength ctx =
    List.length ctx
  
let addbinding ctx x bind =
    (x, bind) :: ctx
  
let addname ctx x =
    addbinding ctx x NameBind
  
let rec isnamebound ctx x =
    match ctx with
    | [] ->
        false
    | (y, _) :: rest ->
        if y = x then true
        else isnamebound rest x
  
let rec pickfreshname ctx x =
    if isnamebound ctx x then
        pickfreshname ctx (x + "'")
    else
        ((x, NameBind) :: ctx), x
  
let index2name fi ctx x =
    try let xn, _ = List.item x ctx
        xn
    with
    | Failure _ ->
        let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" x (List.length ctx)
        error fi msg
  
let rec name2index fi ctx x =
    match ctx with
    | [] ->
        error fi ("Identifier " ^ (x ^ " is unbound"))
    | (y, _) :: rest ->
        if y = x then 0
        else 1 + (name2index fi rest x)
  
(* ---------------------------------------------------------------------- *)
(* Shifting *)
let tmmap onvar c t =
    let rec walk c t =
        match t with
        | TmVar (fi, x, n) ->
            onvar fi c x n
        | TmAbs (fi, x, tyT1, t2) ->
            TmAbs (fi, x, tyT1, walk (c + 1) t2)
        | TmApp (fi, t1, t2) ->
            TmApp (fi, walk c t1, walk c t2)
    walk c t
  
let termShiftAbove d c t =
    (c, t)
    ||> tmmap (fun fi c x n ->
        if x >= c then TmVar (fi, x + d, n + d)
        else TmVar (fi, x, n + d))
  
let termShift d t =
    termShiftAbove d 0 t
  
(* ---------------------------------------------------------------------- *)
(* Substitution *)
let termSubst j s t =
    (j, t)
    ||> tmmap (fun fi j x n ->
        if x = j then termShift j s
        else TmVar (fi, x, n))
  
let termSubstTop s t =
    termShift -1 (termSubst 0 (termShift 1 s) t)
  
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)
let rec getbinding fi ctx i =
    try let _, bind = List.item i ctx
        bind
    with Failure _ ->
        let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" i (List.length ctx)
        error fi msg
  
let getTypeFromContext fi ctx i =
    match getbinding fi ctx i with
    | VarBind tyT -> tyT
    | _ ->
        error fi ("getTypeFromContext: Wrong kind of binding for variable " + (index2name fi ctx i))
  
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
    match t with
    | TmVar (fi, _, _) -> fi
    | TmAbs (fi, _, _, _) -> fi
    | TmApp (fi, _, _) -> fi
  
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
  
let small t =
    match t with
    | TmVar (_) -> true
    | _ -> false
  
let rec printtyType outer tyT =
  match tyT with
  | tyT -> printtyArrowType outer tyT

and printtyArrowType outer tyT =
    match tyT with
    | TyArr (tyT1, tyT2) ->
        obox0 ()
        printtyAType false tyT1
        if outer then pr " " else ()
        pr "->"
        if outer then print_space () else ``break`` ()
        printtyArrowType outer tyT2
        cbox ()
    | tyT ->
        printtyAType outer tyT

and printtyAType outer tyT =
    match tyT with
    | TyTop -> pr "Top"
    | TyBot -> pr "Bot"
    | tyT ->
        pr "("
        printtyType outer tyT
        pr ")"
  
let printty tyT = printtyType true tyT
  
let rec printtmTerm outer ctx t =
    match t with
    | TmAbs (_, x, tyT1, t2) ->
        let ctx', x' = pickfreshname ctx x
        obox ()
        pr "lambda "
        pr x'
        pr ":"
        printtyType false tyT1
        pr "."
        if small t2 && not outer then ``break`` () else print_space ()
        printtmTerm outer ctx' t2
        cbox ()
    | t ->
        printtmAppTerm outer ctx t

and printtmAppTerm outer ctx t =
    match t with
    | TmApp (_, t1, t2) ->
        obox0 ()
        printtmAppTerm false ctx t1
        print_space ()
        printtmATerm false ctx t2
        cbox ()
    | t ->
        printtmATerm outer ctx t

and printtmATerm outer ctx t =
  match t with
  | TmVar (fi, x, n) ->
      if (ctxlength ctx) = n then
        pr (index2name fi ctx x)
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
  
let prbinding _ b =
    match b with
    | NameBind -> ()
    | VarBind tyT ->
        pr ": "
        printty tyT
  
