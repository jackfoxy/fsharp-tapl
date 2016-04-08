(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

module Main

open Microsoft.FSharp.Text
open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.Format
open Ast
open Core
open TaplCommon
  
let parseFile inFile =
  let pi = Common.openfile inFile in
  let lexbuf = Lexer.create inFile pi in
  let result =
    try Parser.toplevel Lexer.main lexbuf
    with | Parsing.RecoverableParseError -> error (Lexer.info lexbuf) "Parse error"
  in ((*Parsing.clear_parser ();*) close_in pi; result)
    
let checkbinding fi ctx b =
  match b with
  | NameBind -> NameBind
  | VarBind tyT -> VarBind tyT
  | TmAbbBind (t, None) -> TmAbbBind (t, Some (typeof ctx t))
  | TmAbbBind (t, (Some tyT)) ->
      let tyT' = typeof ctx t
      in
        if subtype ctx tyT' tyT
        then TmAbbBind (t, Some tyT)
        else error fi "Type of binding does not match declared type"
  | TyVarBind -> TyVarBind
  | TyAbbBind tyT -> TyAbbBind tyT
  
let prbindingty ctx b =
  match b with
  | NameBind -> ()
  | TyVarBind -> ()
  | VarBind tyT -> (pr ": "; printty ctx tyT)
  | TmAbbBind (t, tyT_opt) ->
      (pr ": ";
       (match tyT_opt with
        | None -> printty ctx (typeof ctx t)
        | Some tyT -> printty ctx tyT))
  | TyAbbBind tyT -> pr ":: *"
  
let rec process_command ctx cmd =
  match cmd with
  | Eval (fi, t) ->
      let tyT = typeof ctx t in
      let t' = eval ctx t
      in
        (printtm_ATerm true ctx t';
         print_break 1 2;
         pr ": ";
         printty ctx tyT;
         force_newline ();
         ctx)
  | Bind (fi, x, bind) ->
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind
      in
        (pr x;
         pr " ";
         prbindingty ctx bind';
         force_newline ();
         addbinding ctx x bind')
  
let process_file f ctx =
  (Common.alreadyImported := f :: !Common.alreadyImported;
   let (cmds, _) = parseFile f ctx in
   let g ctx c =
     (open_hvbox 0;
      let results = process_command ctx c in (print_flush (); results))
   in List.fold_left g ctx cmds)
  
let main () =
  let inFile = Common.parseArgs () in let _ = process_file inFile emptycontext in ()
  
let () = set_max_boxes 1000
  
let () = set_margin 67
  
let res = Printexc.catch (fun () -> try (main (); 0) with | Exit x -> x) ()
  
let () = print_flush ()
  
let () = exit res
  

