﻿(*
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

open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.Format
open Ast
open Lexer
open Parser
open Core

let searchpath = ref [ "" ]
  
let argDefs =
  [ ("-I", (Arg.String (fun f -> searchpath := f :: !searchpath)),
     "Append a directory to the search path") ]
  
let parseArgs () =
  let inFile = ref (None : string option)
  in
    (Arg.parse argDefs
       (fun s ->
          match !inFile with
          | Some _ -> err "You must specify exactly one input file"
          | None -> inFile := Some s)
       "";
     match !inFile with
     | None -> err "You must specify an input file"
     | Some s -> s)
  
let openfile infile =
  let rec trynext l =
    match l with
    | [] -> err ("Could not find " ^ infile)
    | d :: rest ->
        let name = if d = "" then infile else d ^ ("/" ^ infile)
        in (try open_in name with | Sys_error m -> trynext rest)
  in trynext !searchpath
  
let parseFile inFile =
  let pi = openfile inFile in
  let lexbuf = Lexer.create inFile pi in
  let result =
    try Parser.toplevel Lexer.main lexbuf
    with | Parsing.RecoverableParseError -> error (Lexer.info lexbuf) "Parse error"
  in ((*Parsing.clear_parser ();*) close_in pi; result)
  
let alreadyImported = ref ([] : string list)
  
let prbindingty ctx b =
  match b with | NameBind -> () | VarBind tyT -> (pr ": "; printty tyT)
  
let rec process_command (ctx, nextuvar, constr) cmd =
  match cmd with
  | Eval (fi, t) ->
      let (tyT, nextuvar', constr_t) = recon ctx nextuvar t in
      let t' = eval ctx t in
      let constr' = combineconstr constr constr_t in
      let constr'' = unify fi ctx "Could not simplify constraints" constr'
      in
        (printtm_ATerm true ctx t';
         print_break 1 2;
         pr ": ";
         open_hovbox 0;
         printty (applysubst constr'' tyT);
         force_newline ();
         (ctx, nextuvar', constr''))
  | Bind (fi, x, bind) ->
      (pr x;
       pr " ";
       prbinding ctx bind;
       force_newline ();
       ((addbinding ctx x bind), uvargen, constr))
  
let process_file f (ctx, nextuvar, constr) =
  (alreadyImported := f :: !alreadyImported;
   let (cmds, _) = parseFile f ctx in
   let g (ctx, nextuvar, constr) c =
     (open_hvbox 0;
      let results = process_command (ctx, nextuvar, constr) c
      in (print_flush (); results))
   in List.fold_left g (ctx, nextuvar, constr) cmds)
  
let main () =
  let inFile = parseArgs () in
  let _ = process_file inFile (emptycontext, uvargen, emptyconstr) in ()
  
let () = set_max_boxes 1000
  
let () = set_margin 67
  
let res = Printexc.catch (fun () -> try (main (); 0) with | Exit x -> x) ()
  
let () = print_flush ()
  
let () = exit res
  

