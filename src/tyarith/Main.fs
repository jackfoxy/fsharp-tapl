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
    
let rec process_command cmd =
  match cmd with
  | Eval (fi, t) ->
      let tyT = typeof t in
      let t' = eval t
      in
        (printtm_ATerm true t';
         print_break 1 2;
         pr ": ";
         printty tyT;
         force_newline ();
         ())
  
let process_file f =
  (Common.alreadyImported := f :: !Common.alreadyImported;
   let cmds = parseFile f in
   let g c =
     (open_hvbox 0;
      let results = process_command c in (print_flush (); results))
   in List.iter g cmds)
  
let main () = let inFile = Common.parseArgs () in let _ = process_file inFile in ()
  
let () = set_max_boxes 1000
  
let () = set_margin 67
  
let res = Printexc.catch (fun () -> try (main (); 0) with | Exit x -> x) ()
  
let () = print_flush ()
  
let () = exit res
  

