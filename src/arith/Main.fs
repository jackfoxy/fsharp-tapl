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
open Microsoft.FSharp.Text.Lexing
open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.Format
open Ast
open Core
open TaplCommon

let parseFile (inFile : string) =
    use textReader = new System.IO.StreamReader(inFile)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    Lexer.filename := inFile
    Lexer.lineno := 1

    try Parser.toplevel Lexer.main lexbuf
    with Parsing.RecoverableParseError ->
        error (Lexer.info lexbuf) "Parse error"

let rec process_command cmd =
    match cmd with
    | Eval (fi, t) ->
        let t' = eval t
        printtm_ATerm true t'
        force_newline ()
  
let process_file f =
    Common.alreadyImported := f :: !Common.alreadyImported
    let cmds = parseFile f
    let g c =
        open_hvbox 0
        let results = process_command c
        print_flush ()
        results
    List.iter g cmds

let main () =
    let inFile = Common.parseArgs ()
    process_file inFile

let () = set_max_boxes 1000
  
let () = set_margin 67
  
let res = Printexc.catch (fun () -> try (main (); 0) with | Exit x -> x) ()
  
let () = print_flush ()
  
let () = exit res

