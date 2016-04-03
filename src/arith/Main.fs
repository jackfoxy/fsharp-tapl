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

//open System
open Microsoft.FSharp.Text
//open Microsoft.FSharp.Text.Lexing
open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.Format
open Ast
//open Lexer
//open Parser
open Core
open TaplCommon

//let searchpath = ref [""]

//let argDefs = [
//  "-I",
//      Arg.String (fun f -> searchpath := f :: !searchpath),
//      "Append a directory to the search path"]

//let parseArgs () =
//    let inFile : string option ref = ref None
//    Arg.parse argDefs
//     (fun s ->
//       match !inFile with
//       | Some _ -> err "You must specify exactly one input file"
//       | None -> inFile := Some s)
//     ""
//    match !inFile with
//    | Some s -> s
//    | None ->
//        err "You must specify an input file"

//let openfile infile =
//    let rec trynext l =
//        match l with
//        | [] -> err ("Could not find " ^ infile)
//        | d :: rest ->
//            let name = if d = "" then infile else (d ^ "/" ^ infile)
//            try open_in name
//            with Sys_error m ->
//                trynext rest
//    trynext !searchpath

let parseFile inFile =
    let pi = Common.openfile inFile
    let lexbuf = Lexer.create inFile pi
    let result =
        try Parser.toplevel Lexer.main lexbuf
        //with Parsing.Parse_error ->
        with Parsing.RecoverableParseError ->
            error (Lexer.info lexbuf) "Parse error"
    //Parsing.clear_parser()
    close_in pi
    result

//let alreadyImported = ref ([] : string list)

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

