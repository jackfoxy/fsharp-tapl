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
    
let prbindingty ctx b =
    match b with
    | NameBind -> ()
    | TyVarBind tyS -> (pr "<: "; printty ctx tyS)
    | VarBind tyT -> (pr ": "; printty ctx tyT)
  
let rec processCommand ctx cmd =
    match cmd with
    | Eval (_, t) ->
        let tyT = typeof ctx t in
        let t' = eval ctx t
        printtm_ATerm true ctx t'
        print_break 1 2
        pr ": "
        printty ctx tyT
        force_newline ()
        ctx
    | Bind (_, x, bind) ->
        pr x
        pr " "
        prbindingty ctx bind
        force_newline ()
        addbinding ctx x bind
  
let processFile f ctx =
    Common.alreadyImported := f :: !Common.alreadyImported
    let (cmds, _) = parseFile f ctx 
    let g ctx c =
        open_hvbox 0
        let results = processCommand ctx c 
        print_flush ()
        results
    List.fold g ctx cmds
  
let main () =
  let inFile = Common.parseArgs ()
  processFile inFile emptycontext |> ignore
  
Common.runMain main
