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
open CommandLine
  
let parseFile (inFile : string) =
    use textReader = new System.IO.StreamReader(inFile)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    Lexer.filename := inFile
    Lexer.lineno := 1

    try Parser.toplevel Lexer.main lexbuf
    with | Parsing.RecoverableParseError -> error (Lexer.info lexbuf) "Parse error"
    
let prbindingty _ b =
    match b with 
    | NameBind -> () 
    | VarBind tyT -> 
        pr ": "
        printty tyT
  
let rec processCommand ctx cmd =
    match cmd with
    | Eval (_, t) ->
        let tyT = typeof ctx t 
        let t' = eval ctx t
        printtmATerm true ctx t'
        print_break 1 2
        pr ": "
        printty tyT
        force_newline ()
        ctx
    | Bind (_, x, bind) ->
        pr x
        pr " "
        prbindingty ctx bind
        force_newline ()
        addbinding ctx x bind
  
let processFile f ctx =
    let (cmds, _) = parseFile f ctx 
    let g ctx c =
        open_hvbox 0
        let results = processCommand ctx c
        print_flush ()
        results
    List.fold g ctx cmds

module console1 =
    [<EntryPoint>]
    let main argv = 

        let parsedCommand = CommandLine.parse argv

        match parsedCommand.Source with
        | Source.Console s -> printfn "%s" parsedCommand.Usage
        | Source.File inFile -> 
            let main () =
                processFile inFile emptycontext |> ignore

            Common.runMain main
            ()
        
        | NoSource -> 
            CommandLine.reportEerror parsedCommand

        0