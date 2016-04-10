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
    
let rec processCommand cmd =
    match cmd with
    | Eval (_, t) ->
        let tyT = typeof t
        let t' = eval t
        printtmATerm true t'
        print_break 1 2
        pr ": "
        printty tyT
        force_newline ()
        ()
  
let processFile f =
    Common.alreadyImported := f :: !Common.alreadyImported
    let cmds = parseFile f
    let g c =
        open_hvbox 0
        let results = processCommand c
        print_flush ()
        results
    List.iter g cmds
  
let main () = 
    let inFile = Common.parseArgs ()
    processFile inFile |> ignore
  
Common.runMain main
