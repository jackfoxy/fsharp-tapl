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
  
let parseInput (input : CommandLine.Source) =

    let parseIt lexbuf =
        Lexer.lineno := 1

        try Parser.toplevel Lexer.main lexbuf
        with Parsing.RecoverableParseError ->
            error (Lexer.info lexbuf) "Parse error"

    match input with
    | Source.Console s -> 
        LexBuffer<char>.FromString s
        |> parseIt
    | Source.File path ->
        use textReader = new System.IO.StreamReader(path)
        Lexer.filename := path
        LexBuffer<char>.FromTextReader textReader
        |> parseIt
    | _ -> invalidArg "can't get here" ""
    
let rec processCommand ctx cmd =
    match cmd with
    | Eval (_, t) ->
        let t' = eval ctx t
        printtmATerm true ctx t'
        force_newline ()
        ctx
    | Bind (_, x, bind) ->
        let bind' = evalbinding ctx bind
        pr x
        pr " "
        prbinding ctx bind'
        force_newline ()
        addbinding ctx x bind'
  
let processInput input ctx =
    let (cmds, _) = parseInput input ctx 
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
        | NoSource -> 
            CommandLine.reportEerror parsedCommand
        | input -> 
            let main () =
                processInput input emptycontext |> ignore

            Common.runMain main
            ()

        0