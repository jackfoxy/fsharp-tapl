(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module EquirecLib: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

namespace FSharpTapl

open Ast
open CommandLine
open Compatability
open Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing

module EquirecLib = 

    let parseInput (input : CommandLine.Source) = 

        let parseIt lexbuf = 
            Lexer.lineno := 1

            try 
                Parser.toplevel Lexer.main lexbuf
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
    
    let prbindingty ctx b = 
        match b with
        | NameBind -> ()
        | VarBind tyT -> 
            (pr ": "
             printty ctx tyT)
        | TyVarBind -> ()
    
    let rec processCommand ctx cmd = 
        match cmd with
        | Eval(_, t) -> 
            let tyT = typeof ctx t
            let t' = eval ctx t
            printtmATerm true ctx t'
            print_break 1 2
            pr ": "
            printty ctx tyT
            force_newline()
            ctx
        | Bind(_, x, bind) -> 
            pr x
            pr " "
            prbindingty ctx bind
            force_newline()
            addbinding ctx x bind
    
    let processInput parsedCommand input ctx = 
        setOutput parsedCommand
        let (cmds, _) = parseInput input ctx
        
        let g ctx c = 
            open_hvbox 0
            let results = processCommand ctx c
            print_flush ()
            results
        List.fold g ctx cmds
