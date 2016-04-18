(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module FullreconLib: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

namespace FSharpTapl

open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open FSharp.Compatibility.OCaml.Format
open Ast
open Core
open CommandLine

module FullreconLib =

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
    
    let prbindingty _ b =
        match b with 
        | NameBind -> () 
        | VarBind tyT -> 
            pr ": " 
            printty tyT
  
    let rec processCommand (ctx, nextuvar, constr) cmd =
        match cmd with
        | Eval (fi, t) ->
            let (tyT, nextuvar', constr_t) = recon ctx nextuvar t 
            let t' = eval ctx t 
            let constr' = combineconstr constr constr_t 
            let constr'' = unify fi ctx "Could not simplify constraints" constr'
            printtmATerm true ctx t'
            print_break 1 2
            pr ": "
            open_hovbox 0
            printty (applysubst constr'' tyT)
            force_newline ()
            (ctx, nextuvar', constr'')
        | Bind (_, x, bind) ->
            pr x
            pr " "
            prbinding ctx bind
            force_newline ()
            ((addbinding ctx x bind), uvargen, constr)
  
    let processInput input (ctx, nextuvar, constr) =
        let (cmds, _) = parseInput input ctx 
        let g (ctx, nextuvar, constr) c =
            open_hvbox 0
            let results = processCommand (ctx, nextuvar, constr) c
            print_flush ()
            results
        List.fold g (ctx, nextuvar, constr) cmds
