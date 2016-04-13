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
    with Parsing.RecoverableParseError ->
        error (Lexer.info lexbuf) "Parse error"
    
let checkbinding fi ctx b =
    match b with
    | NameBind -> NameBind
    | TyVarBind -> TyVarBind
    | TyAbbBind tyT -> TyAbbBind tyT
    | VarBind tyT -> VarBind tyT
    | TmAbbBind (t, None) -> TmAbbBind (t, Some (typeof ctx t))
    | TmAbbBind (t, (Some tyT)) ->
        let tyT' = typeof ctx t
        if tyeqv ctx tyT' tyT
        then TmAbbBind (t, Some tyT)
        else error fi "Type of binding does not match declared type"
  
let prbindingty ctx b =
  match b with
  | NameBind -> ()
  | TyVarBind -> ()
  | VarBind tyT -> (pr ": "; printty ctx tyT)
  | TyAbbBind _ -> pr ":: *"
  | TmAbbBind (t, tyTopt) ->
    pr ": "
    match tyTopt with
    | None -> printty ctx (typeof ctx t)
    | Some tyT -> printty ctx tyT
  
let rec processCommand ctx cmd =
    match cmd with
    | Eval (_, t) ->
        let tyT = typeof ctx t
        let t' = eval ctx t
        printtmATerm true ctx t'
        print_break 1 2
        pr ": "
        printty ctx tyT
        force_newline ()
        ctx
    | Bind (fi, x, bind) ->
        let bind = checkbinding fi ctx bind
        let bind' = evalbinding ctx bind
        pr x
        pr " "
        prbindingty ctx bind'
        force_newline ()
        addbinding ctx x bind'
    | SomeBind (fi, tyX, x, t) ->
        let tyT = typeof ctx t
        match simplifyty ctx tyT with
        | TySome (_, tyBody) ->
            let t' = eval ctx t
            let b =
                match t' with
                | TmPack (_, _, t12, _) ->
                    TmAbbBind (termShift 1 t12, Some tyBody)
                | _ -> VarBind tyBody
            let ctx1 = addbinding ctx tyX TyVarBind
            let ctx2 = addbinding ctx1 x b
            pr tyX
            force_newline ()
            pr x
            pr " : "
            printty ctx1 tyBody
            force_newline ()
            ctx2
        | _ -> error fi "existential type expected"
  
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