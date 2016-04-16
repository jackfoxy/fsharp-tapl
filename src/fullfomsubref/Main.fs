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

//let parseInputFile (f : string) =
//
//    let parseIt lexbuf =
//        Lexer.lineno := 1
//
//        try Parser.toplevel Lexer.main lexbuf
//        with Parsing.RecoverableParseError ->
//            error (Lexer.info lexbuf) "Parse error"
//
//    use textReader = new System.IO.StreamReader(f)
//    Lexer.filename := f
//    LexBuffer<char>.FromTextReader textReader
//    |> parseIt

  
let checkbinding fi ctx b =
    match b with
    | NameBind -> NameBind
    | TyVarBind tyS -> 
        kindof ctx tyS |> ignore
        TyVarBind tyS
    | TyAbbBind (tyT, None) -> TyAbbBind (tyT, Some (kindof ctx tyT))
    | VarBind tyT -> VarBind tyT
    | TmAbbBind (t, None) -> TmAbbBind (t, Some (typeof ctx t))
    | TmAbbBind (t, (Some tyT)) ->
        let tyT' = typeof ctx t
        if subtype ctx tyT' tyT
        then TmAbbBind (t, Some tyT)
        else error fi "Type of binding does not match declared type"
    | TyAbbBind (tyT, (Some knK)) ->
        let knK' = kindof ctx tyT
        if knK = knK'
        then TyAbbBind (tyT, Some knK)
        else error fi "Kind of binding does not match declared kind"
  
let prbindingty ctx b =
    match b with
    | NameBind -> ()
    | TyVarBind tyS -> (pr "<: "; printty ctx tyS)
    | VarBind tyT -> (pr ": "; printty ctx tyT)
    | TyAbbBind (tyT, knKopt) ->
        pr ":: "
        match knKopt with
        | None -> printkn ctx (kindof ctx tyT)
        | Some knK -> printkn ctx knK
    | TmAbbBind (t, tyTopt) ->
        pr ": "
        match tyTopt with
        | None -> printty ctx (typeof ctx t)
        | Some tyT -> printty ctx tyT
  
let rec processInput input (ctx, store) =
    let (cmds, _) = parseInput input ctx 
    let g (ctx, store) c =
        open_hvbox 0
        let results = processCommand (ctx, store) c
        print_flush ()
        results
    List.fold g (ctx, store) cmds
and processCommand (ctx, store) cmd =
    match cmd with
    | Import f -> processInput (Source.File f) (ctx, store)
    | Eval (_, t) ->
        let tyT = typeof ctx t 
        let (t', store) = eval ctx store t
        printtmATerm true ctx t'
        print_break 1 2
        pr ": "
        printty ctx tyT
        force_newline ()
        (ctx, store)
    | Bind (fi, x, bind) ->
        let bind = checkbinding fi ctx bind
        let (bind', store') = evalbinding ctx store bind
        pr x
        pr " "
        prbindingty ctx bind'
        force_newline ()
        ((addbinding ctx x bind'), (shiftstore 1 store'))
    | SomeBind (fi, tyX, x, t) ->
        let tyT = typeof ctx t
        match lcst ctx tyT with
        | TySome (_, tyBound, tyBody) ->
            let (t', store') = eval ctx store t 
            let b =
                match t' with
                | TmPack (_, _, t12, _) ->
                    TmAbbBind (termShift 1 t12, Some tyBody)
                | _ -> VarBind tyBody
            let ctx1 = addbinding ctx tyX (TyVarBind tyBound)
            let ctx2 = addbinding ctx1 x b
            pr tyX
            force_newline ()
            pr x
            pr " : "
            printty ctx1 tyBody
            force_newline ()
            (ctx2, store')
        | _ -> error fi "existential type expected"

module console1 =
    [<EntryPoint>]
    let main argv = 

        let parsedCommand = CommandLine.parse argv

        match parsedCommand.Source with
        | NoSource -> 
            CommandLine.reportEerror parsedCommand
        | input -> 
            let main () =
                processInput input (emptycontext, emptystore) |> ignore

            Common.runMain main
            ()
        
        | NoSource -> 
            CommandLine.reportEerror parsedCommand

        0