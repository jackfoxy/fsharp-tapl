(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module Fullfomsubref: The main program.  Deals with processing the command
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

module FullfomsubrefLib = 

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
    
    let checkbinding fi ctx b = 
        match b with
        | NameBind -> NameBind
        | TyVarBind tyS -> 
            kindOf ctx tyS |> ignore
            TyVarBind tyS
        | TyAbbBind(tyT, None) -> TyAbbBind(tyT, Some(kindOf ctx tyT))
        | VarBind tyT -> VarBind tyT
        | TmAbbBind(t, None) -> TmAbbBind(t, Some(typeOf ctx t))
        | TmAbbBind(t, (Some tyT)) -> 
            let tyT' = typeOf ctx t
            if subType ctx tyT' tyT then TmAbbBind(t, Some tyT)
            else error fi "Type of binding does not match declared type"
        | TyAbbBind(tyT, (Some knK)) -> 
            let knK' = kindOf ctx tyT
            if knK = knK' then TyAbbBind(tyT, Some knK)
            else error fi "Kind of binding does not match declared kind"
    
    let prbindingty ctx b = 
        match b with
        | NameBind -> ()
        | TyVarBind tyS -> 
            (pr "<: "
             printTy ctx tyS)
        | VarBind tyT -> 
            (pr ": "
             printTy ctx tyT)
        | TyAbbBind(tyT, knKopt) -> 
            pr ":: "
            match knKopt with
            | None -> printKn ctx (kindOf ctx tyT)
            | Some knK -> printKn ctx knK
        | TmAbbBind(t, tyTopt) -> 
            pr ": "
            match tyTopt with
            | None -> printTy ctx (typeOf ctx t)
            | Some tyT -> printTy ctx tyT
    
    let rec processInput parsedCommand input (ctx, store) = 
        setOutput parsedCommand
        let (cmds, _) = parseInput input ctx
        
        let g (ctx, store) c = 
            open_hvbox 0
            let results = processCommand parsedCommand (ctx, store) c
            print_flush ()
            results
        List.fold g (ctx, store) cmds
    
    and processCommand parsedCommand (ctx, store) cmd = 
        match cmd with
        | Import f -> processInput parsedCommand (Source.File f) (ctx, store)
        | Eval(_, t) -> 
            let tyT = typeOf ctx t
            let (t', store) = eval ctx store t
            printTerm true ctx t'
            print_break 1 2
            pr ": "
            printTy ctx tyT
            force_newline()
            (ctx, store)
        | Bind(fi, x, bind) -> 
            let bind = checkbinding fi ctx bind
            let (bind', store') = evalBinding ctx store bind
            pr x
            pr " "
            prbindingty ctx bind'
            force_newline()
            ((addBinding ctx x bind'), (shiftStore 1 store'))
        | SomeBind(fi, tyX, x, t) -> 
            let tyT = typeOf ctx t
            match lcst ctx tyT with
            | TySome(_, tyBound, tyBody) -> 
                let (t', store') = eval ctx store t
                
                let b = 
                    match t' with
                    | TmPack(_, _, t12, _) -> TmAbbBind(termShift 1 t12, Some tyBody)
                    | _ -> VarBind tyBody
                
                let ctx1 = addBinding ctx tyX (TyVarBind tyBound)
                let ctx2 = addBinding ctx1 x b
                pr tyX
                force_newline()
                pr x
                pr " : "
                printTy ctx1 tyBody
                force_newline()
                (ctx2, store')
            | _ -> error fi "existential type expected"
