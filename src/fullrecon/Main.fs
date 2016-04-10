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
  
let processFile f (ctx, nextuvar, constr) =
    Common.alreadyImported := f :: !Common.alreadyImported
    let (cmds, _) = parseFile f ctx 
    let g (ctx, nextuvar, constr) c =
        open_hvbox 0
        let results = processCommand (ctx, nextuvar, constr) c
        print_flush ()
        results
    List.fold g (ctx, nextuvar, constr) cmds
  
let main () =
    let inFile = Common.parseArgs () in
    processFile inFile (emptycontext, uvargen, emptyconstr) |> ignore
  
Common.runMain main
