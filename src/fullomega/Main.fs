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
open FSharp.Compatibility.OCaml
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
  
let checkbinding fi ctx b =
  match b with
  | NameBind -> NameBind
  | TyVarBind knK -> TyVarBind knK
  | TyAbbBind (tyT, None) -> TyAbbBind (tyT, Some (kindof ctx tyT))
  | VarBind tyT -> VarBind tyT
  | TmAbbBind (t, None) -> TmAbbBind (t, Some (typeof ctx t))
  | TmAbbBind (t, (Some tyT)) ->
      let tyT' = typeof ctx t
      in
        if tyeqv ctx tyT' tyT
        then TmAbbBind (t, Some tyT)
        else error fi "Type of binding does not match declared type"
  | TyAbbBind (tyT, (Some knK)) ->
      let knK' = kindof ctx tyT
      in
        if knK = knK'
        then TyAbbBind (tyT, Some knK)
        else error fi "Kind of binding does not match declared kind"
  
let prbindingty ctx b =
  match b with
  | NameBind -> ()
  | TyVarBind knK -> (pr ": "; printkn ctx knK)
  | VarBind tyT -> (pr ": "; printty ctx tyT)
  | TyAbbBind (tyT, knK_opt) ->
      (pr ":: ";
       (match knK_opt with
        | None -> printkn ctx (kindof ctx tyT)
        | Some knK -> printkn ctx knK))
  | TmAbbBind (t, tyT_opt) ->
      (pr ": ";
       (match tyT_opt with
        | None -> printty ctx (typeof ctx t)
        | Some tyT -> printty ctx tyT))
  
let rec process_command (ctx, store) cmd =
  match cmd with
  | Eval (fi, t) ->
      let tyT = typeof ctx t in
      let (t', store) = eval ctx store t
      in
        (printtm_ATerm true ctx t';
         print_break 1 2;
         pr ": ";
         printty ctx tyT;
         force_newline ();
         (ctx, store))
  | Bind (fi, x, bind) ->
      let bind = checkbinding fi ctx bind in
      let (bind', store') = evalbinding ctx store bind
      in
        (pr x;
         pr " ";
         prbindingty ctx bind';
         force_newline ();
         ((addbinding ctx x bind'), (shiftstore 1 store')))
  | SomeBind (fi, tyX, x, t) ->
      let tyT = typeof ctx t
      in
        (match simplifyty ctx tyT with
         | TySome (_, knK, tyBody) ->
             let (t', store') = eval ctx store t in
             let b =
               (match t' with
                | TmPack (_, _, t12, _) ->
                    TmAbbBind (termShift 1 t12, Some tyBody)
                | _ -> VarBind tyBody) in
             let ctx1 = addbinding ctx tyX (TyVarBind knK) in
             let ctx2 = addbinding ctx1 x b
             in
               (pr tyX;
                force_newline ();
                pr x;
                pr " : ";
                printty ctx1 tyBody;
                force_newline ();
                (ctx2, store'))
         | _ -> error fi "existential type expected")
  
let process_file f (ctx, store) =
  (Common.alreadyImported := f :: !Common.alreadyImported;
   let (cmds, _) = parseFile f ctx in
   let g (ctx, store) c =
     (open_hvbox 0;
      let results = process_command (ctx, store) c
      in (print_flush (); results))
   in List.fold g (ctx, store) cmds)
  
let main () =
  let inFile = Common.parseArgs () in
  let _ = process_file inFile (emptycontext, emptystore) in ()
  
let () = set_max_boxes 1000
  
let () = set_margin 67
  
let res = Printexc.catch (fun () -> try (main (); 0) with | Exit x -> x) ()
  
let () = print_flush ()
  
let () = exit res
  

