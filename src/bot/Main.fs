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

namespace FSharpTapl

open Ast
open BotLib
open CommandLine

module console1 =
    [<EntryPoint>]
    let main argv = 

        let parsedCommand = parse argv

        match parsedCommand.Source with
        | NoSource -> 
            reportError parsedCommand
        | input -> 
            let main () =
                processInput parsedCommand input emptyContext |> ignore

            Common.runMain main
            ()

        0
