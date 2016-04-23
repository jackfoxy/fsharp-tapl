(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

namespace FSharpTapl

open Ast
open CommandLine
open Core
open Fullfomsubref

module console1 =
    [<EntryPoint>]
    let main argv = 

        let parsedCommand = parse argv

        match parsedCommand.Source with
        | NoSource -> 
            reportEerror parsedCommand
        | input -> 
            let main () =
                processInput parsedCommand input (emptycontext, emptystore) |> ignore

            Common.runMain main
            ()
        
        | NoSource -> 
            CommandLine.reportEerror parsedCommand

        0