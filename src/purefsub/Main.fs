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
open PurefsubLib 

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