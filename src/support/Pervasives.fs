(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

[<AutoOpen>]
module Support.Pervasives

open FSharp.Compatibility.OCaml


type info = Error.info

let mutable buffer = ""

let pr = Format.print_string

//    fun (s : string) ->
//        match buffer.Length, (s.Substring(0)) with
//        | 0, _ ->
//            buffer <- s
//        | _, " " -> 
//        if buffer.Length > 0 then
//            if buffer.EndsWith ()
//
//    printf "%s"
