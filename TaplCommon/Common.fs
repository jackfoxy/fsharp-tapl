namespace TaplCommon

open Microsoft.FSharp.Text
open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.Format
open Core

module Common =

    let searchpath = ref [""]

    let argDefs = [
      "-I",
          Arg.String (fun f -> searchpath := f :: !searchpath),
          "Append a directory to the search path"]

    let parseArgs () =
        let inFile : string option ref = ref None
        Arg.parse argDefs
         (fun s ->
           match !inFile with
           | Some _ -> err "You must specify exactly one input file"
           | None -> inFile := Some s)
         ""
        match !inFile with
        | Some s -> s
        | None ->
            err "You must specify an input file"

    let openfile infile =
        let rec trynext l =
            match l with
            | [] -> err ("Could not find " ^ infile)
            | d :: rest ->
                let name = if d = "" then infile else (d ^ "/" ^ infile)
                try open_in name
                with Sys_error m ->
                    trynext rest
        trynext !searchpath

    let alreadyImported = ref ([] : string list)

