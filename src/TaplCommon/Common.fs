namespace TaplCommon

open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.Format
open Core

module List =
    let assoc a (l : ('a * 'b) list) =
        
        let rec loop (l' : ('a * 'b) list) =
            match l' with
            | [] -> None
            | (a', b')::tl ->
                if a' = a then Some b'
                else loop tl
        loop l

module Common =

    exception NoRuleAppliesException
    exception NotFoundException

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
            | [] -> err ("Could not find " + infile)
            | d :: rest ->
                let name = if d = "" then infile else (d + "/" + infile)
                try open_in name
                with Sys_error m ->
                    trynext rest
        trynext !searchpath

    let alreadyImported = ref ([] : string list)

    let runMain (main : unit -> unit) =

        set_max_boxes 1000
        set_margin 67
        
        let res =
            try 
                (fun () -> 
                    try 
                        main ()
                        0 
                    with | Exit x -> x) ()
            with e ->
                printfn "%A" e
                exit 2
  
        print_flush ()
  
        exit res

