namespace TaplCommon

open FSharpx.Choice
open Argu

module CommandLine = 

    let (|Success|Failure|) = function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

    type Source =
        | File of string
        | Console of string
        | NoSource

    type Target =
        | File of string
        | Console

    type ParsedCommand =
        {
        Usage : string
        Source : Source
        Target : Target
        ErrorMsg: string option
        }

    type CLIArguments =
        | [<AltCommandLine("-i")>] Input of string
        | [<AltCommandLine("-o")>] Output of string
        | [<AltCommandLine("-s")>] ConsoleInput of string 
  
         with
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | Input _ -> "(optional) file path to process"
                    | Output _ -> "(optional, not implemented) output path"
                    | ConsoleInput _ -> "input from console"

    let parseCommandLine argv = 

        try
            Success (ArgumentParser.Create<CLIArguments>().Parse argv)
        with e ->
            match e with
            | :? System.ArgumentException -> Failure e.Message
            | _ -> raise e
             
    let parseTarget (commandLine : ParseResults<CLIArguments>) = 

        let targetList = 
            []
            |> (fun x -> 
                    if commandLine.Contains <@ Output @> then 
                        match commandLine.TryGetResult <@ Output @> with
                        | Some path -> (Target.File path)::x
                        | None -> x
                    else x)

        match targetList with
        | [] -> Success Target.Console
        | [x] -> Success x
        | hd::tl -> Failure (sprintf "more than one output target specified: %s, %s" (hd.ToString()) (tl.Head.ToString()))

    let parseSource (commandLine : ParseResults<CLIArguments>) = 

        let sourceList = 
            []
            |> (fun x -> 
                    if commandLine.Contains <@ Input @> then 
                        match commandLine.TryGetResult <@ Input @> with
                        | Some path -> (Source.File path)::x
                        | None -> x
                        
                    else x)
            |> (fun x -> 
                    if commandLine.Contains <@ ConsoleInput @> then 
                        match commandLine.TryGetResult <@ ConsoleInput @> with
                        | Some consoleInput -> (Source.Console consoleInput)::x
                        | None -> x
                    else x)

        match sourceList with
        | [] -> Success Source.NoSource
        | [x] -> Success x
        | hd::tl ->Failure (sprintf "more than one input source specified: %s, %s" (hd.ToString()) (tl.Head.ToString()))
        
    let parse argv = 

        match choose { 
                        let! commandLine = parseCommandLine argv
                       
                        let! target = parseTarget commandLine
                        let! source = parseSource commandLine

                        return 
                            {
                            Usage = commandLine.Usage()
                            Source = source
                            Target = target
                            ErrorMsg = None
                            } 
                        } with
        | Success x -> x
        | Failure msg -> 
            let commandLine = ArgumentParser.Create<CLIArguments>()
            {
            Usage = commandLine.Usage()
            Source = Source.NoSource
            Target = Target.Console 
            ErrorMsg = Some msg
            } 

    let reportEerror (parsedCommand : ParsedCommand) =
        
        match parsedCommand.ErrorMsg with
        | Some msg ->
            printfn "%s" msg
            printfn " "
        | None -> ()

        printfn "%s" parsedCommand.Usage

module List =
    let assoc a (l : ('a * 'b) list) =
        
        let rec loop (l' : ('a * 'b) list) =
            match l' with
            | [] -> None
            | (a', b')::tl ->
                if a' = a then Some b'
                else loop tl
        loop l

open FSharp.Compatibility.OCaml.Format

module Common =

    exception NoRuleAppliesException
    exception NotFoundException

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

