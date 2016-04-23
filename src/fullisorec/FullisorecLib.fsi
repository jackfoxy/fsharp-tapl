namespace FSharpTapl

open Ast
open CommandLine

module FullisorecLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
