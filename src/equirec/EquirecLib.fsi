namespace FSharpTapl

open Ast
open CommandLine

module EquirecLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
