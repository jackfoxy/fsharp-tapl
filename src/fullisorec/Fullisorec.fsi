namespace FSharpTapl

open Ast
open CommandLine

module Fullisorec =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
