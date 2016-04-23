namespace FSharpTapl

open Ast
open Core
open CommandLine

module SimpleBoolLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list 
