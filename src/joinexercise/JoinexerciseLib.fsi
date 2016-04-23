namespace FSharpTapl

open Ast
open CommandLine

module JoinexerciseLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  -> (string * Binding) list 
