namespace FSharpTapl

open Ast
open CommandLine

module Fullfomsub =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
