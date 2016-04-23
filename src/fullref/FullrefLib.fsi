namespace FSharpTapl

open Ast
open CommandLine

module FullrefLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  * store : Term list -> (string * Binding) list * Term list
