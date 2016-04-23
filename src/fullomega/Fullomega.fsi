namespace FSharpTapl

open Ast
open CommandLine

module Fullomega =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context *  Store : Term list -> Context * Term list

