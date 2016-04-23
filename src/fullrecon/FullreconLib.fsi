namespace FSharpTapl

open Ast
open Core
open CommandLine

module FullreconLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  * nextuvar : (unit -> Nextuvar) * constr : (Ty * Ty) list -> (string * Binding) list * (unit -> Nextuvar) * (Ty * Ty) list


