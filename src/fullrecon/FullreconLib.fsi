namespace FSharpTapl

open Ast
open Core
open CommandLine

module FullreconLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  * nextuvar : (unit -> NextUvar) * constr : (Ty * Ty) list -> (string * Binding) list * (unit -> NextUvar) * (Ty * Ty) list


