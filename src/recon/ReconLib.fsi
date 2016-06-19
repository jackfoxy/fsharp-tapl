namespace FSharpTapl

open Ast
open Core
open CommandLine

module ReconLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context * nxtuvar : (unit -> NextUVar) * constr : (Ty * Ty) list -> (string * Binding) list * (unit -> NextUVar) * (Ty * Ty) list 
