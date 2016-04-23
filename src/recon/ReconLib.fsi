namespace FSharpTapl

open Ast
open Core
open CommandLine

module ReconLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context * nxtuvar : (unit -> Nextuvar) * constr : (Ty * Ty) list -> (string * Binding) list * (unit -> Nextuvar) * (Ty * Ty) list 
