namespace FSharpTapl

open Ast
open CommandLine

module FullrefLib =

    val processInput : input : Source -> ctx : Context  * store : Term list -> (string * Binding) list * Term list
