namespace FSharpTapl

open Ast
open CommandLine

module EquirecLib =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list
