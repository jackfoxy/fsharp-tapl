namespace FSharpTapl

open Ast
open CommandLine

module FullpolyLib =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list

