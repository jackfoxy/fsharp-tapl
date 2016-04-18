namespace FSharpTapl

open Ast
open CommandLine

module FomsubLib =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list
