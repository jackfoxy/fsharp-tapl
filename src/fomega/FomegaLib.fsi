namespace FSharpTapl

open Ast
open CommandLine

module FomegaLib =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list
