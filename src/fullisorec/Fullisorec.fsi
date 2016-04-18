namespace FSharpTapl

open Ast
open CommandLine

module Fullisorec =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list
