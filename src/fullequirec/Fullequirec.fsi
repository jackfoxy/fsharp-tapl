namespace FSharpTapl

open Ast
open CommandLine

module Fullequirec =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list

