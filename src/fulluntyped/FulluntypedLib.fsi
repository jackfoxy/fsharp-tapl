namespace FSharpTapl

open Ast
open CommandLine

module FulluntypedLib =

    val processInput : input : Source -> ctx : Context  -> (string * Binding) list 

