namespace FSharpTapl

open Ast
open CommandLine

module FullSimpleLib =

    val processInput : input : Source -> ctx : Context  -> (string * Binding) list 
