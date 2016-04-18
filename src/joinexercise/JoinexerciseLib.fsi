namespace FSharpTapl

open Ast
open CommandLine

module JoinexerciseLib =

    val processInput : input : Source -> ctx : Context  -> (string * Binding) list 
