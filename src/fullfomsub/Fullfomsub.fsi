namespace FSharpTapl

open Ast
open CommandLine

module Fullfomsub =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list
