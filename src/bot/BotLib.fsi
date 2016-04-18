namespace FSharpTapl

open Ast
open CommandLine

module BotLib =

    val processInput : input : Source -> ctx : Context -> (string * Binding) list