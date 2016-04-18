namespace FSharpTapl

open Ast
open CommandLine

module Fullomega =

    val processInput : input : Source -> ctx : Context *  Store : Term list -> Context * Term list

