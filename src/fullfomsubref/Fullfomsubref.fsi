namespace FSharpTapl

open Ast
open CommandLine

module Fullfomsubref =

    val processInput : input : Source -> ctx : Context *  Store : Term list -> Context * Term list

