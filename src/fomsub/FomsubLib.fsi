﻿namespace FSharpTapl

open Ast
open CommandLine

module FomsubLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
