﻿namespace FSharpTapl

open Ast
open CommandLine

module FomegaLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
