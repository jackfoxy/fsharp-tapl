﻿namespace FSharpTapl

open Ast
open CommandLine

module FullerrorLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list
