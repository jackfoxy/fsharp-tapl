﻿namespace FSharpTapl

open Ast
open CommandLine

module BotLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context -> (string * Binding) list