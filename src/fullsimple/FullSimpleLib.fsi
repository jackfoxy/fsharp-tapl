﻿namespace FSharpTapl

open Ast
open CommandLine

module FullSimpleLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  -> (string * Binding) list 
