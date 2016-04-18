namespace FSharpTapl

module CommandLine = 
    
    type Source =
        | File of string
        | Console of string
        | NoSource

    type Target =
        | File of string
        | Console

    type ParsedCommand =
        {
        Usage : string
        Source : Source
        Target : Target
        ErrorMsg: string option
        }

    val parse : argv : string [] -> ParsedCommand

    val reportEerror : ParsedCommand -> unit

module List =
    val assoc : a : 'a  -> l : ('a * 'b) list -> 'b option when 'a : equality

module Common =

    exception NoRuleAppliesException
    exception NotFoundException

    val runMain : main : (unit -> unit) -> 'a