namespace Protogen
module FsharpTypes =

    type Key =
        | Value of string
        | Items of Key list
        | Inner of Key

    let (|TryFind|_|) f key = f key