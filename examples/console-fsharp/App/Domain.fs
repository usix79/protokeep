namespace Domain

open Protokeep.FsharpTypes

type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3

type LightStatus =
    | Unknown
    | Normal
    | Warning of errorsCount: int
    | OutOfOrder of since: System.DateTime

    static member MakeUnknownKey () = Key.Value "0"
    static member MakeNormalKey () = Key.Value "1"
    static member MakeWarningKey () = Key.Value "2"
    static member MakeOutOfOrderKey () = Key.Value "3"
    member x.Key =
        match x with
        | LightStatus.Unknown -> LightStatus.MakeUnknownKey ()
        | LightStatus.Normal -> LightStatus.MakeNormalKey ()
        | LightStatus.Warning (errorsCount') -> LightStatus.MakeWarningKey ()
        | LightStatus.OutOfOrder (since') -> LightStatus.MakeOutOfOrderKey ()

type Crossroad = {
    Id : int
    Street1 : string
    Street2 : string
    Light : TrafficLight
    LightStatus : LightStatus
    History : LightStatus list
    Lirycs : string list
}
with
    static member Default: Lazy<Crossroad> =
        lazy {
            Id = 0
            Street1 = ""
            Street2 = ""
            Light = TrafficLight.Unknown
            LightStatus = LightStatus.Unknown
            History = List.empty
            Lirycs = List.empty
        }

type Crossroad2 = {
    Id : int
    LongId : int64
    AltId : System.Guid
    Street1 : string
    Street2 : string
    IsMonitored : bool
    Xpos : float32
    Ypos : float
    Ratio : decimal
    LastChecked : System.DateTime
    ServiceInterval : System.TimeSpan
    CurrentLight : TrafficLight
    Nickname : string option
    Img : byte array
    Notes : string array
    Props : Map<string,string>
}
with
    static member Default: Lazy<Crossroad2> =
        lazy {
            Id = 0
            LongId = 0L
            AltId = System.Guid.Empty
            Street1 = ""
            Street2 = ""
            IsMonitored = false
            Xpos = 0.f
            Ypos = 0.
            Ratio = 0m
            LastChecked = System.DateTime.MinValue
            ServiceInterval = System.TimeSpan.Zero
            CurrentLight = TrafficLight.Unknown
            Nickname = None
            Img = Array.empty
            Notes = Array.empty
            Props = Map.empty
        }

