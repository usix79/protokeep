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
    | Warning of errorsCount: int*level: int
    | OutOfOrder of since: System.DateTime

    static member MakeUnknownKey () = Key.Value "0"
    static member MakeNormalKey () = Key.Value "1"
    static member MakeWarningKey () = Key.Value "2"
    static member MakeOutOfOrderKey () = Key.Value "3"
    interface IEntity with
        member x.Key =
            match x with
            | LightStatus.Unknown -> LightStatus.MakeUnknownKey ()
            | LightStatus.Normal -> LightStatus.MakeNormalKey ()
            | LightStatus.Warning (errorsCount', level') -> LightStatus.MakeWarningKey ()
            | LightStatus.OutOfOrder (since') -> LightStatus.MakeOutOfOrderKey ()


type Crossroad = {
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
    Nickname : string voption
    Img : byte array
    Notes : string array
    Props : Map<string,string>
}
with
    static member Default: Lazy<Crossroad> =
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
            Nickname = ValueNone
            Img = Array.empty
            Notes = Array.empty
            Props = Map.empty
        }

    static member MakeKey (id': int) =
        Key.Value (id'.ToString())

    interface IEntity with
        member x.Key = Crossroad.MakeKey (x.Id)

