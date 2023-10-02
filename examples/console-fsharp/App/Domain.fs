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
    | Warning of errorsCount:int
    | OutOfOrder of since:System.DateTime
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeNormalKey () = Key.Value "1"
    static member MakeWarningKey () = Key.Value "2"
    static member MakeOutOfOrderKey () = Key.Value "3"
    interface IEntity with
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
