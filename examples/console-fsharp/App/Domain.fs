module rec Domain
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type LightStatus =
    | Unknown
    | Normal
    | Warning of errorsCount:int
    | OutOfOrder of since:System.DateTimeOffset
type Crossroad = {
    Id : int
    Street1 : string
    Street2 : string
    Light : Domain.TrafficLight
    LightStatus : Domain.LightStatus
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
    LastChecked : System.DateTimeOffset
    ServiceInterval : System.TimeSpan
    CurrentLight : Domain.TrafficLight
    Nickname : string option
    Img : byte array
    Notes : string array
    Props : Map<string,string>
}
