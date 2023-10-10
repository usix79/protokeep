namespace Test.Domain

open Protokeep.FsharpTypes

type Crossroad = {
    Id : int
    AltId : System.Guid
    Address : string
    Corner : string voption
    IsMonitored : bool
    Patch : sbyte
    Model : int16
    Serial : int
    Mask : int64
    Cost : decimal
    Xpos : float32
    Ypos : float
    LastChecked : System.DateTime
    ServiceInterval : System.TimeSpan
    Intervals : int list
    Notes : string array
    Tags : Map<string,int>
    Next : Crossroad voption
    Img : byte array
    mutable Version : int
}
with
    static member Default: Lazy<Crossroad> =
        lazy {
            Id = 0
            AltId = System.Guid.Empty
            Address = ""
            Corner = ValueNone
            IsMonitored = false
            Patch = 0uy
            Model = 0s
            Serial = 0
            Mask = 0L
            Cost = 0m
            Xpos = 0.f
            Ypos = 0.
            LastChecked = System.DateTime.MinValue
            ServiceInterval = System.TimeSpan.Zero
            Intervals = List.empty
            Notes = Array.empty
            Tags = Map.empty
            Next = ValueNone
            Img = Array.empty
            Version = 0
        }

    static member MakeKey (id': int) =
        Key.Value (id'.ToString())

    interface IEntity with
        member x.Key = Crossroad.MakeKey (x.Id)

    interface IVersioned with
        member x.Version
            with get () = x.Version
            and set v = x.Version <- v

