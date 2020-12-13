namespace ProtoConvertors

type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.TrafficLight) : Domain.TrafficLight =
        enum<Domain.TrafficLight>(int x)
    static member ToProtobuf (x:Domain.TrafficLight) : ProtoClasses.Domain.TrafficLight =
        enum<ProtoClasses.Domain.TrafficLight>(int x)
    static member FromProtobuf (x:ProtoClasses.Domain.Crossroad) : Domain.Crossroad =
        {
            Id = x.Id
            Street1 = x.Street1
            Street2 = x.Street2
            Light = ConvertDomain.FromProtobuf x.Light
            LightStatus =
                match x.LightStatusCase with
                | ProtoClasses.Domain.Crossroad.LightStatusOneofCase.LightStatusNormal -> Domain.LightStatus.Normal
                | ProtoClasses.Domain.Crossroad.LightStatusOneofCase.LightStatusWarning -> Domain.LightStatus.Warning (x.LightStatusWarning.ErrorsCount)
                | ProtoClasses.Domain.Crossroad.LightStatusOneofCase.LightStatusOutOfOrder -> Domain.LightStatus.OutOfOrder (x.LightStatusOutOfOrder.Since.ToDateTimeOffset())
                | _ -> Domain.LightStatus.Unknown
        }
    static member ToProtobuf (x:Domain.Crossroad) : ProtoClasses.Domain.Crossroad =
        let v = ProtoClasses.Domain.Crossroad()
        v.Id <- x.Id
        v.Street1 <- x.Street1
        v.Street2 <- x.Street2
        v.Light <- ConvertDomain.ToProtobuf x.Light
        match x.LightStatus with
        | Domain.Normal ->  v.LightStatusNormal <- ProtoClasses.Domain.LightStatus__Normal()
        | Domain.Warning errorsCount -> v.LightStatusWarning <- ProtoClasses.Domain.LightStatus__Warning(ErrorsCount = errorsCount)
        | Domain.OutOfOrder since -> v.LightStatusOutOfOrder <- ProtoClasses.Domain.LightStatus__OutOfOrder(Since = Google.Protobuf.WellKnownTypes.Timestamp.FromDateTimeOffset since)
        | Domain.Unknown -> ()
        v