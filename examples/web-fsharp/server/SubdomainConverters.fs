namespace Protogen.FsharpConverters
type ConvertDomainSubdomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.Subdomain.Status) : Domain.Subdomain.Status =
        enum<Domain.Subdomain.Status>(int x)
    static member ToProtobuf (x:Domain.Subdomain.Status) : ProtoClasses.Domain.Subdomain.Status =
        enum<ProtoClasses.Domain.Subdomain.Status>(int x)
