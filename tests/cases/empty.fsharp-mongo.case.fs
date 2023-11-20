namespace Test.Converters

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertTestDomain() =

type ConvertTestDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(GuidSerializer(GuidRepresentation.Standard))
