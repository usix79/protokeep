module Protogen.Types

type ComplexName = ComplexName of string list

type Type =
    | Bool
    | String
    | Int
    | Long
    | Decimal of scale: int
    | Float
    | Double
    | Bytes
    | Timespamp
    | Duration
    | Guid
    | Optional of value: Type
    | Array of value: Type
    | Map of value: Type
    | Complex of name: ComplexName

type EnumInfo = { Name: string; Symbols: string list }
type FieldInfo = { Name: string;  Type: Type }
type RecordInfo = { Name: string;  Fields: FieldInfo list }
type UnionCaseInfo = {Name: string; Fields: FieldInfo list}
type UnionInfo = {Name: string;  Cases: UnionCaseInfo list}

type ModuleItem =
    | Enum of EnumInfo
    | Record of RecordInfo
    | Union of UnionInfo

type Module = {
    Name: ComplexName
    Items: ModuleItem list
}