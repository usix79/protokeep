# Protokeep

![Build status](https://github.com/usix79/protokeep/workflows/build/badge.svg)

Open-source library purpose-built to streamline the development in data-centric architectures.

Problems addressed:

- **Schema Compatibility**: Seamless evolution of data schemas without breaking changes.
- **Boilerplate Reduction**: Minimizes redundant code for serialization and deserialization.
- **Composite Key Handling**: Efficiently creates keys for composite data types.

 Current scope:

- **Languages and Tools**: Fsharp, Protobuf, Json, MongoDb, Fable

## Key Features

### 1. Domain Type Descriptions

- **Simple Types**: Basic data types like `int`, `string`, `bool`, etc. for common use cases.
  
- **Generic Types**: Types like `optional`, `array`, `list` and `map` which can hold other types.

- **Enums**: A set of named values.

- **Algebraic Types**:
  - `record`: product type for bundling values.
  - `union`: add type with the unique case's identifiers
  
- **Modifiers**:
  - `struct`: Ensures the type is referenced by value.
  - `key`: Denotes a field as part of the main identifier for a record.
  - `idx`: Specifies fields that can be accessed using an indexer.
  - `Version`: For managing transaction logic in the storage layers.

### 2. Schema Evolution Checking

Ensures your data model adapts while preserving backward and forward compatibility.

### 3. Domain Type Generation

Currently, Protokeep supports type generation for **FSharp**.

### 4. Data Schema Generation

Presently, the only supported data schema format is **Protobuf**.

### 5. Converters

Automatically produce converters for popular serialization libraries:

- `Google.Protobuf`
- `System.Text.Json`
- `MongoDb.Bson`
- `Fable.SimpleJson`

## Getting Started

TODO
