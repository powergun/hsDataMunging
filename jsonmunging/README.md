# JSON data munging

## Decode & Encode standard data types

source: <https://tech.fpcomplete.com/haskell/library/aeson>

## Decode any json document to Value type

see [./src/AnyTypes/Value.hs](./src/AnyTypes/Value.hs)

source: <https://tech.fpcomplete.com/haskell/library/aeson>

> Working with a arbitrary JSON data
> Sometimes you want to work with the JSON data without actually
> converting it to some specific type first. One way to do this is
> to work with the JSON abstract > syntax tree (AST). This can be
> done by simply decoding it to a Value

I don't have to predefine the data type - it the values are decoded
as raw map `fromList`

this is similar to python/ruby's decode-as-dictionary

### JSON AST

> Once you have a JSON value, you can define a Parser like we did
> in parseJSON above, and run it using parse, parseEither or parseMaybe.
> Decoding JSON to a specific Haskell type is actually a two-step
> process - first, the JSON string is converted to a Value, and then
> the FromJSON instance is used to convert that Value to the specific type.

## Decode & Encode Defined Data Types

see minimal example: [./src/DataTypes/DeriveGeneric.hs](./src/DataTypes/DeriveGeneric.hs)

source: <https://tech.fpcomplete.com/haskell/library/aeson>

this is the simplest approach to make a data type jsonified; the entire
`shellast` project is based on this machinary in order to export the
bash script ast

to implement the decode and encode function myself, see minimal example:
[./src/DataTypes/EncodeDecode.hs](./src/DataTypes/EncodeDecode.hs)

## Decode & Encode to Aeson Value | How to inspect Aeson Value

see: [./src/DataTypes/Value.hs](./src/DataTypes/Value.hs)

inspired by <https://stackoverflow.com/questions/28629290/how-to-inspect-parsed-aeson-value>
