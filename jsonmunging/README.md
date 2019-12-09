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

see minimal example: [./src/DataTypes/Person.hs](./src/DataTypes/Person.hs)

source: <https://tech.fpcomplete.com/haskell/library/aeson>

to implement the decode and encode function myself
