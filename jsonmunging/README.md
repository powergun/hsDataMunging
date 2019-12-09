# JSON data munging

## Decode & Encode standard data types

source: <https://tech.fpcomplete.com/haskell/library/aeson>

## Decode any json document to Value type

see [./src/AnyTypes/Value.hs](./src/AnyTypes/Value.hs)

I don't have to predefine the data type - it the values are decoded
as raw map `fromList`

this is similar to python/ruby's decode-as-dictionary

## Decode & Encode Defined Data Types

see minimal example:
[./src/DataTypes/Person.hs](./src/DataTypes/Person.hs)
