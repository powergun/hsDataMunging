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

### Create JSON AST in Haskell

see [./src/AnyTypes/JSONTypes.hs](./src/AnyTypes/JSONTypes.hs)

source: <https://artyom.me/aeson>

This can be useful for quick prototyping without defining the
concrete Haskell data type

### How to traverse JSON AST - string visitor and key visitor

see: [./src/AnyTypes/StringVisitor.hs](./src/AnyTypes/StringVisitor.hs)

source: <https://artyom.me/aeson>

An Object is a HashMap; an Array is a Vector; a String is a Text;
a Number is Scientific;

I took a step further from the example and created KeyVisitor example:
[./src/AnyTypes/KeyVisitor.hs](./src/AnyTypes/KeyVisitor.hs);

this shows how to selectively action on the `Object` using a predicate
function

### How to process arbitrarily named fields

see: [./src/AnyTypes/UnnamedFields.hs](./src/AnyTypes/UnnamedFields.hs)

source: <https://artyom.me/aeson>

if the structure stores the arbitrarily named fields as key-value pair,
such as:

```text
motion: .../amc
puppet: .../asf
model: .../obj
texture: .../tiff
```

I can utilize the HashMap-to-list builtin mechanism

## Decode & Encode Defined Data Types

see minimal example: [./src/DataTypes/DeriveGeneric.hs](./src/DataTypes/DeriveGeneric.hs)

source: <https://tech.fpcomplete.com/haskell/library/aeson>

this is the simplest approach to make a data type jsonified; the entire
`shellast` project is based on this machinary in order to export the
bash script ast

to implement the decode and encode function myself, see minimal example:
[./src/DataTypes/EncodeDecode.hs](./src/DataTypes/EncodeDecode.hs)

### how to treat optional fields

see: [./src/DataTypes/OptionalFields.hs](./src/DataTypes/OptionalFields.hs)

see also below for how to use Aeson's Value type as the field type,
which allows me to unparse a sub-section of the JSON document;

optional field must have a default value which is handled by special
operators

## Decode & Encode to Aeson Value | How to inspect Aeson Value

see: [./src/DataTypes/Value.hs](./src/DataTypes/Value.hs)

source: <https://hackernoon.com/flexible-data-with-aeson-d8a23ba2169e>

to traverse the json AST I may need additional libraries (lens-aeson)

inspired by <https://stackoverflow.com/questions/28629290/how-to-inspect-parsed-aeson-value>

aeson official document provides a simple example of decomposing the
"fromList" representation of a JSON value:
<https://hackage.haskell.org/package/aeson-1.4.6.0/docs/Data-Aeson.html#g:20>

### Use JSON Value as-is without parsing (for a sub-section)

see: [./src/DataTypes/SimpleDecompose.hs](./src/DataTypes/SimpleDecompose.hs)

this example shows how to partially parse (or un-parse) a sub-structure
of the given json document by giving a field `Value (Aeson.Value)` type;

after decoding stage, such field is represented by `HM.fromList` or
other `Aeson.Value` data constructors; I can encode them back to the
ByteString if the intention is to leave them alone; if such field is
optional in the source Json document I need to use `Data.Maybe.fromMaybe`
to give it a default empty value (but NOT `Nothing`) during decoding.

this can be useful in the cases where I need to modify a sub-section
(such as sort the content) but preserve the rest - this means I don't
care of rest of the data representation but use their JSON representation as
they are; see `hsSrcMing/vsws` for a tool to sort the visual studio
code workspace file (without touching the whole workspace-setting
sub section)

NOTE: (I was stuck on this for a while) the type of the un-parsed
sub section must be `Value` (`Object` is the value ctor not the data
type!!)

## Exception | Handle parsing failures

`eitherDecode` function returns the detail of the failure - type
incompatibity etc.

## Parsing

source: <https://artyom.me/aeson>

> A parser is something that turns a JSON value into something else
> that you need (record, list of tuples, etc).

### FromScratch - the inner working of parsing

see: [./src/Parsing/FromScratch.hs](./src/Parsing/FromScratch.hs)

use `parseMaybe` to apply a parser to a value

### with- parsring: avoid manual type check

### alternative- parsing: try with a different key

see [./src/Parsing/Alternative.hs](./src/Parsing/Alternative.hs)

using `Control.Applicative.(<|>)`

> You could choose from a list of parsers, even, with the asum function
> from Data.Foldable (which is generalised choice from various parsing libraries)

I can test various types of conditions and decide whether to "fall back"
to a different parsing strategy.

I can also implement a parser that supports multiple data-ctors (via
asum-based fallback mechanism)

```haskell
data Weapon = M16 { damage :: Int, attachment :: String}
            | M9 { damage :: Int }
```

## Rediscover Aeson and Parsing, First Principles P/968

it shows the use of QuasiQuote, very helpful, see:
src/AnyTypes/StringVisitor.hs
