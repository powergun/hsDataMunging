import qualified StandardTypes.List
import qualified StandardTypes.Scalar

import qualified AnyTypes.JSONTypes
import qualified AnyTypes.KeyVisitor
import qualified AnyTypes.StringVisitor
import qualified AnyTypes.Value

import qualified DataTypes.DeriveGeneric
import qualified DataTypes.EncodeDecode
import qualified DataTypes.OptionalFields
import qualified DataTypes.SimpleDecompose
import qualified DataTypes.Value

import qualified Exceptions.EitherParse

import qualified Parsing.Applicative
import qualified Parsing.FromScratch
import qualified Parsing.WithParsing

main :: IO ()
main = do
  StandardTypes.Scalar.demo
  StandardTypes.List.demo

  AnyTypes.Value.demo
  AnyTypes.JSONTypes.demo
  AnyTypes.StringVisitor.demo
  AnyTypes.KeyVisitor.demo

  DataTypes.DeriveGeneric.demo
  DataTypes.EncodeDecode.demo
  DataTypes.Value.demo
  DataTypes.SimpleDecompose.demo
  DataTypes.OptionalFields.demo

  Exceptions.EitherParse.demo

  Parsing.FromScratch.demo
  Parsing.WithParsing.demo
  Parsing.Applicative.demo
