import qualified StandardTypes.List
import qualified StandardTypes.Scalar

import qualified AnyTypes.JSONTypes
import qualified AnyTypes.KeyVisitor
import qualified AnyTypes.StringVisitor
import qualified AnyTypes.Value
import qualified AnyTypes.UnnamedFields

import qualified DataTypes.DeriveGeneric
import qualified DataTypes.DeriveGenericSimpler
import qualified DataTypes.DeriveGenericTH
import qualified DataTypes.EncodeDecode
import qualified DataTypes.OptionalFields
import qualified DataTypes.OptionalFieldsDeriving
import qualified DataTypes.SimpleDecompose
import qualified DataTypes.Value

import qualified Exceptions.EitherParse

import qualified Parsing.Alternative
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
  AnyTypes.UnnamedFields.demo

  DataTypes.DeriveGeneric.demo
  DataTypes.DeriveGenericSimpler.demo
  DataTypes.DeriveGenericTH.demo
  DataTypes.EncodeDecode.demo
  DataTypes.Value.demo
  DataTypes.SimpleDecompose.demo
  DataTypes.OptionalFields.demo
  DataTypes.OptionalFieldsDeriving.demo

  Exceptions.EitherParse.demo

  Parsing.FromScratch.demo
  Parsing.WithParsing.demo
  Parsing.Applicative.demo
  Parsing.Alternative.demo
