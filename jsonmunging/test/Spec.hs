import qualified StandardTypes.List
import qualified StandardTypes.Scalar

import qualified AnyTypes.Value

import qualified DataTypes.DeriveGeneric
import qualified DataTypes.EncodeDecode
import qualified DataTypes.SimpleDecompose
import qualified DataTypes.Value

import qualified Exceptions.EitherParse

main :: IO ()
main = do
  StandardTypes.Scalar.demo
  StandardTypes.List.demo

  AnyTypes.Value.demo

  DataTypes.DeriveGeneric.demo
  DataTypes.EncodeDecode.demo
  DataTypes.Value.demo
  DataTypes.SimpleDecompose.demo

  Exceptions.EitherParse.demo

