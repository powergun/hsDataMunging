import qualified StandardTypes.List
import qualified StandardTypes.Scalar

import qualified AnyTypes.Value

import qualified DataTypes.DeriveGeneric
import qualified DataTypes.EncodeDecode

main :: IO ()
main = do
  StandardTypes.Scalar.demo
  StandardTypes.List.demo

  AnyTypes.Value.demo

  DataTypes.DeriveGeneric.demo
  DataTypes.EncodeDecode.demo
