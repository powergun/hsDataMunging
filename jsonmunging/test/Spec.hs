import qualified StandardTypes.List
import qualified StandardTypes.Scalar

import qualified AnyTypes.Value

import qualified DataTypes.Person

main :: IO ()
main = do
  StandardTypes.Scalar.demo
  StandardTypes.List.demo

  AnyTypes.Value.demo
  DataTypes.Person.demo
