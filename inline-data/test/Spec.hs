import qualified Herestring.InterpolatedHere
import qualified Herestring.InterpolatedNeat
import qualified Herestring.StaticByteStr
import qualified Herestring.StaticStr
import qualified Herestring.StaticText

main :: IO ()
main = do
  Herestring.InterpolatedNeat.demo
  Herestring.InterpolatedHere.demo
  Herestring.StaticByteStr.demo
  Herestring.StaticStr.demo
  Herestring.StaticText.demo
