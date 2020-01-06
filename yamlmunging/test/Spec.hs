import qualified ConfigDemo
import qualified SimpleDemo

main :: IO ()
main = do
  SimpleDemo.demo
  ConfigDemo.demo
