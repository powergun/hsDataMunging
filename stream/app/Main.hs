module Main where

-- stack script --resolver lts-12.21
import           Conduit

demoTestStream :: IO ()
demoTestStream = do
  -- Pure operations: summing numbers.
  print $ runConduitPure $ yieldMany [1..10] .| sumC

  -- Exception safe file access: copy a file.
  writeFile "input.txt" "This is a test." -- create the source file
  runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" -- actual copying
  readFile "output.txt" >>= putStrLn -- prove that it worked

  -- Perform transformations.
  print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList

demoTestYield :: IO ()
demoTestYield = do
    putStrLn "List version:"
    print $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList

main :: IO ()
main = do
  demoTestStream
  demoTestYield
