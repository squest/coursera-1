import Data.List
import System.Environment (getArgs)

intWith fn inFile outFile = do
  input <- readFile inFile
  writeFile outFile (fn input)

main = mainWith myFun
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> intWith function input output
            _ -> putStrln "error: bla bla bla"
