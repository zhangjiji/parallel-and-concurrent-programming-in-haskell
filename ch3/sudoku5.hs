import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.Parallel.Strategies
import Control.DeepSeq

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = map solve puzzles `using` parList rseq

  print (length (filter isJust solutions))
