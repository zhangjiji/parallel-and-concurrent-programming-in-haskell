import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.Parallel.Strategies
import Control.DeepSeq

parMap' :: (a->b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (x:xs) = do
  b <- rpar (f x)
  bs <- parMap' f xs
  return (b:bs)

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = runEval (parMap' solve puzzles)

  print (length (filter isJust solutions))


-- ghc -O2 sudoku3.hs -threaded -rtsopts -eventlog
-- ./sudoku3 sudoku17.1000.txt +RTS -N2 -l
