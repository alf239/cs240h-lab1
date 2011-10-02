import System.Environment
import Control.Monad
import qualified Data.Map as Map

main = do
        files <- getArgs
        g     <- if files == [] 
                           then (fmap (\a->[a]) getContents) 
                           else mapM readFile files
        let dict = Map.unionsWith (+) (map (wordFreq . words) g)
        putStrLn $ (histogram . Map.toList) dict

histogram :: (Show w, Num n) => [(w, n)] -> String
histogram []    = ""
histogram (h:t) = (show a) ++ "\t" ++ (show b) ++ "\n" ++ (histogram t)
                  where (a, b) = h

wordFreq :: (Ord k, Num n) => [k] -> Map.Map k n
wordFreq list0 = wordCount Map.empty list0
                 where wordCount acc []    = acc
                       wordCount acc (h:t) = wordCount (Map.insertWith' (+) h 1 acc) t