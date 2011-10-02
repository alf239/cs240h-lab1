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
histogram a0 = hist "" a0 
               where hist a' []    = a'
                     hist a' (h:t) = hist (a' ++ (show w) ++ "\t" ++ (show c) ++ "\n") t
                                     where (w, c) = h

wordFreq :: (Ord k, Num n) => [k] -> Map.Map k n
wordFreq list0 = foldl (\m a -> Map.insertWith' (+) a 1 m) Map.empty list0