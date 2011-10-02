import System.Environment
import Control.Monad
import qualified Data.Map as Map

main = do
        files <- getArgs
        g     <- if files == [] 
                           then (fmap (\a->[a]) getContents)
                           else mapM readFile files
        let dict = Map.toList (Map.unionsWith (+) (map (wordFreq . words) g))
        putStrLn $ histogram dict


histogram :: (Show w, Num n) => [(w, n)] -> String
histogram a0 = concat $ map lineTemplate a0

lineTemplate :: (Show a, Show b) => (a, b) -> String
lineTemplate (a, b) = (show a) ++ "\t" ++ (show b) ++ "\n"

wordFreq :: (Ord k, Num n) => [k] -> Map.Map k n
wordFreq list0 = foldl (\m a -> Map.insertWith' (+) a 1 m) Map.empty list0