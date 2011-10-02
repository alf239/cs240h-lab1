import System.Environment
import Control.Monad
import qualified Data.Map as Map
import Data.List

main = do
        files <- getArgs
        g     <- if files == [] 
                           then fmap (: []) getContents
                           else mapM readFile files
        let dict = Map.toList (Map.unionsWith (+) (map (wordFreq . words) g))
        putStrLn $ histogram dict


histogram :: (Show w, Num n) => [(w, n)] -> String
histogram = concatMap (\(a, b) -> show a ++ "\t" ++ show b ++ "\n")

wordFreq :: (Ord k, Num n) => [k] -> Map.Map k n
wordFreq = foldl' (\m a -> Map.insertWith' (+) a 1 m) Map.empty