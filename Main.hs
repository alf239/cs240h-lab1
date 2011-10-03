import System.Environment
import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Monoid (mappend)

main = do
        files <- getArgs
        g     <- if null files
                    then fmap (: []) getContents
                    else mapM readFile files
        let dict = Map.toList (wordFreq (map sToLowerCase (concatMap cleanWords g)))
        putStrLn $ histogram (sortBy bySecond dict)

sToLowerCase = map toLower

bySecond :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
bySecond (a1, b1) (a2, b2) = compare b2 b1 `mappend` compare a2 a1

histogram :: (Show w, Num n) => [(w, n)] -> String
histogram = concatMap (\(a, b) -> show a ++ "\t" ++ show b ++ "\n")

wordFreq :: (Ord k, Num n) => [k] -> Map.Map k n
wordFreq = foldl' (\m a -> Map.insertWith' (+) a 1 m) Map.empty

isNotWordPart :: Char -> Bool
isNotWordPart c = not (isAlpha c || c == '\'' || c == '-')

cleanWords :: String -> [String]
cleanWords s =  case dropWhile isNotWordPart s of
                        "" -> []
                        s' -> w : cleanWords s''
                              where (w, s'') =
                                     break isNotWordPart s'
