import System.Environment
import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Monoid (mappend)
import Data.Ratio

main = do
        files <- getArgs
        g     <- if null files
                    then fmap (: []) getContents
                    else mapM readFile files
        let dict = Map.toList (wordFreq (map sToLowerCase (concatMap cleanWords g)))
        putStrLn $ histogram (sortBy bySecond dict) 80

sToLowerCase = map toLower

bySecond :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
bySecond (a1, b1) (a2, b2) = compare b2 b1 `mappend` compare a2 a1

scale :: Int -> Int -> Int -> Int
scale a l m = floor (a * l % m)

histogramLine :: String -> Int -> Int -> String
histogramLine _ _  0   = ""
histogramLine w wl val = w ++ replicate (wl - length w) ' ' ++ replicate val '#' ++ "\n"

histogram :: [(String, Int)] -> Int -> String
histogram k m = concatMap (\(a, b) -> histogramLine a wl (scale b hl hm)) k
            where wl = maximum (map (length . fst) k)
                  hm = maximum (map snd k)
                  hl = m - wl
                  
wordFreq :: Ord k => [k] -> Map.Map k Int
wordFreq = foldl' (\m a -> Map.insertWith' (+) a 1 m) Map.empty

isNotWordPart :: Char -> Bool
isNotWordPart c = not (isAlpha c || c == '\'' || c == '-')

cleanWords :: String -> [String]
cleanWords s =  case dropWhile isNotWordPart s of
                        "" -> []
                        s' -> w : cleanWords s''
                              where (w, s'') =
                                     break isNotWordPart s'
