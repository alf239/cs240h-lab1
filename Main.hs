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
        let dict = buildFreqDictionary g
        putStrLn $ histogram (sortBy bySecond dict) 80

-- | Builds a frequency dictionary given a list of input Strings ("texts")
-- We take a list of strings in order to avoid merging frequence dictionaries later on
buildFreqDictionary :: [String] -> [(String, Int)]
buildFreqDictionary g = Map.toList (wordFreq (map sToLowerCase (concatMap cleanWords g)))

-- | Canonic representation of the word
-- Can be extended later on if we decide, e.g., merge together "tomorrow" and "to-morrow"
sToLowerCase = map toLower

-- | Ordering for our histogram
-- Currently, inverted ordering by frequency, then by word
bySecond :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
bySecond (a1, b1) (a2, b2) = compare b2 b1 `mappend` compare a1 a2

-- | Scale first parameter to be represented on the histogram 
-- of a given length with a given max value of the dataset
scale :: Int -> Int -> Int -> Int
scale a l m = floor (a * l % m)

-- | A template for histogram line
-- Skips words with empty histogram, which may well be inefficient:
-- we should try filtering elements that are < (max/80) before attempting to render
histogramLine :: String -> Int -> Int -> String
histogramLine _ _  0   = ""
histogramLine w wl val = w ++ replicate (wl - length w) ' ' ++ replicate val '#' ++ "\n"

-- | Renders the histogram for a frequency dictionary 
-- or for any (label, value) list with max length passed
-- as a second argument
histogram :: [(String, Int)] -> Int -> String
histogram k m = concatMap (\(a, b) -> histogramLine a wl (scale b hl hm)) k
            where wl = maximum (map (length . fst) k)
                  hm = maximum (map snd k)
                  hl = m - wl

-- | Builds a frequency dictionary for a list of values
-- It expects that we already have words splitted and put to canonic form
wordFreq :: Ord k => [k] -> Map.Map k Int
wordFreq = foldl' (\m a -> Map.insertWith' (+) a 1 m) Map.empty

-- | Definition of what constitutes a part of word
-- It's context-unaware at the moment, and quite dumb
isNotWordPart :: Char -> Bool
isNotWordPart c = not (isAlpha c || c == '\'' || c == '-')

-- | A wersion of 'words' which allows "don't" and "de-interlacing"
-- Plain copy of 'words' with 'isSpace' replaced with 'isNotWordPart'
-- (My apologies for the "Not" in name: we never use it without negation)
cleanWords :: String -> [String]
cleanWords s =  case dropWhile isNotWordPart s of
                        "" -> []
                        s' -> w : cleanWords s''
                              where (w, s'') =
                                     break isNotWordPart s'
