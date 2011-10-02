import System.Environment
import Control.Monad
import qualified Data.Map as Map

main = do
        s   <- getArgs
        g   <- mapM readFile s
        let dict = Map.unionsWith (+) (map (wordFreq . words) g)
        print dict

wordFreq list0 = wordCount Map.empty list0
                 where wordCount acc []    = acc
                       wordCount acc (h:t) = wordCount (Map.insertWith' (+) h 1 acc) t