import System.Environment
import Control.Monad
import Data.Map

main = do
        s   <- getArgs
        g   <- mapM readFile s
        let dict = Prelude.map words g
        print $ show dict

wordFreq list0 = wordCount empty list0
                 where wordCount acc []    = acc
                       wordCount acc (h:t) = wordCount (insertWith' (+) h 1 acc) t