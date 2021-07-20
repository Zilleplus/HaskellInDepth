import Data.Char
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

-- (word in text, number of occurences)
type Entry = (T.Text, Int)

type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab txt = 
    map buildEntry $ group $ sort ws
    where
        ws = map T.toCaseFold $ filter (not . T.null)
                 $ map  cleanWord $ T.words txt
        buildEntry g = (head g, length g)
        cleanWord = (T.dropAround $ not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords ws = 
    TIO.putStrLn $ T.unwords $ map fst ws

processTextFile :: FilePath -> IO ()
processTextFile tf = do
    txt <- TIO.readFile tf
    let voc = extractVocab txt
    printAllWords voc
    print $ length voc

main = do
    [fname] <- getArgs
    processTextFile fname
