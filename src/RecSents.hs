import Control.Applicative
import Data.List.Split
import FUtil
import System.Environment
import System.Posix.Signals
import System.Process

recSents :: Int -> [String] -> [String] -> IO ()
recSents _ [] _ = return ()
recSents i sentenceNames sentenceTexts = do
    let sentenceName = sentenceNames !! i
    putStrLn $ sentenceName ++ ":"
    putStrLn $ sentenceTexts !! i
    putStrLn $ "Enter to record, q to quit" ++
        (if i > 0 then ", p or r to play or redo previous" else "") ++
        ", n to skip ahead:"
    let wtf = (putStrLn "XXX Invalid command. XXX" >>)
        tryInp = do
            inp <- getLine
            case inp of
              "q" -> return ()
              "p" ->
                if i > 0
                  then do
                    _ <- runInteractiveProcess "mp"
                        [(sentenceNames !! (i - 1)) ++ ".wav"]
                        Nothing Nothing
                    recSents i sentenceNames sentenceTexts
                  else wtf tryInp
              "r" ->
                if i > 0
                  then recSents (i - 1) sentenceNames sentenceTexts
                  else wtf tryInp
              "n" -> recSents (i + 1) sentenceNames sentenceTexts
              "" -> do
                (_, _, _, pId) <-
                    runInteractiveProcess "arecord" [sentenceName ++ ".wav"]
                    Nothing Nothing
                putStrLn "<<< Press enter when done recording. >>>"
                inp2 <- getLine
                interruptProcessGroupOf pId
                case inp2 of
                  "r" -> recSents i sentenceNames sentenceTexts
                  _ -> recSents (i + 1) sentenceNames sentenceTexts
              _ -> wtf tryInp
    tryInp

onIndex :: Int -> (a -> a) -> [a] -> [a]
onIndex n f l = take n l ++ [f $ l !! n] ++ drop (n + 1) l

main :: IO ()
main = do
    args <- getArgs
    let startN = case args of
          [] -> 1
          [nStr] -> read nStr
          _ -> error "Usage: recSents <sentenceNumberToStartFrom>"
        sentenceNames = concatMap
            (\ sentenceName -> [sentenceName ++ "e", sentenceName ++ "f"]) $
            map (padl '0' 5 . show) [1 :: Int ..]
        sentFile = "/home/danl/mass-sentence-method/cmn/list.txt"
    sentenceTexts <-
        map (unlines . take 3) .
        concatMap (\ a -> [onIndex 0 (">>>" ++) a, onIndex 1 (">>>" ++) a]) .
        chunksOf 4 . lines <$> readFile sentFile
    _ <- installHandler sigINT Ignore Nothing
    recSents (2 * (startN - 1)) sentenceNames sentenceTexts
