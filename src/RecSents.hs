import FUtil
import System.Environment
import System.Posix.Signals
import System.Process

recSents :: Int -> [String] -> IO ()
recSents _ [] = return ()
recSents i sIds = do
    let sId = sIds !! i
    putStrLn $
        sId ++ " next: Enter to record, q to quit" ++
        if i > 0 then ", p or r to play or redo previous:" else ":"
    let wtf = (putStrLn "XXX Invalid command. XXX" >>)
        tryInp = do
            inp <- getLine
            case inp of
              "q" -> return ()
              "p" ->
                if i > 0
                  then do
                    runInteractiveProcess "mp" [(sIds !! (i - 1)) ++ ".wav"]
                        Nothing Nothing
                    recSents i sIds
                  else wtf tryInp
              "r" -> if i > 0 then recSents (i - 1) sIds else wtf tryInp
              "" -> do
                (_, _, _, pId) <-
                    runInteractiveProcess "arecord" [sId ++ ".wav"]
                    Nothing Nothing
                putStrLn "<<< Press enter when done recording. >>>"
                inp2 <- getLine
                interruptProcessGroupOf pId
                case inp2 of
                  "r" -> recSents i sIds
                  _ -> recSents (i + 1) sIds
              _ -> wtf tryInp
    tryInp

main :: IO ()
main = do
    args <- getArgs
    let startN = case args of
          [] -> 1
          [nStr] -> read nStr
          _ -> error "Usage: recSents <sentenceNumberToStartFrom>"
    installHandler sigINT Ignore Nothing
    recSents 0 . concatMap (\ sId -> [sId ++ "e", sId ++ "f"]) $
        map (padl '0' 5 . show) [startN..]
