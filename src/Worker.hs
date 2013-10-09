import Network            (PortID (PortNumber), withSocketsDo, connectTo)
import System.IO          (Handle (), hFlush, hPutStrLn, hGetLine)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Safe               (readMay)

import Types

main = withSocketsDo $ do
  commandLineArguments <- getArgs
  print commandLineArguments
  let host = commandLineArguments !! 0
      port = read $ commandLineArguments !! 1
  handle <- connectTo host (PortNumber $ fromIntegral port)
  acceptCommands handle

acceptCommands handle = do
  newCommand <- hGetLine handle
  case readMay newCommand of
    Nothing      -> putStrLn ("Invalid command from foreman: " ++ newCommand)
    Just command -> processCommand command
  acceptCommands handle
  where processCommand (StartWork jobid) = print jobid >> sendToForeman Result handle

sendToForeman :: WorkerToForemanCommand -> Handle -> IO ()
sendToForeman message handle = hPutStrLn handle (show message) >> hFlush handle
