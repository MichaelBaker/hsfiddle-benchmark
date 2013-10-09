import Prelude hiding               (log)
import Network                      (PortID (PortNumber), HostName (), PortNumber (), listenOn, accept, withSocketsDo)
import Network.Socket               (close)
import Data.HashMap.Strict          (HashMap (), empty, insert, elems)
import Control.Concurrent           (forkIO)
import Control.Concurrent.STM.TVar  (TVar (), newTVarIO, readTVar, modifyTVar')
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan, readTChan)
import Control.Monad.STM            (atomically)
import Safe                         (readMay)
import System.IO                    (Handle (), IOMode (WriteMode), hClose, hFlush, stdout, hPutStrLn, hGetLine, withFile)
import Data.UUID                    (toString)
import Data.UUID.V4                 (nextRandom)

import Types

type Client   = (Handle, HostName, PortNumber)
type Clients  = HashMap String Client
type TClients = TVar Clients

log handle message = hPutStrLn handle message >> hFlush handle

main = withSocketsDo $ do
  withFile "errors.log"   WriteMode $ \errorLog    -> do
  withFile "activity.log" WriteMode $ \activityLog -> do
    clients   <- newTVarIO empty
    responses <- newTChanIO
    socket    <- listenOn $ PortNumber 8337
    forkIO $ acceptConnections socket clients responses errorLog
    forkIO $ processResponses responses activityLog
    acceptCommands clients errorLog
    clientHandles clients >>= mapM_ hClose

processResponses responses activityLog = do
  nextResponse <- atomically $ readTChan responses
  processResponse nextResponse
  processResponses responses activityLog
  where processResponse Result = log activityLog "Got a result"

acceptConnections socket clients responses errorLog = do
  (handle, hostname, port) <- accept socket
  atomically $ modifyTVar' clients (insert (show handle) (handle, hostname, port))
  forkIO     $ acceptResponses handle responses errorLog
  acceptConnections socket clients responses errorLog

acceptResponses handle responses errorLog = do
  newResponse <- hGetLine handle
  case readMay newResponse of
    Nothing       -> log errorLog ("Invalid response from worker: " ++ newResponse)
    Just response -> atomically $ writeTChan responses response
  acceptResponses handle responses errorLog

acceptCommands clients errorLog = do
  putStr "-> "
  hFlush stdout
  line <- getLine
  case readMay line of
    Nothing      -> log errorLog ("Unrecognized command '" ++ line ++ "'") >> acceptCommands clients errorLog
    Just Exit    -> return ()
    Just E       -> return ()
    Just Quit    -> return ()
    Just Q       -> return ()
    Just command -> processCommand command >> acceptCommands clients errorLog
  where processCommand Start = do
          jobId <- randomJobId
          sendToClients (StartWork jobId) clients


sendToClients :: ForemanToWorkerCommand -> TClients -> IO ()
sendToClients message clients = do
  handles <- clientHandles clients
  mapM_ (sendMessage message) handles
  where sendMessage message clientHandle = hPutStrLn clientHandle (show message) >> hFlush clientHandle

clientHandles clients = atomically $ do
  clientConnections <- readTVar clients
  return $ map handle (elems clientConnections)

handle (a, _, _) = a

randomJobId :: IO JobId
randomJobId = nextRandom >>= return . JobId . toString
