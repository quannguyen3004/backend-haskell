{-# LANGUAGE OverloadedStrings #-}

module App.WebSocket (
  application,
  newAppState,
  app
) where

import Domain.Domain
import Data.Data (runDb, ChatMessage(..))
import Control.Exception (finally, try, SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar, readMVar, modifyMVar_, modifyMVar, newMVar)
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import Database.Persist.Sqlite (insert_)

-- Initialize application state
newAppState :: IO AppState
newAppState = do
  clients <- newMVar M.empty
  nextId <- newMVar 0
  return $ AppState clients nextId

-- Add new client
addClient :: ClientConnection -> AppState -> IO ClientId
addClient conn appState = modifyMVar (appNextId appState) $ \clientId -> do
  let newId = clientId + 1
  modifyMVar_ (appClients appState) $ \clientMap -> do
    return $ M.insert clientId conn clientMap
  putStrLn $ "Client " ++ show clientId ++ " connected."
  return (newId, clientId) 

-- Remove client
removeClient :: ClientId -> AppState -> IO ()
removeClient clientId appState = modifyMVar_ (appClients appState) $ \clientMap -> do
  putStrLn $ "Client " ++ show clientId ++ " disconnected."
  return $ M.delete clientId clientMap

-- Broadcast message
broadcast :: WsMessage -> AppState -> IO ()
broadcast message appState = do
  clientMap <- readMVar (appClients appState)
  let payload = encode message 
  mapM_ (\conn -> WS.sendTextData conn payload) (M.elems clientMap)
  putStrLn $ "Broadcasting message to " ++ show (M.size clientMap) ++ " clients."


-- Main client handler
handleClient :: ClientId -> WS.Connection -> AppState -> IO ()
handleClient clientId conn appState = forever $ do
  bytes <- WS.receiveData conn
  putStrLn $ "Received raw message: " ++ show bytes
  case decode bytes of
    -- Handle chat message
    Just (SendChatMessage message) -> do
      putStrLn $ "Decoded message from " ++ show clientId ++ ": " ++ show message
      
      -- Save message to database
      putStrLn "Attempting to save message to database..."
      result <- liftIO $ try $ runDb $ insert_ $ ChatMessage
        { chatMessageSenderId = senderId message
        , chatMessageSenderName = senderName message
        , chatMessageContent = content message
        , chatMessageTimestamp = timestamp message
        }
      case result of
        Left e -> putStrLn $ "Database error: " ++ show (e :: SomeException)
        Right _ -> putStrLn "Message saved to database successfully"
      
      -- Broadcast message to all clients
      broadcast (BroadcastMessage message) appState
      
    -- Handle invalid message format
    Nothing -> do
      putStrLn $ "Received invalid message from " ++ show clientId

-- Main WebSocket application
application :: AppState -> WS.ServerApp
application appState pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.withPingThread conn 30 (return ()) $ do  -- Ping mỗi 30 giây
    clientId <- addClient conn appState
    finally
      (handleClient clientId conn appState)
      (removeClient clientId appState)

-- Khởi tạo ứng dụng WebSocket với options mặc định
app :: IO ()
app = do
  state <- newAppState
  putStrLn "WebSocket server starting on port 9160..."
  putStrLn "Accepting connections from all interfaces..."
  WS.runServer "0.0.0.0" 9160 (application state)