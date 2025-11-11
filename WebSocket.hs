{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Maybe (mapMaybe)

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
    return $ M.insert clientId (conn, Nothing) clientMap
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
  mapM_ (\(conn, _) -> WS.sendTextData conn payload) (M.elems clientMap)
  putStrLn $ "Broadcasting message to " ++ show (M.size clientMap) ++ " clients."

sendMessageToUser :: T.Text -> WsMessage -> AppState -> IO ()
sendMessageToUser userId message appState = do
  clientMap <- readMVar (appClients appState)
  let payload = encode message
  
  -- Tìm kết nối của người nhận
  let receiverConn = findConnectionByUserId userId clientMap
  case receiverConn of
    Just conn -> do
      WS.sendTextData conn payload
      putStrLn $ "Sent private message to " ++ T.unpack userId
    Nothing ->
      putStrLn $ "Could not find user " ++ T.unpack userId ++ " to send message."

-- HÀM MỚI: Helper để tìm Connection từ UserId
findConnectionByUserId :: T.Text -> ClientMap -> Maybe ClientConnection
findConnectionByUserId userId clientMap =
  -- Tìm trong danh sách các clientInfo
  let found = filter (\(_, (_, mUserId)) -> mUserId == Just userId) (M.toList clientMap)
  in case found of
    -- Lấy ra connection (phần tử đầu tiên của clientInfo)
    ((_, (conn, _)):_) -> Just conn
    _ -> Nothing

-- HÀM MỚI: Helper để định danh client
identifyClient :: ClientId -> T.Text -> AppState -> IO ()
identifyClient clientId userId appState = do
  modifyMVar_ (appClients appState) $ \clientMap ->
    -- Cập nhật client info, gán UserId
    return $ M.adjust (\(conn, _) -> (conn, Just userId)) clientId clientMap
  putStrLn $ "Client " ++ show clientId ++ " identified as " ++ T.unpack userId

-- Main client handler
handleClient :: ClientId -> WS.Connection -> AppState -> IO ()
handleClient clientId conn appState = forever $ do
  bytes <- WS.receiveData conn
  putStrLn $ "Received raw message from client " ++ show clientId
  
  case decode bytes of
    -- 1. Xử lý tin nhắn ĐỊNH DANH
    Just (Identify userId) -> do
      identifyClient clientId userId appState
      
    -- 2. Xử lý tin nhắn CHAT CHUNG
    Just (SendChatMessage message) -> do
      -- Tạo tin nhắn cuối cùng với receiverId là Nothing
      let finalMessage = message { receiverId = Nothing }
      
      putStrLn $ "Decoded GROUP message from " ++ show clientId
      
      -- Kiểm tra kết quả lưu CSDL
      result <- liftIO $ (try (runDb $ insert_ $ ChatMessage
        { chatMessageSenderId = senderId finalMessage
        , chatMessageSenderName = senderName finalMessage
        , chatMessageContent = content finalMessage
        , chatMessageTimestamp = timestamp finalMessage
        , chatMessageReceiverId = receiverId finalMessage
        }) :: IO (Either SomeException ()))

      -- Chỉ broadcast NẾU lưu thành công
      case result of
        Left (ex :: SomeException) -> do
          -- Báo lỗi nghiêm trọng trên console
          putStrLn $ "!!! ERROR SAVING TO DB (GROUP): " ++ show ex
        
        Right () -> do
          -- Lưu thành công, bây giờ mới broadcast
          putStrLn "Saved GROUP message to DB successfully."
          broadcast (BroadcastMessage finalMessage) appState
      
-- 3. Xử lý tin nhắn CHAT RIÊNG
    Just (SendPrivateMessage toUserId message) -> do
      -- Tạo tin nhắn cuối cùng với receiverId là toUserId
      let finalMessage = message { receiverId = Just toUserId }

      putStrLn $ "Decoded PRIVATE message from " ++ show clientId ++ " to " ++ T.unpack toUserId
      
      -- SỬA LẠI: Kiểm tra kết quả lưu CSDL
      result <- liftIO $ (try (runDb $ insert_ $ ChatMessage
        { chatMessageSenderId = senderId finalMessage
        , chatMessageSenderName = senderName finalMessage
        , chatMessageContent = content finalMessage
        , chatMessageTimestamp = timestamp finalMessage
        , chatMessageReceiverId = receiverId finalMessage
        }) :: IO (Either SomeException ()))

      -- Chỉ broadcast NẾU lưu thành công
      case result of
        Left (ex :: SomeException) -> do
          -- Báo lỗi nghiêm trọng trên console
          putStrLn $ "!!! ERROR SAVING TO DB (PRIVATE): " ++ show ex

        Right () -> do
          -- Lưu thành công, bây giờ mới broadcast
          putStrLn "Saved PRIVATE message to DB successfully."
          sendMessageToUser toUserId (BroadcastMessage finalMessage) appState
          sendMessageToUser (senderId finalMessage) (BroadcastMessage finalMessage) appState

    -- 4. Xử lý lỗi
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