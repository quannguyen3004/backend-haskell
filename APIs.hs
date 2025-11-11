{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.APIs (API, server) where

import Database.Persist ((||.)) -- Thêm '||.' để dùng cho toán tử OR
import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Data (runDb, ChatMessage(..), EntityField(..), Key)
import Database.Persist (selectList, (==.), Entity(..), entityVal)

-- Định nghĩa API
type API = "messages" :> Get '[JSON] [ChatMessage]  -- Lấy tất cả tin nhắn
      :<|> "messages" :> Capture "userId" Text :> Get '[JSON] [ChatMessage]  -- Lấy tin nhắn theo userId

-- Server implementation
server :: Server API
server = getAllMessages :<|> getMessagesByUser
  where
    getAllMessages :: Handler [ChatMessage]
    getAllMessages = liftIO $ runDb $ do
      entities <- selectList [] []
      return $ map entityVal entities

    getMessagesByUser :: Text -> Handler [ChatMessage]
    getMessagesByUser userId = liftIO $ runDb $ do
      entities <- selectList
        ([ChatMessageSenderId ==. userId] ||. [ChatMessageReceiverId ==. Just userId])
        []
      return $ map entityVal entities