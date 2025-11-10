{-# LANGUAGE DeriveGeneric #-}

module Domain.Domain where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Control.Concurrent (MVar)
import qualified Data.Map as M
import qualified Network.WebSockets as WS

-- Dinh nghia tin nhan chat
data Message = Message
  { senderId   :: Text
  , senderName :: Text
  , content    :: Text
  , timestamp  :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message


-- Dinh nghia cac loai tin nhan tren WebSocket
data WsMessage =
    SendChatMessage Message
  | BroadcastMessage Message
  deriving (Show, Generic)

instance FromJSON WsMessage
instance ToJSON WsMessage


-- Dinh nghia Client Connection
type ClientId = Int
type ClientConnection = WS.Connection
type ClientMap = M.Map ClientId ClientConnection
type ClientConnections = MVar ClientMap

-- Trang thai (State) cua toan bo ung dung
data AppState = AppState
  { appClients :: ClientConnections
  , appNextId  :: MVar ClientId
  }