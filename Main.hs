module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import App.WebSocket (application, newAppState)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Data.Data as DB (migrateDb)

-- Servant Imports
import Servant
import App.APIs (API, server) -- Import API từ file APIs.hs

-- Định nghĩa cổng Server
serverPort :: Int
serverPort = 9160

-- Định nghĩa Proxy cho API
apiProxy :: Proxy API
apiProxy = Proxy

main :: IO ()
main = do
  putStrLn "Initializing database..."
  DB.migrateDb
  
  putStrLn $ "Starting Server (API + WebSocket) on http://localhost:" ++ show serverPort
  
  -- Khởi tạo AppState
  appState <- newAppState
  
  -- Tạo ứng dụng Servant (API)
  -- 'simpleCors' rất quan trọng để React (localhost:3000) có thể gọi
  let servantApp = simpleCors $ serve apiProxy server
  
  -- Chạy server, kết hợp cả WebSocket và Servant
  run serverPort $
    websocketsOr
      defaultConnectionOptions      -- Tùy chọn WS
      (application appState)        -- Ứng dụng WebSocket
      servantApp                    -- Ứng dụng dự phòng (chính là API của bạn)