module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import App.WebSocket (application, newAppState)
import Network.Wai (Application, responseBuilder)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Builder (byteString)
import qualified Data.Data as DB (migrateDb)

-- Định nghĩa cổng Server
serverPort :: Int
serverPort = 9160

main :: IO ()
main = do
  putStrLn "Initializing database..."
  DB.migrateDb
  
  putStrLn $ "Starting WebSocket server on http://localhost:" ++ show serverPort
  
  -- Initialize application state
  appState <- newAppState
  
  -- Run server with CORS enabled
  run serverPort $ simpleCors $
    websocketsOr
      defaultConnectionOptions
      (application appState)
      backupApp

-- Ứng dụng dự phòng khi không kết nối được WebSocket
backupApp :: Application
backupApp _ respond = respond $ 
  responseBuilder
    status200
    [(hContentType, BS.pack "text/plain")]
    (byteString $ BS.pack "WebSocket server is running. Please connect via WebSocket protocol.")