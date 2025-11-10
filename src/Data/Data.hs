-- KHAI BAO LANGUAGE EXTENSIONS (DE SUA LOI)
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Data 
    ( ChatMessage(..)
    , migrateDb
    , runDb
    , EntityField(..)
    , Key(..)
    , migrateAll
    ) where

import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT) -- Them LoggingT
import Database.Persist.Sqlite
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Data.Time (UTCTime)
import Data.Text (Text)

-- Define the database schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ChatMessage
    senderId   Text
    senderName Text
    content    Text
    timestamp  UTCTime
    deriving Show
|]


-- Function to run database operations
runDb :: ReaderT SqlBackend (LoggingT IO) a -> IO a
runDb action = runStdoutLoggingT $
    withSqlitePool "chat.db" 10 $ \pool ->
        flip runSqlPool pool action

-- Function to run database migrations
migrateDb :: IO ()
migrateDb = runDb $ runMigration migrateAll