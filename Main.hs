{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp ( run )
import Network.JsonRpc.Server
import Network.HTTP.Types (status200, hContentType)
import Data.Aeson
import Control.Monad.IO.Class ( liftIO ) 
import Control.Monad.Reader ( ReaderT )
import Control.Monad.Logger (runStderrLoggingT)
import GHC.Generics ( Generic )
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name String
    deriving Show
|]

type Name = String
type Id   = Key User

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id

addUser :: ConnectionPool -> Name -> IO Id 
addUser pool name = flip runSqlPersistMPool pool $ do
  insert $ User name 

addUserRpc :: ConnectionPool -> Method IO
addUserRpc pool = toMethod "add_user" (handler pool) (Required "name" :+: ()) 
  where
    handler :: ConnectionPool -> Name -> RpcResult IO Id
    handler pool = liftIO . addUser pool 

allUsers :: ConnectionPool -> IO [User]
allUsers pool =  flip runSqlPersistMPool pool $ do
  users <- selectList [] [] 
  pure $ fmap entityVal users

allUsersRpc :: ConnectionPool -> Method IO
allUsersRpc pool = toMethod "all_users" (handler pool) ()
  where
    handler :: ConnectionPool -> RpcResult IO [User]
    handler pool = liftIO $ allUsers pool

methods = [ addUserRpc, allUsersRpc]

application :: ConnectionPool -> Application
application pool = \ req respond -> do
  body <- strictRequestBody req
  maybeResponseBody <- call ( fmap ($ pool) methods) body
  case maybeResponseBody of
    Just b -> respond $
      responseLBS status200 [(hContentType, "text/plain")] b
    Nothing -> respond $
      responseLBS status200 [(hContentType, "text/plain")] ""

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createSqlitePool "idcar.db" 5
  runSqlPersistMPool ( runMigration migrateAll ) pool 
  liftIO $ run 8000 $ application pool
