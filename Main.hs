{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.DateTime
import qualified Data.ByteString.Char8 as B
import Control.Lens
import Text.Regex.Posix
import qualified Data.Text as T
import System.Environment
import Web.Twitter.Conduit ( call , update , inReplyToStatusId , StreamingAPI ( SStatus ) ) 
import Web.Twitter.Conduit.Stream
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types ( statusId , statusText , statusUser , userScreenName )
import Web.Authenticate.OAuth
import Control.Monad.Logger
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Database.Persist ( insert_ , selectList , SelectOpt ( Desc ) , Entity ( Entity ) )
import Database.Persist.Sqlite ( runSqlite , runMigration )
import Database.Persist.TH

share [ mkPersist sqlSettings , mkMigrate "migrateAll" ] [persistLowerCase|
Memo
    memo T.Text
    time DateTime default=CURRENT_TIME
|]

dbName = "memo.sqlite3"

tokens = do
  ck <- getEnv "OAUTH_CONSUMER_KEY"
  cs <- getEnv "OAUTH_CONSUMER_SECRET"
  return $ twitterOAuth
    { oauthConsumerKey = B.pack ck
    , oauthConsumerSecret = B.pack cs
    }

credential = do
  token <- getEnv "OAUTH_TOKEN"
  token_secret <- getEnv "OAUTH_TOKEN_SECRET"
  return $ Credential
    [ ("oauth_token", B.pack token)
    , ("oauth_token_secret", B.pack token_secret )
    ]

twInfo = do
  tokens' <- tokens
  credential' <- credential
  return $ setCredential tokens' credential' def

pingPongImpl ( SStatus status ) = do
  case ( T.unpack ( statusText status ) =~ ( "@minamiyama1994_ " :: String ) ) :: ( String , String , String ) of
    ( "" , _ , "ping" ) -> do
      call $ ( update $ T.pack $ "@" ++ ( T.unpack $ userScreenName $ statusUser status ) ++ " pong" ) & inReplyToStatusId ?~ statusId status
      return ( )
    ( "" , _ , "now" ) -> do
      current <- liftIO getCurrentTime
      call $ ( update $ T.pack $ "@" ++ ( T.unpack $ userScreenName $ statusUser status ) ++ " Now is " ++ show current ) & inReplyToStatusId ?~ statusId status
      return ( )
    ( "" , _ , "help" ) -> do
      call $ ( update $ T.pack $ "@" ++ ( T.unpack $ userScreenName $ statusUser status ) ++ " Please see https://github.com/minamiyama1994/chomado-bot-on-haskell/blob/minamiyama1994/README.md" ) & inReplyToStatusId ?~ statusId status
      return ( )
    ( "" , _ , 'm' : 'e' : 'm' : 'o' : ' ' : memo ) -> if ( userScreenName $ statusUser status ) == "minamiyama1994"
      then do
        current <- liftIO getCurrentTime
        liftIO $ runSqlite dbName $ insert_ $ Memo ( T.pack memo ) current
        call $ update $ T.pack $ "#南山まさかずメモ " ++ memo
        return ( )
      else return ( )
    ( "" , _ , "list memo" ) -> do
      memos <- liftIO $ runSqlite dbName $ selectList [ ] [ Desc MemoTime ]
      forM_ memos $ \ ( Entity _ ( Memo memo time ) ) -> call $ update $ T.pack $ "#南山まさかずメモ " ++ T.unpack memo ++ " at " ++ show time
    _ -> return ( )
pingPongImpl _ = return ( )

pingPong = do
  tl <- stream userstream
  tl C.$$+- CL.mapM_ ( ^! act ( pingPongImpl ) )

main = do
  runSqlite dbName $ do
    runMigration migrateAll
  info <- twInfo
  runNoLoggingT . runTW info $ do
    current <- liftIO getCurrentTime
    call $ update $ T.pack $ "南山まさかずbotが起動しました : " ++ show current
    pingPong
