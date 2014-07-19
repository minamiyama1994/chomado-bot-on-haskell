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
import Control.Concurrent ( forkIO , threadDelay )
import Database.Persist ( insert_ , selectList , SelectOpt ( Desc ) , Entity ( Entity ) )
import Database.Persist.Sqlite ( runSqlite , runMigration )
import Database.Persist.TH
import Text.Parser.Char
import Text.Parser.Combinators
import Text.ParserCombinators.ReadP ( readP_to_S )
import DateTimePattern

share [ mkPersist sqlSettings , mkMigrate "migrateAll" ] [persistLowerCase|
Memo
    memo T.Text
    time DateTime default=CURRENT_TIME
Timer
    time DateTimePattern
    text T.Text
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

parseTimeImplSuccess = some digit >>= return . Just . read

parseTimeImplFailure = char '*' >> return Nothing

parseTimeImpl = choice [ parseTimeImplSuccess , parseTimeImplFailure ]

parseTime = do
  year <- parseTimeImpl
  some space
  month <- parseTimeImpl
  some space
  day <- parseTimeImpl
  some space
  hour <- parseTimeImpl
  some space
  minute <- parseTimeImpl
  some space
  second <- parseTimeImpl
  return $ DateTimePattern ( year , month >>= return . fromInteger , day >>= return . fromInteger , hour >>= return . fromInteger , minute >>= return . fromInteger , second >>= return . fromInteger )

parseTimer = do
  time <- parseTime
  some space
  text <- many anyChar
  eof
  return $ Timer time $ T.pack text

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
    ( "" , _ , 't' : 'i' : 'm' : 'e' : 'r' : ' ' : body ) -> case readP_to_S parseTimer body of
      [ ( timer' , "" ) ] -> liftIO $ runSqlite dbName $ insert_ timer'
      _ -> return ( )
    _ -> return ( )
pingPongImpl _ = return ( )

pingPong = do
  tl <- stream userstream
  tl C.$$+- CL.mapM_ ( ^! act ( pingPongImpl ) )

matchTimeImpl time timeP = maybe True ( == time ) timeP

matchTime dateTime pattern = matchTimeImpl year yearP && matchTimeImpl month monthP && matchTimeImpl day dayP && matchTimeImpl hour hourP && matchTimeImpl minute minuteP && matchTimeImpl second secondP where
  ( year , month , day , hour , minute , second ) = toGregorian dateTime
  ( yearP , monthP , dayP , hourP , minuteP , secondP ) = dateTimePattern pattern

timer = do
  current <- liftIO getCurrentTime
  times <- liftIO $ runSqlite dbName $ selectList [ ] [ ]
  forM_ times $ \ ( Entity _ time ) -> if matchTime ( addMinutes ( 60 * 9 ) current ) $ timerTime time
    then do
      call $ update $ T.pack $ ( "@minamiyama1994 " ++ ) $ T.unpack $ timerText time
      return ( )
    else return ( )
  liftIO $ threadDelay $ 1000 * 500
  timer

main = do
  runSqlite dbName $ do
    runMigration migrateAll
  info <- twInfo
  forkIO $ runNoLoggingT . runTW info $ timer
  runNoLoggingT . runTW info $ do
    current <- liftIO getCurrentTime
    call $ update $ T.pack $ "南山まさかずbotが起動しました : " ++ show current
    pingPong
