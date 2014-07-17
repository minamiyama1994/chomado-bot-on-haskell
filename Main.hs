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
import Text.Regex
import Text.Regex.Posix
import qualified Data.Text as T
import System.Environment
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Stream
import Web.Twitter.Types
import Web.Authenticate.OAuth
import Control.Monad.Logger
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [ mkPersist sqlSettings , mkMigrate "migrateAll" ] [persistLowerCase|
Memo
    memo T.Text
    time DateTime default=CURRENT_TIME
|]

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
  case ( T.unpack ( Web.Twitter.Types.statusText status ) =~ ( "@minamiyama1994_ " :: String ) ) :: ( String , String , String ) of
    ( "" , _ , "ping" ) -> do
      call $ ( Web.Twitter.Conduit.update $ T.pack $ "@" ++ ( T.unpack $ Web.Twitter.Types.userScreenName $ Web.Twitter.Types.statusUser status ) ++ " pong" ) & inReplyToStatusId ?~ Web.Twitter.Types.statusId status
      return ( )
    _ -> return ( )
pingPongImpl _ = return ( )

pingPong = do
  tl <- stream userstream
  tl C.$$+- CL.mapM_ ( ^! act ( pingPongImpl ) )

main = do
  info <- twInfo
  runNoLoggingT . runTW info $ pingPong
