{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.IO.Class
import Control.Concurrent

tokens :: IO OAuth
tokens = do
  ck <- getEnv "OAUTH_CONSUMER_KEY"
  cs <- getEnv "OAUTH_CONSUMER_SECRET"
  return $ twitterOAuth
    { oauthConsumerKey = B.pack ck
    , oauthConsumerSecret = B.pack cs
    }

credential :: IO Credential
credential = do
  token <- getEnv "OAUTH_TOKEN"
  token_secret <- getEnv "OAUTH_TOKEN_SECRET"
  return $ Credential
    [ ("oauth_token", B.pack token)
    , ("oauth_token_secret", B.pack token_secret )
    ]

twInfo :: IO TWInfo
twInfo = do
  tokens' <- tokens
  credential' <- credential
  return $ setCredential tokens' credential' def

pingPongImpl :: StreamingAPI -> IO ( )
pingPongImpl ( SStatus status ) = do
  info <- twInfo
  runNoLoggingT . runTW info $ do
    case ( T.unpack ( Web.Twitter.Types.statusText status ) =~ ( "@chomado_bot " :: String ) ) :: ( String , String , String ) of
      ( "" , _ , "ping" ) -> do
        call $ ( update $ T.pack $ "@" ++ ( T.unpack $ Web.Twitter.Types.userScreenName $ Web.Twitter.Types.statusUser status ) ++ " pong" ) & inReplyToStatusId ?~ Web.Twitter.Types.statusId status
        return ( )
      _ -> return ( )
pingPongImpl _ = return ( )

pingPong :: IO ( )
pingPong = do
  info <- twInfo
  runNoLoggingT . runTW info $ do
    tl <- stream userstream
    tl C.$$+- CL.mapM_ ( ^! act ( liftIO . pingPongImpl ) )

main :: IO ( )
main = do
  forkIO pingPong
  info <- twInfo
  runNoLoggingT . runTW info $ do
    forM_ [ 0 , 0 .. ] $ \ _ -> do
      liftIO $ threadDelay $ 1000 * 1000 * 1
      current <- liftIO getCurrentTime >>= ( return . addMinutes' ( 60 * 9 ) )
      case toGregorian current of
        ( _ , _ , _ , hour , 0 , 0 ) -> do
          call $ update $ T.pack $ "It is " ++ show hour ++ " o'clock now."
          return ( )
        _ -> return ( )
      
