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

tokens :: IO OAuth -- TwitterにアクセスするためのConsumerKeyとConsumerSecretを環境変数から取得します
tokens = do
  ck <- getEnv "OAUTH_CONSUMER_KEY"
  cs <- getEnv "OAUTH_CONSUMER_SECRET"
  return $ twitterOAuth
    { oauthConsumerKey = B.pack ck
    , oauthConsumerSecret = B.pack cs
    }

credential :: IO Credential -- TwitterにアクセスするためのAccessTokenとAccessTokenSecretを環境変数から取得します
credential = do
  token <- getEnv "OAUTH_TOKEN"
  token_secret <- getEnv "OAUTH_TOKEN_SECRET"
  return $ Credential
    [ ("oauth_token", B.pack token)
    , ("oauth_token_secret", B.pack token_secret )
    ]

twInfo :: IO TWInfo -- Twitterにアクセスするための情報を集めたオブジェクトの生成です
twInfo = do
  tokens' <- tokens
  credential' <- credential
  return $ setCredential tokens' credential' def

pingPongImpl :: StreamingAPI -> IO ( ) -- UserStreamをで流れてきたイベントのうち、"ping"というリプライがきていた場合はpongと返信します、その他の場合は無視します
pingPongImpl ( SStatus status ) = do -- 普通のツイートの場合
  info <- twInfo
  runNoLoggingT . runTW info $ do
    case ( T.unpack ( Web.Twitter.Types.statusText status ) =~ ( "@chomado_bot " :: String ) ) :: ( String , String , String ) of -- ちょまどbotへのリプライかどうかを正規表現で調べます
      ( "" , _ , "ping" ) -> do -- リプライでかつpingだった場合
        call $ ( update $ T.pack $ "@" ++ ( T.unpack $ Web.Twitter.Types.userScreenName $ Web.Twitter.Types.statusUser status ) ++ " pong" ) & inReplyToStatusId ?~ Web.Twitter.Types.statusId status -- リプライ元の人にpongと返信します、Web.Twitter.Typesとかついてるのは識別子の衝突を避けるためです
        return ( )
      _ -> return ( ) -- リプライじゃなかったりpingじゃなかったりしたらガンスルーします
pingPongImpl _ = return ( ) -- 普通のツイートでなければガンスルーします

pingPong :: IO ( ) -- UserStreamでTLを監視します
pingPong = do
  info <- twInfo
  runNoLoggingT . runTW info $ do
    tl <- stream userstream -- UserStreamでTLを監視します
    tl C.$$+- CL.mapM_ ( ^! act ( liftIO . pingPongImpl ) ) -- 一つ一つのイベントごとにpingpongimplを呼び出します

main :: IO ( )
main = do
  forkIO pingPong -- UserStreamを監視するスレッドを生成します
  info <- twInfo
  runNoLoggingT . runTW info $ do -- twitter-conduitをたたくためのおまじない
    forM_ [ 0 , 0 .. ] $ \ _ -> do -- とりあえず無限ループのために無限リストを生成しています、中身に特に意味はないです、よりよい書き方があるかも
      liftIO $ threadDelay $ 1000 * 1000 * 1 -- 1秒間待機
      current <- liftIO getCurrentTime >>= ( return . addMinutes' ( 60 * 9 ) ) -- 現在時刻を取得、getCurrentTimeはUTCを返してくるので日本時刻に直すために9時間足しています
      case toGregorian current of
        ( _ , _ , _ , hour , 0 , 0 ) -> do -- ○時きっかりであれば時刻をツイートします
          call $ update $ T.pack $ "It is " ++ show hour ++ " o'clock now."
          return ( )
        _ -> return ( ) -- そうでなければ何もしません
