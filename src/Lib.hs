{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics hiding (from)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Data.Maybe
import Data.Monoid
import Web.Telegram.API.Bot
import System.Environment

-- Create the API endpoints
-- "webhook" endpoint will receive updates from the telegram's servers
type BotAPI =
     "webhook"
  :> Capture "secret" Text
  :> ReqBody '[JSON] Update
  :> Post '[JSON] ()

botApi :: Proxy BotAPI
botApi = Proxy

-- Starts up the APP by reading the env from the System
-- and defines the initial configuration
startApp :: IO ()
startApp = do
  putStrLn "Starting Pizza Chatbot Server"
  env <- getEnvironment
  manager' <- newManager tlsManagerSettings
  let telegramToken' = fromJust $ lookup "TELEGRAM_PIZZA_CHATBOT_TOKEN" env
      config = BotConfig
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , manager = manager'
        }
  run 8080 $ app config

newtype Bot a = Bot
  { runBot :: ReaderT BotConfig Handler a
  } deriving ( Functor, Applicative, Monad, MonadIO,
               MonadReader BotConfig, MonadError ServantErr)

data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  }

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
          transform config = Nat (flip runReaderT config . runBot)

-- Proccess the received requests
botServer :: ServerT BotAPI Bot
botServer = handleWebhook
  where
    handleWebhook :: Text -> Update -> Bot ()
    handleWebhook secret update = do
      Token token <- asks telegramToken
      if EQ == compare secret token
        then handleUpdate update
        else throwError err403

handleUpdate :: Update -> Bot ()
handleUpdate update = do
    case update of
        Update { message = Just msg } -> handleMessage msg
        _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update

handleMessage :: Message -> Bot ()
handleMessage msg = do
  BotConfig{..} <- ask
  let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
      messageText = text msg
      sendHelpMessage = sendMessageM (helpMessage chatId) >> return ()
      onCommand (Just (T.stripPrefix "/ajuda" -> Just _)) = sendHelpMessage
      onCommand _ = sendHelpMessage
  liftIO $ runClient (onCommand messageText) telegramToken manager
  return ()

-- Help message from the bot
helpMessage userId = sendMessageRequest userId $ T.unlines
  [ "/ajuda - mostra todas as opções do menu"
  , "/pedir - inicia o processo de pedido"
  , "/sabores - lista os sabores de pizza"
  , "/cancelar - um processo de pedido em andamento"
  ]
