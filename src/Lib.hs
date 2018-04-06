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
import System.IO

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
        , order = createOrder Nothing Nothing Nothing
        }
  run 8080 $ app config

newtype Bot a = Bot
  { runBot :: ReaderT BotConfig Handler a
  } deriving ( Functor, Applicative, Monad, MonadIO,
               MonadReader BotConfig, MonadError ServantErr)

data Order = Order
  {
    size :: Maybe Text
  , flavour :: Maybe Text
  , price :: Maybe Int
  } deriving (Show, Generic)

data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  , order :: Order
  }

createOrder :: Maybe Text -> Maybe Text -> Maybe Int -> Order
createOrder size flavour price =
  Order {
    size = size
  , flavour = flavour
  , price = price
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
        Update { message = Just Message
          { location = Just location } } -> handleLocation location
        Update { message = Just msg } -> handleMessage msg
        Update { callback_query = Just query } -> handleCallbackQuery query
        _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update

updateOrder :: Order -> Maybe Text -> Order
updateOrder order response
  | isNothing (size order) =
      Order {
        size = response
      , flavour = Nothing
      , price = Nothing
      }
  | isNothing (flavour order) =
      Order {
        size = size order
      , flavour = response
      , price = Nothing
      }
  | otherwise = order

-- flavorReceivedMessage chatId = sendMessageRequest chatId "A Pizza de Mussarela é deliciosa."

flavourOptionsMessage :: ChatId -> SendMessageRequest
flavourOptionsMessage chatId =
  SendMessageRequest {
      message_chat_id = chatId
    , message_parse_mode = Nothing
    , message_disable_web_page_preview = Nothing
    , message_disable_notification = Nothing
    , message_reply_to_message_id = Nothing
    , message_text = "Escolha o sabor da pizza."
    , message_reply_markup = Just flavourOptionsKeyboard
}

flavourOptionsKeyboard =
  ReplyInlineKeyboardMarkup {
      reply_inline_keyboard = map flavourOptionsKeyboardButton flavourOptions
  }

flavourOptions :: [(Text)]
flavourOptions = ["Calabresa", "Mussarela", "Portuguesa"]

flavourOptionsKeyboardButton :: Text -> [InlineKeyboardButton]
flavourOptionsKeyboardButton text =
  [InlineKeyboardButton {
      ikb_text = text
    , ikb_url = Nothing
    , ikb_callback_data = Just "/flavour_option"
    , ikb_switch_inline_query = Nothing
    , ikb_callback_game = Nothing
    , ikb_switch_inline_query_current_chat = Nothing
    , ikb_pay = Nothing
  }]

handleCallbackQuery :: CallbackQuery -> Bot ()
handleCallbackQuery query = do
  BotConfig{..} <- ask
  let chatId = ChatId $ fromIntegral $ user_id $ cq_from query
      dataText = cq_data query
      sendFlavourReceivedMessage = sendMessageM (flavourOptionsMessage chatId) >> return ()
      sendLocationMessage = sendMessageM (locationMessage chatId) >> return ()

      onQuery (Just (T.stripPrefix "/size_option" -> Just _)) = sendFlavourReceivedMessage
      onQuery (Just (T.stripPrefix "/flavour_option" -> Just _)) = sendLocationMessage
  liftIO $ runClient (onQuery dataText) telegramToken manager
  -- let x  = updateOrder order (cq_data query)
  -- liftIO $ putStrLn $ show (x)
  return ()


handleMessage :: Message -> Bot ()
handleMessage msg = do
  BotConfig{..} <- ask
  let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
      messageText = text msg
      sendHelpMessage = sendMessageM (helpMessage chatId) >> return ()
      sendSizeOptionsMessage = sendMessageM (sizeOptionsMessage chatId) >> return ()
      sendLocationMessage = sendMessageM (locationMessage chatId) >> return ()

      sendFlavours flavours = mapM_ sendPhotoM $ map (buildFlavourMessage chatId) flavours

      onCommand (Just (T.stripPrefix "/pedir" -> Just _)) = sendSizeOptionsMessage
      onCommand (Just (T.stripPrefix "/ajuda" -> Just _)) = sendHelpMessage
      onCommand (Just (T.stripPrefix "/sabores" -> Just _)) = sendFlavours allFlavours
      -- onCommand (Just (T.stripPrefix "/local" -> Just _)) = sendLocationMessage
      onCommand _ = sendHelpMessage
  liftIO $ putStrLn $ show "Zero"
  liftIO $ runClient (onCommand messageText) telegramToken manager
  return ()

pizzaSizeOptions :: [(Text)]
pizzaSizeOptions = ["Pequena", "Média", "Grande", "Gigante"]

sizeOptionsKeyboardButton :: Text -> [InlineKeyboardButton]
sizeOptionsKeyboardButton text =
  [InlineKeyboardButton {
      ikb_text = text
    , ikb_url = Nothing
    , ikb_callback_data = Just "/size_option"
    , ikb_switch_inline_query = Nothing
    , ikb_callback_game = Nothing
    , ikb_switch_inline_query_current_chat = Nothing
    , ikb_pay = Nothing
  }]

-- sizeOptionsKeyboard :: ReplyKeyboardMarkup
sizeOptionsKeyboard =
  ReplyInlineKeyboardMarkup {
      reply_inline_keyboard = map sizeOptionsKeyboardButton pizzaSizeOptions
  }

sizeOptionsMessage :: ChatId -> SendMessageRequest
sizeOptionsMessage chatId =
  SendMessageRequest {
      message_chat_id = chatId
    , message_parse_mode = Nothing
    , message_disable_web_page_preview = Nothing
    , message_disable_notification = Nothing
    , message_reply_to_message_id = Nothing
    , message_text = "Escolha o tamanho da pizza."
    , message_reply_markup = Just sizeOptionsKeyboard
}

-- Button used for selecting location
locationKeyboardButton :: [KeyboardButton]
locationKeyboardButton =
  [ KeyboardButton {
      kb_text = "Compartilhar Posição"
    , kb_request_contact = Nothing
    , kb_request_location = Just True
  } ]

-- locationKeyboard :: ReplyKeyboardMarkup
locationKeyboard =
  ReplyKeyboardMarkup {
      reply_keyboard = [ locationKeyboardButton ]
    , reply_resize_keyboard = Nothing
    , reply_one_time_keyboard = Just True
    , reply_selective = Nothing
}

locationMessage :: ChatId -> SendMessageRequest
locationMessage userId =
  SendMessageRequest {
      message_chat_id = userId
    , message_parse_mode = Nothing
    , message_disable_web_page_preview = Nothing
    , message_disable_notification = Nothing
    , message_reply_to_message_id = Nothing
    , message_text = "Informe seu local."
    , message_reply_markup = Just locationKeyboard
}

-- Help message from the bot
helpMessage userId = sendMessageRequest userId $ T.unlines
  [ "/ajuda - mostra todas as opções do menu"
  , "/pedir - inicia o processo de pedido"
  , "/sabores - lista os sabores de pizza"
  , "/cancelar - um processo de pedido em andamento"
  ]

-- List of Favlours
-- [(Flavour), (Image, Ingredients, Price)]
allFlavours :: [(Text, (Text, Text, Int))]
allFlavours =
  [ ("Calabresa", ("https://d3o331zyotmfip.cloudfront.net/img/products/15116057185a1945d6e1d642.49694767.png", "Ingredients", 100))
  , ("Mussarela", ("https://d3o331zyotmfip.cloudfront.net/img/products/15116058865a19467ea285f0.80350123.png", "Ingredients", 200))
  , ("Portuguesa", ("https://d3o331zyotmfip.cloudfront.net/img/products/15116059245a1946a4dd73c4.20424311.png", "Ingredients", 300))
  ]

buildFlavourMessage chatId (flavour, (image, ingredients, price)) =
  SendPhotoRequest {
      photo_chat_id = chatId
    , photo_photo = image
    , photo_caption = Just flavour
    , photo_disable_notification = Nothing
    , photo_reply_to_message_id = Nothing
    , photo_reply_markup = Nothing
  }

locationReceivedMessage chatId = sendMessageRequest chatId "OK! Nós entregamos no seu local."

handleLocation :: Location -> Bot ()
handleLocation location = do
  liftIO $ print $ "Location received."
  -- BotConfig{..} <- ask
  -- let sendLocationReceivedMessage = sendMessageM (locationReceivedMessage chatId) >> return ()
  -- liftIO $ runClient (onCommand sendLocationReceivedMessage) telegramToken manager
  return ()
