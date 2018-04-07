{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Storage where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Order = Order
  { size :: !Text
  , flavour :: !Text
  , price :: Int
  } deriving (Show, Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Order
instance ToJSON Order

orderFile :: Int -> FilePath
orderFile orderId = "orders/" ++ show orderId ++ ".json"

createOrder orderId = do
  let order = Order { size = "null"
                    , flavour = "null"
                    , price = 0
                    }
  storeOrder orderId order
  return ()

storeOrder orderId order = B.writeFile (orderFile orderId) (encode order)

readOrder :: Int -> IO B.ByteString
readOrder orderId = B.readFile $ orderFile orderId

getOrder :: Int -> IO (Maybe Order)
getOrder orderId = decode <$> readOrder orderId

-- updateOrder :: Order -> Maybe Text -> Order
-- updateOrder order response
--   | isNothing (size order) =
--       Order {
--         size = response
--       , flavour = Nothing
--       , price = Nothing
--       }
--   | isNothing (flavour order) =
--       Order {
--         size = size order
--       , flavour = response
--       , price = Nothing
--       }
--   | otherwise = order
