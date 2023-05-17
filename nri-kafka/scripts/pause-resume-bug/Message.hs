module Message where

import Data.Aeson

newtype Message = Message
  { id :: Int
  }
  deriving (Generic)

instance FromJSON Message

instance ToJSON Message
