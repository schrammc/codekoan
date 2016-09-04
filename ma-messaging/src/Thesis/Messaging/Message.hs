{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A basic, json-convertible message type with constructors and forwarding.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Thesis.Messaging.Message
       ( -- * Message Types
         Message(..)
       , MessageHeader(..)
         -- * Building Messages
       , buildMessage
         -- * Forwarding Messages
       , prepareToForwardMessage
       )where

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX

import           GHC.Generics

newtype SenderId = SenderId Text
                 deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A generic header type for all messages, that contains meta information.
data MessageHeader =
  MessageHeader { headerSendTime :: POSIXTime
                  -- ^ Time the message was initially
                  -- generated.
                , headerSender :: SenderId
                  -- ^ Information on the sender of the message
                , headerContentType :: Text
                  -- ^ Information on what content the message contains
                , forwardedBy :: [(SenderId, POSIXTime)]
                  -- ^ If the message was forwarded, 
                }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

-- | Add forwarding information to a message's header.
prepareToForwardMessage :: SenderId -> Message a -> IO (Message a)
prepareToForwardMessage sender (Message header content) = do
  time <- getPOSIXTime
  return $ Message { messageHeader =
                       header{forwardedBy= (sender, time):(forwardedBy header)}
                   ,  messageContent = content
                   }

-- | Build a message with current timestamp
buildMessage :: (Show a, Eq a, ToJSON a, FromJSON a) =>
                SenderId
                -- ^ Id of the sender
             -> Text
             -- ^ Content type description
             -> a
             -- ^ The actual content
             -> IO (Message a)
buildMessage sender contentInfo content = do
  time <- getPOSIXTime
  let header = MessageHeader { headerSendTime = time
                             , headerSender = sender
                             , headerContentType = contentInfo
                             , forwardedBy = []
                             }
  return $ Message header content

data Message a where
  Message :: (Show a, Eq a, ToJSON a, FromJSON a) =>
             { messageHeader :: MessageHeader
             , messageContent :: a
             } -> Message a

deriving instance Eq (Message a)
deriving instance Show (Message a)

instance (Show a, FromJSON a, ToJSON a, Eq a) => FromJSON (Message a) where
  parseJSON (Object o) = Message <$>
                           o .: "messageHeader" <*>
                           o .: "messageContent"
  parseJSON _ = mzero

instance (Show a, FromJSON a, ToJSON a, Eq a) => ToJSON (Message a) where
      -- this generates a Value
  toJSON Message{..} =
    object [ "messageHeader" .= messageHeader
           , "messageContent" .= messageContent
           ]
