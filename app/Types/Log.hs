{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Log where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.Aeson.Types     (fieldLabelModifier, typeMismatch)
import           Data.Char            (toLower)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.AWS.DynamoDB (AttributeValue, attributeValue, avS)

import           Types


data Log = Log {
    lContactId :: Text
  , lTimestamp :: Text
  , lBody      :: Text
} deriving (Generic, Show)

instance ToJSON Log where
  toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = drop 1 . fmap toLower }

instance FromJSON Log where

instance FromAttrs Log where
  parseAttrs hm = Log
    <$> parseStringAttr "ContactId" hm
    <*> parseStringAttr "Timestamp" hm
    <*> parseStringAttr "Body" hm

instance ToAttrs Log where
  toAttrs Log{lContactId, lTimestamp, lBody} =
    SHM.fromList [
                    ("ContactId", stringAttr lContactId)
                  , ("Timestamp", stringAttr lTimestamp)
                  , ("Body",      stringAttr lBody)
                  ]


