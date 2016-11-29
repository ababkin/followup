{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Contact where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.AWS.DynamoDB (AttributeValue, attributeValue, avS)

import           Types


data Contact = Contact {
    cId        :: Text
  , cFirstName :: Text
  , cLastName  :: Text
} deriving (Generic, Show)

instance ToJSON Contact where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Contact where

instance FromAttrs Contact where
  parseAttrs hm = Contact
    <$> parseStringAttr "Id" hm
    <*> parseStringAttr "FirstName" hm
    <*> parseStringAttr "LastName" hm

instance ToAttrs Contact where
  toAttrs Contact{cId, cFirstName, cLastName} =
    SHM.fromList [
                    ("Id",        stringAttr cId)
                  , ("FirstName", stringAttr cFirstName)
                  , ("LastName",  stringAttr cLastName)
                  ]



