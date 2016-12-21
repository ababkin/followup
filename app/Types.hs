{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as SHM
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.AWS.DynamoDB (AttributeValue, attributeValue, avS)


class FromAttrs a where
  parseAttrs
    :: SHM.HashMap Text AttributeValue
    -> Result a

class ToAttrs a where
  toAttrs
    :: a
    -> SHM.HashMap Text AttributeValue

parseStringAttr
  :: Text
  -> SHM.HashMap Text AttributeValue
  -> Result Text
parseStringAttr attrName hm =
  maybe
    (Error $ "could not parse attribute: " ++ T.unpack attrName)
    return
    $ (^.avS) =<< SHM.lookup attrName hm


idKeys
  :: (Hashable k, IsString k, Eq k)
  => Text
  -> SHM.HashMap k AttributeValue
idKeys cid = SHM.fromList [ ("Id", stringAttr cid) ]

stringAttr
  :: Text
  -> AttributeValue
stringAttr s = attributeValue & avS .~ Just s

