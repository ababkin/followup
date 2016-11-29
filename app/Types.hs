{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
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


idKeys cid = SHM.fromList [ ("Id", stringAttr cid) ]

stringAttr s = attributeValue & avS .~ Just s

{- newtype DdbThing = DdbThing {unDdbThing :: Thing} -}

{- instance ToJSON DdbThing where -}
  {- toJSON (DdbThing Thing{name, shape, size}) = asObject -}
    {- [ -}
      {- "name"  .= asString name -}
    {- , "shape" .= asString shape -}
    {- , "size"  .= asNumber size -}
    {- ] -}

{- asNumber -}
  {- :: Show a -}
  {- => a -}
  {- -> Value -}
{- asNumber n = object ["N" .= show n] -}

{- asString -}
  {- :: ToJSON a -}
  {- => a -}
  {- -> Value -}
{- asString s = object ["S" .= s] -}

{- asObject -}
  {- :: [(Text, Value)] -}
  {- -> Value -}
{- asObject o = object ["M" .= object o] -}


{- instance FromJSON DdbThing where -}
  {- parseJSON (Object v) = do -}
    {- obj <- fromObject v -}
    {- DdbThing <$> ( -}
          {- Thing -}
            {- <$> fromStringProp "name" obj -}
            {- <*> fromStringProp "shape" obj -}
            {- <*> fromNumberProp "size" obj -}
        {- ) -}

    {- where -}
      {- fromNumber = fmap (read . T.unpack) . (.: "N") -}
      {- fromNumberProp name = fromNumber <=< (.: name) -}

      {- fromString = (.: "S") -}
      {- fromStringProp name = fromString <=< (.: name) -}

      {- fromObject = (.: "M") -}
      {- fromObjectProp name = fromObject <=< (.: name) -}

  {- parseJSON invalid = typeMismatch "DdbThing" invalid -}



