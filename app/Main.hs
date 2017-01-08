{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM, void, (<=<))
import           Data.Aeson
import           Data.Default                    (def)
import           Data.Hashable                   (hash)
import qualified Data.HashMap.Strict             as SHM
import qualified Data.Text                       as T
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan
import           Prelude                         hiding (log)

import           Qi                              (withConfig)
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..),
                                                  DdbProvCap (..))
import           Qi.Config.AWS.Lambda            (lpTimeoutSeconds)
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface
import           Qi.Program.Lambda.Interface     (GenericLambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, queryDdbRecords,
                                                  say, scanDdbRecords)
import           Qi.Util
import           Qi.Util.Cognito                 (cognitoPoolProviderLambda)
import           Qi.Util.DDB

import           Types
import           Types.Contact
import           Types.Log


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      cognito <- customResource "cognitoPoolProvider" cognitoPoolProviderLambda $ def


      contactsTable <- ddbTable "contacts" (DdbAttrDef "Id" S) def
      {- logsTable     <- ddbTable "logs"     (DdbAttrDef "ContactId" S) def -}


      genericLambda "scanContacts"  (scanContacts contactsTable) def
      {- genericLambda "postContact"   (postContact contactsTable) def -}
      {- genericLambda "getContact"    (getContact contactsTable) def -}
      {- genericLambda "putContact"    (putContact contactsTable) def -}
      {- genericLambda "deleteContact" (deleteContact contactsTable) def -}

      {- genericLambda "getContactLogs" (getContactLogs contactsTable) def -}
      {- genericLambda "putContactLog"  (putContactLog contactsTable) def -}


      return ()


scanContacts
  :: DdbTableId
  -> GenericLambdaProgram
scanContacts ddbTableId payload = do
  r <- scanDdbRecords ddbTableId
  withSuccess (r^.srsResponseStatus) $
    result
      (internalError . ("Parsing error: " ++))
      (success . (toJSON :: [Contact] -> Value))
      $ forM (r^.srsItems) parseAttrs

{- postContact -}
  {- :: DdbTableId -}
  {- -> GenericLambdaProgram -}
{- postContact ddbTableId event = -}
  {- withDeserializedBody event $ \(contact :: Contact) -> do -}
    {- r <- putDdbRecord ddbTableId . toAttrs $ addId contact -}
    {- withSuccess (r^.pirsResponseStatus) $ -}
      {- success "successfully posted contact" -}
  {- where -}
    {- addId :: Contact -> Contact -}
    {- addId c = c{cId = T.pack . show $ hash c} -}

{- getContact -}
  {- :: DdbTableId -}
  {- -> GenericLambdaProgram -}
{- getContact ddbTableId event = -}
  {- withPathParam "contactId" event $ \cid -> do -}
    {- r <- getDdbRecord ddbTableId $ idKeys cid -}
    {- withSuccess (r^.girsResponseStatus) $ -}
      {- result -}
        {- (internalError . ("Parsing error: " ++)) -}
        {- (success . (toJSON :: Contact -> Value)) -}
        {- $ parseAttrs $ r^.girsItem -}

{- putContact -}
  {- :: DdbTableId -}
  {- -> GenericLambdaProgram -}
{- putContact ddbTableId event = -}
  {- withDeserializedBody event $ \(contact :: Contact) -> do -}
    {- r <- putDdbRecord ddbTableId $ toAttrs contact -}
    {- withSuccess (r^.pirsResponseStatus) $ -}
      {- success "successfully put contact" -}

{- deleteContact -}
  {- :: DdbTableId -}
  {- -> GenericLambdaProgram -}
{- deleteContact ddbTableId event = -}
    {- withPathParam "contactId" event $ \cid -> do -}
      {- r <- deleteDdbRecord ddbTableId $ idKeys cid -}
      {- withSuccess (r^.dirsResponseStatus) $ -}
        {- success "successfully deleted contact" -}


-- Logs

{- getContactLogs -}
  {- :: DdbTableId -}
  {- -> GenericLambdaProgram -}
{- getContactLogs ddbTableId payload = -}
  {- withPathParam "contactId" event $ \cid -> do -}
    {- r <- queryDdbRecords ddbTableId keyCond (expAttrs cid) -}
    {- withSuccess (r^.qrsResponseStatus) $ -}
      {- result -}
        {- (internalError . ("Parsing error: " ++)) -}
        {- (success . (toJSON :: [Contact] -> Value)) -}
        {- $ forM (r^.qrsItems) parseAttrs -}
  {- where -}
    {- keyCond = Just $ T.concat ["ContactId = :cid"] -}
    {- expAttrs = SHM.singleton ":cid" . stringAttr -}


{- putContactLog -}
  {- :: GenericLambdaProgram -}
  {- -> ApiLambdaProgram -}
{- putContactLog ddbTableId payload = -}
  {- withDeserializedBody event $ \(contactLog :: Log) -> do -}
    {- r <- putDdbRecord ddbTableId $ toAttrs contactLog -}
    {- withSuccess (r^.pirsResponseStatus) $ -}
      {- success "successfully put log" -}




