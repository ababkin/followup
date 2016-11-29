{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                hiding (view, (.=))
import           Control.Monad               (forM, void, (<=<))
import           Data.Aeson
import qualified Data.HashMap.Strict         as SHM
import qualified Data.Text                   as T
import           Prelude                     hiding (log)

import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Delete, Get, Post),
                                              RequestBody (..), aeBody,
                                              aeParams, rpPath)
import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Config.Identifier        (DdbTableId)
import           Qi.Program.Config.Interface
import           Qi.Program.Lambda.Interface (LambdaProgram)
import           Qi.Util.Api

import           Types
import           Types.Contact
import           Types.Log
import           Util                        (deleteDdbRecord, getDdbRecord,
                                              putDdbRecord, queryDdbRecords,
                                              scanDdbRecords)


-- Use the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://gliqqtz1pi.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things/cup"
curl -v -X GET "$API/things/cup"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"chair\", \"shape\": \"square\", \"size\": 10}" "$API/things/chair"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/mycup"
curl -v -X GET "$API/things"
-}


main :: IO ()
main =
  "followup" `withConfig` config

    where
      config :: ConfigProgram ()
      config = do
        contactsTable <- ddbTable "contacts" (DdbAttrDef "Id" S) Nothing (DdbProvCap 1 1)
        logsTable     <- ddbTable "logs"     (DdbAttrDef "ContactId" S) Nothing (DdbProvCap 1 1)

        api "followup" >>= \followup ->
          apiResource "contacts" followup >>= \contacts -> do
            apiMethodLambda "scanContacts" Get
              contacts $ scanContacts contactsTable

            apiResource "{contactId}" contacts >>= \contact -> do
              apiMethodLambda "getContact" Get
                contact $ getContact contactsTable
              apiMethodLambda "putContact" Post
                contact $ putContact contactsTable
              apiMethodLambda "deleteContact" Delete
                contact $ deleteContact contactsTable


              apiResource "logs" contact >>= \logs -> do
                apiMethodLambda "getContactLogs" Get
                  logs $ getContactLogs logsTable

                apiMethodLambda "putContactLog" Post
                  logs $ putContactLog logsTable



        return ()

scanContacts
  :: DdbTableId
  -> ApiEvent
  -> LambdaProgram ()
scanContacts ddbTableId _ =
  scanDdbRecords ddbTableId $ \(contacts :: [Contact]) ->
    success $ toJSON contacts

getContact
  :: DdbTableId
  -> ApiEvent
  -> LambdaProgram ()
getContact ddbTableId event =
  withContactId event $ \cid ->
    getDdbRecord ddbTableId (idKeys cid) $ \(contact :: Contact) ->
      success $ toJSON contact


putContact
  :: DdbTableId
  -> ApiEvent
  -> LambdaProgram ()
putContact ddbTableId event =
  withContact event $ \contact ->
    putDdbRecord ddbTableId contact $
      successString "successfully put contact"

deleteContact
  :: DdbTableId
  -> ApiEvent
  -> LambdaProgram ()
deleteContact ddbTableId event =
  withContactId event $ \cid ->
    deleteDdbRecord ddbTableId (idKeys cid) $
      successString "successfully deleted contact"


-- Logs

getContactLogs
  :: DdbTableId
  -> ApiEvent
  -> LambdaProgram ()
getContactLogs ddbTableId event =
  withContactId event $ \cid ->
    queryDdbRecords ddbTableId (keyCond cid) $ \(logs :: [Log]) ->
      success $ toJSON logs
  where
    keyCond cid = Just $ T.concat ["ContactId = '", cid, "'"]


putContactLog
  :: DdbTableId
  -> ApiEvent
  -> LambdaProgram ()
putContactLog ddbTableId event =
  withLog event $ \log ->
    putDdbRecord ddbTableId log $
      successString "successfully put log"

-- Util

withContact
  :: ApiEvent
  -> (Contact -> LambdaProgram ())
  -> LambdaProgram ()
withContact event f = case event^.aeBody of
  JsonBody jb -> case fromJSON jb of
    Success contact -> f contact
    Error err       -> internalError $ "Error: fromJson: " ++ err ++ ". Json was: " ++ show jb
  unexpected  ->
    argumentsError $ "Unexpected request body: " ++ show unexpected

withLog
  :: ApiEvent
  -> (Log -> LambdaProgram ())
  -> LambdaProgram ()
withLog event f = case event^.aeBody of
  JsonBody jb -> case fromJSON jb of
    Success log -> f log
    Error err   -> internalError $ "Error: fromJson: " ++ err ++ ". Json was: " ++ show jb
  unexpected  ->
    argumentsError $ "Unexpected request body: " ++ show unexpected


withContactId event f = case SHM.lookup "contactId" $ event^.aeParams.rpPath of
  Just (String x) -> f x
  Just unexpected ->
    argumentsError $ "unexpected path parameter: " ++ show unexpected
  Nothing ->
    argumentsError "expected path parameter 'thingId' was not found"



