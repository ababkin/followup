{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM)
import           Data.Aeson
import qualified Data.HashMap.Strict             as SHM
import           Data.Text                       (Text)
import           Network.AWS.DynamoDB            (AttributeValue,
                                                  attributeValue, avS)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan

import           Qi                              (withConfig)
import           Qi.Config.AWS.Api               (ApiEvent (..),
                                                  ApiVerb (Delete, Get, Post),
                                                  RequestBody (..), aeBody,
                                                  aeParams, rpPath)
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..), DdbAttrs,
                                                  DdbProvCap (..))
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Lambda.Interface     (LambdaProgram)
import qualified Qi.Program.Lambda.Interface     as LI
import           Qi.Util

import           Types


scanDdbRecords
  :: FromAttrs a
  => DdbTableId
  -> ([a] -> LambdaProgram ())
  -> LambdaProgram ()
scanDdbRecords ddbTableId f = do
  res <- LI.scanDdbRecords ddbTableId
  withSuccess (res^.srsResponseStatus) $
    case forM (res^.srsItems) parseAttrs of
      Success records ->
        f records
      Error err ->
        internalError $ "Parsing error: " ++ show err

queryDdbRecords
  :: FromAttrs a
  => DdbTableId
  -> Maybe Text
  -> DdbAttrs
  -> ([a] -> LambdaProgram ())
  -> LambdaProgram ()
queryDdbRecords ddbTableId keyCond expAttrs f = do
  res <- LI.queryDdbRecords ddbTableId keyCond expAttrs
  withSuccess (res^.qrsResponseStatus) $
    case forM (res^.qrsItems) parseAttrs of
      Success records ->
        f records
      Error err ->
        internalError $ "Parsing error: " ++ show err

getDdbRecord
  :: FromAttrs a
  => DdbTableId
  -> SHM.HashMap Text AttributeValue
  -> (a -> LambdaProgram ())
  -> LambdaProgram ()
getDdbRecord ddbTableId keys f = do
  res <- LI.getDdbRecord ddbTableId keys
  withSuccess (res^.girsResponseStatus) $
    case parseAttrs $ res^.girsItem of
      Success record ->
        f record
      Error err ->
        internalError $ "Parsing error: " ++ show err


putDdbRecord
  :: ToAttrs a
  => DdbTableId
  -> a
  -> LambdaProgram ()
  -> LambdaProgram ()
putDdbRecord ddbTableId item cont = do
  res <- LI.putDdbRecord ddbTableId $ toAttrs item
  withSuccess (res^.pirsResponseStatus) $ cont


deleteDdbRecord
  :: DdbTableId
  -> SHM.HashMap Text AttributeValue
  -> LambdaProgram ()
  -> LambdaProgram ()
deleteDdbRecord ddbTableId keys cont = do
  res <- LI.deleteDdbRecord ddbTableId keys
  withSuccess (res^.dirsResponseStatus) $ cont




withSuccess
  :: Int
  -> LambdaProgram ()
  -> LambdaProgram ()
withSuccess code f =
  case code of
    200         -> f
    unexpected  ->
      internalError $ "Error: unexpected response status: " ++ show unexpected



