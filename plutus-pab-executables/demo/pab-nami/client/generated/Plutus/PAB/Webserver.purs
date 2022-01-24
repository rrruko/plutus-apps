-- File auto generated by servant-purescript! --
module Plutus.PAB.Webserver where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Except (ExceptT)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.RawJson (RawJson)
import Data.Tuple (Tuple(..))
import DemoContract (DemoContract)
import Plutus.PAB.Webserver.Types (ContractActivationArgs, ContractInstanceClientState, ContractSignatureResponse, FullReport)
import Servant.PureScript (class MonadAjax, flagQueryPairs, paramListQueryPairs, paramQueryPairs, request, toHeader, toPathSegment)
import URI (PathAbsolute(..), RelativePart(..), RelativeRef(..))
import URI.Path.Segment (segmentNZFromString)
import Wallet.Types (ContractInstanceId)
import Affjax.RequestBody (json) as Request
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.String.NonEmpty as NES

getApiHealthcheck
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ExceptT e m Unit
getApiHealthcheck =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.unit
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "healthcheck"
    ]
  query = Nothing

getApiFullreport
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ExceptT e m (FullReport DemoContract)
getApiFullreport =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "fullreport"
    ]
  query = Nothing

postApiContractActivate
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ContractActivationArgs DemoContract
  -> ExceptT e m ContractInstanceId
postApiContractActivate reqBody =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left POST
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Just reqBody
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.value
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "activate"
    ]
  query = Nothing

getApiContractInstanceByContractinstanceidStatus
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ContractInstanceId
  -> ExceptT e m (ContractInstanceClientState DemoContract)
getApiContractInstanceByContractinstanceidStatus contract_instance_id =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "instance"
    , toPathSegment contract_instance_id
    , "status"
    ]
  query = Nothing

getApiContractInstanceByContractinstanceidSchema
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ContractInstanceId
  -> ExceptT e m (ContractSignatureResponse DemoContract)
getApiContractInstanceByContractinstanceidSchema contract_instance_id =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "instance"
    , toPathSegment contract_instance_id
    , "schema"
    ]
  query = Nothing

postApiContractInstanceByContractinstanceidEndpointByEndpointname
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => RawJson
  -> ContractInstanceId
  -> String
  -> ExceptT e m Unit
postApiContractInstanceByContractinstanceidEndpointByEndpointname
  reqBody
  contract_instance_id
  endpoint_name =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left POST
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Just reqBody
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.value
  decoder = D.unit
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "instance"
    , toPathSegment contract_instance_id
    , "endpoint"
    , toPathSegment endpoint_name
    ]
  query = Nothing

putApiContractInstanceByContractinstanceidStop
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ContractInstanceId
  -> ExceptT e m Unit
putApiContractInstanceByContractinstanceidStop contract_instance_id =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left PUT
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.unit
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "instance"
    , toPathSegment contract_instance_id
    , "stop"
    ]
  query = Nothing

getApiContractInstancesWalletByWalletid
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => String
  -> Maybe String
  -> ExceptT e m (Array (ContractInstanceClientState DemoContract))
getApiContractInstancesWalletByWalletid wallet_id status =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "instances"
    , "wallet"
    , toPathSegment wallet_id
    ]
  query = Just $ fold
    [ paramQueryPairs "status" status
    ]

getApiContractInstances
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => Maybe String
  -> ExceptT e m (Array (ContractInstanceClientState DemoContract))
getApiContractInstances status =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "instances"
    ]
  query = Just $ fold
    [ paramQueryPairs "status" status
    ]

getApiContractDefinitions
  :: forall e m
   . MonadAjax JsonDecodeError Json e m
  => ExceptT e m (Array (ContractSignatureResponse DemoContract))
getApiContractDefinitions =
  request req
  where
  req = { method, uri, headers, content, encode, decode }
  method = Left GET
  uri = RelativeRef relativePart query Nothing
  headers = catMaybes
    [
    ]
  content = Nothing
  encode = E.encode encoder
  decode = D.decode decoder
  encoder = E.null
  decoder = D.value
  relativePart = RelativePartNoAuth $ Just
    [ "api"
    , "contract"
    , "definitions"
    ]
  query = Nothing
