module SSE (module SSE.Service, bsEvent, cors, emptyResponse) where

import SSE.Service

import BasePrelude
import Data.ByteString.Builder (lazyByteString)
import Data.ByteString.Lazy as B (ByteString, empty)
import Network.HTTP.Types (Status)
import Network.Wai (Middleware, Response, responseLBS)
import Network.Wai.EventSource (ServerEvent (ServerEvent))
import Network.Wai.Middleware.AddHeaders (addHeaders)


cors :: Middleware
cors = addHeaders [("Access-Control-Allow-Origin", "*")]

bsEvent :: ByteString -> ServerEvent
bsEvent bs = ServerEvent Nothing Nothing [lazyByteString bs]

emptyResponse :: Status -> Response
emptyResponse status = responseLBS status [] B.empty
