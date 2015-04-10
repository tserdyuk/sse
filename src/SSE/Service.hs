
module SSE.Service where

import Prelude
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.EventSource (ServerEvent)


data SSErvice key = SSErvice

sservice :: (Ord key) => (Request -> key) -> IO (SSErvice key)
sservice = undefined

application :: SSErvice a -> Application
application = undefined

close :: SSErvice a -> SockAddr -> IO ()
close = undefined

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send = undefined

sendAll :: (Ord key) => SSErvice key -> ServerEvent -> IO ()
sendAll = undefined
