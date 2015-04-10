
module SSE.Service where

import Prelude
import Control.Applicative ((<$>))
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar, newMVar)
import Data.Map.Strict (Map, empty)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.EventSource (ServerEvent)


data SSErvice key = SSErvice {
	route :: Request -> key,
	state :: MVar (State key)
}

data State key = State {
	channels :: Map key (Chan ServerEvent, Int),
	keys :: Map SockAddr key
}


sservice :: (Ord key) => (Request -> key) -> IO (SSErvice key)
sservice route = SSErvice route <$> newMVar (State empty empty)

application :: SSErvice a -> Application
application = undefined

close :: SSErvice a -> SockAddr -> IO ()
close = undefined

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send = undefined

sendAll :: (Ord key) => SSErvice key -> ServerEvent -> IO ()
sendAll = undefined
