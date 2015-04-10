
module SSE.Service where

import BasePrelude
import Control.Concurrent.Chan (Chan)
import Data.Map.Strict as M (Map, empty)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.EventSource (ServerEvent)


data SSErvice key = SSErvice {
	getRoute :: Request -> key,
	getState :: MVar (State key)
}

data State key = State {
	getChannels :: Map key (Chan ServerEvent, Int),
	getKeys :: Map SockAddr key
}


sservice :: (Ord key) => (Request -> key) -> IO (SSErvice key)
sservice route = SSErvice route <$> newMVar (State M.empty M.empty)

application :: SSErvice a -> Application
application = undefined

close :: SSErvice a -> SockAddr -> IO ()
close = undefined

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send = undefined

sendAll :: (Ord key) => SSErvice key -> ServerEvent -> IO ()
sendAll = undefined
