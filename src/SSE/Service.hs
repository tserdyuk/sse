
module SSE.Service where

import BasePrelude
import Control.Concurrent.Chan (Chan)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.EventSource (ServerEvent)

import Data.GroupMap as M


type Route key = Request -> Maybe key

type State key = GroupMap SockAddr key (Chan ServerEvent)

data SSErvice key = SSErvice {
	getRoute :: Route key,
	getState :: MVar (State key)
}


sservice :: (Ord key) => Route key -> IO (SSErvice key)
sservice route = SSErvice route <$> newMVar M.empty

application :: SSErvice a -> Application
application = undefined

onClose :: SSErvice a -> SockAddr -> IO ()
onClose = undefined

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send = undefined

sendAll :: (Ord key) => SSErvice key -> ServerEvent -> IO ()
sendAll = undefined


subscribe :: (Ord key) => SockAddr -> key -> State key -> State key
subscribe = undefined

unsubscribe :: (Ord key) => SockAddr -> State key -> State key
unsubscribe = undefined

select :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
select = undefined

subscribed :: (Ord key) => key -> State key -> Int
subscribed = undefined
