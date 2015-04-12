
module SSE.Service (SSErvice, application, onClose, send, sservice) where

import BasePrelude
import Control.Concurrent.Chan (Chan)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request)
import Network.Wai.EventSource (ServerEvent)

import Data.GroupMap (GroupMap, lookupGroup)
import qualified Data.GroupMap as M (delete, empty, insert)


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


subscribe :: (Ord key) => SockAddr -> key -> State key -> State key
subscribe addr key state = undefined

unsubscribe :: (Ord key) => SockAddr -> State key -> State key
unsubscribe = M.delete

select :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
select = undefined

subscribed :: (Ord key) => key -> State key -> Int
subscribed = undefined
