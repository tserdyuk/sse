
module SSE.Service (SSErvice, application, onClose, send, sservice) where

import BasePrelude
import Control.Concurrent.Chan (Chan, writeChan)
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

onClose :: (Ord key) => SSErvice key -> SockAddr -> IO ()
onClose (SSErvice _ state) addr = modifyMVar_ state (return . M.delete addr)

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send (SSErvice _ state) key se = readMVar state >>=
	flip writeChan se . snd . head . fromJust . lookupGroup key


subscribe :: (Ord key) => SockAddr -> key -> State key -> State key
subscribe addr key state = undefined

select :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
select = undefined

subscribed :: (Ord key) => key -> State key -> Int
subscribed = undefined
