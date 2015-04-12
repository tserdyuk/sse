
module SSE.Service (SSErvice, application, onClose, send, sservice) where

import BasePrelude
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Data.ByteString.Lazy as B (empty)
import Network.HTTP.Types (unauthorized401)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request, lazyRequestBody, remoteHost, responseLBS)
import Network.Wai.EventSource (ServerEvent, eventSourceAppChan)

import Data.GroupMap (GroupMap, lookupGroup)
import qualified Data.GroupMap as M (delete, empty, insert)


type Route key = Request -> IO (Maybe key)

type State key = GroupMap SockAddr key (Chan ServerEvent)

data SSErvice key = SSErvice {
	getRoute :: Route key,
	getState :: MVar (State key)
}


sservice :: (Ord key) => Route key -> IO (SSErvice key)
sservice route = SSErvice route <$> newMVar M.empty

application :: (Ord key) => SSErvice key -> Application
application (SSErvice route state) request respond = route request >>= \case
	Just key -> do
		chan <- modifyMVar state (getChannel (remoteHost request) key)
		eventSourceAppChan chan request respond
	Nothing -> respond $ responseLBS unauthorized401 [] B.empty

getChannel :: (Ord key) => SockAddr -> key -> State key -> IO (State key, Chan ServerEvent)
getChannel addr key state = case lookupChannel key state of
	Just chan -> return (state, chan)
	Nothing -> newChan >>= \chan -> return (M.insert addr key chan state, chan)

onClose :: (Ord key) => SSErvice key -> SockAddr -> IO ()
onClose (SSErvice _ state) addr = modifyMVar_ state (return . M.delete addr)

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send service key se = readMVar (getState service) >>=
	maybe (return ()) (flip writeChan se) . lookupChannel key

lookupChannel :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
lookupChannel key = fmap (snd . head) . lookupGroup key
