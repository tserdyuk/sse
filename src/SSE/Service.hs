
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


-- Add IO for future authorization
type Route key = Request -> Maybe key

type State key = GroupMap SockAddr key (Chan ServerEvent)

data SSErvice key = SSErvice {
	getRoute :: Route key,
	getState :: MVar (State key)
}


sservice :: (Ord key) => Route key -> IO (SSErvice key)
sservice route = SSErvice route <$> newMVar M.empty

application :: (Ord key) => SSErvice key -> Application
application (SSErvice route state) request respond = case route request of
	Just key -> do
		channel <- modifyMVar state (getChannel (remoteHost request) key)
		eventSourceAppChan channel request respond
	Nothing -> respond $ responseLBS unauthorized401 [] B.empty

getChannel :: (Ord key) => SockAddr -> key -> State key -> IO (State key, Chan ServerEvent)
getChannel addr key state = case lookupChannel key state of
	Just channel -> return (state, channel)
	Nothing -> do
		channel <- newChan
		let newState = M.insert addr key channel
		return (state, channel)

onClose :: (Ord key) => SSErvice key -> SockAddr -> IO ()
onClose (SSErvice _ state) addr = modifyMVar_ state (return . M.delete addr)

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send service key se = readMVar (getState service) >>=
	maybe (return ()) (flip writeChan se) . lookupChannel key

lookupChannel :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
lookupChannel key = fmap (snd . head) . lookupGroup key
