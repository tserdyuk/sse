
module SSE.Service (SSErvice, application, onClose, send, sservice, subscribe) where

import BasePrelude
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Data.ByteString.Lazy as B (empty)
import Data.ByteString.Builder (lazyByteString)
import Network.HTTP.Types (unauthorized401)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request, lazyRequestBody, remoteHost, responseLBS)
import Network.Wai.EventSource (ServerEvent (CommentEvent), eventSourceAppChan)

import Data.GroupMap (GroupMap, lookupGroup, toGroupList)
import qualified Data.GroupMap as M (delete, empty, insert)


type Route key = Request -> IO (Maybe key)

type State key = GroupMap SockAddr key (Chan ServerEvent)

data SSErvice key = SSErvice {
	getRoute :: Route key,
	getState :: MVar (State key)
}


sservice :: (Ord key) => Route key -> IO (SSErvice key)
sservice route = do
	state <- newMVar M.empty
	forkIO $ keepAlive state
	return $ SSErvice route state

keepAlive :: MVar (State key) -> IO ()
keepAlive state = go where
	go = readMVar state >>= pingAll >> threadDelay 20000000 >> go
	pingAll = mapM_ (ping . snd . snd) . toGroupList
	ping = flip writeChan (CommentEvent $ lazyByteString $ B.empty)

application :: (Ord key) => SSErvice key -> Application
application service@(SSErvice route _) request respond = route request >>= \case
	Just key -> subscribe service key request respond
	Nothing -> respond $ responseLBS unauthorized401 [] B.empty

-- TODO: move route to application
subscribe :: (Ord key) => SSErvice key -> key -> Application
subscribe (SSErvice _ state) key request respond =
	modifyMVar state (getChannel (remoteHost request) key) >>= app
	where app chan = eventSourceAppChan chan request respond


getChannel :: (Ord key) => SockAddr -> key -> State key -> IO (State key, Chan ServerEvent)
getChannel addr key state = case lookupChannel key state of
	-- TODO: add new addr - rename function
	Just chan -> return (state, chan)
	Nothing -> newChan >>= \chan -> return (M.insert addr key chan state, chan)

onClose :: (Ord key) => SSErvice key -> SockAddr -> IO ()
onClose (SSErvice _ state) addr = modifyMVar_ state (return . M.delete addr)

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send service key se = readMVar (getState service) >>=
	maybe (return ()) (flip writeChan se) . lookupChannel key

lookupChannel :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
lookupChannel key = fmap (snd . head) . lookupGroup key
