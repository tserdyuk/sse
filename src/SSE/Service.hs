
module SSE.Service (SSErvice, application, onClose, send, sservice, subscribe) where

import BasePrelude
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Data.ByteString.Lazy as B (empty)
import Data.ByteString.Builder (lazyByteString)
import Network.HTTP.Types (unauthorized401)
import Network.Socket (SockAddr)
import Network.Wai (Application, Request, lazyRequestBody, remoteHost)
import Network.Wai.EventSource (ServerEvent (CommentEvent), eventSourceAppChan)

import Data.GroupMap (GroupMap, lookupGroup, toGroupList)
import qualified Data.GroupMap as M (delete, empty, insert)
import SSE.Utils (emptyResponse)


type Route key = Request -> IO (Maybe key)

type State key = GroupMap SockAddr key (Chan ServerEvent)

newtype SSErvice key = SSErvice (MVar (State key))


sservice :: (Ord key) => IO (SSErvice key)
sservice = do
	state <- newMVar M.empty
	forkIO $ keepAlive state
	return $ SSErvice state

keepAlive :: MVar (State key) -> IO ()
keepAlive state = go where
	go = readMVar state >>= pingAll >> threadDelay 20000000 >> go
	pingAll = mapM_ (ping . snd . snd) . toGroupList
	ping = flip writeChan (CommentEvent $ lazyByteString $ B.empty)

application :: (Ord key) => Route key -> SSErvice key -> Application
application route service request respond = route request >>= \case
	Just key -> subscribe service key request respond
	Nothing -> respond $ emptyResponse unauthorized401

subscribe :: (Ord key) => SSErvice key -> key -> Application
subscribe (SSErvice state) key request respond =
	modifyMVar state (addClient (remoteHost request) key) >>=
	\chan -> eventSourceAppChan chan request respond


addClient :: (Ord key) => SockAddr -> key -> State key -> IO (State key, Chan ServerEvent)
addClient addr key state =
	maybe newChan return (lookupChannel key state) >>=
	\chan -> return (M.insert addr key chan state, chan)

onClose :: (Ord key) => SSErvice key -> SockAddr -> IO ()
onClose (SSErvice state) addr = modifyMVar_ state (return . M.delete addr)

send :: (Ord key) => SSErvice key -> key -> ServerEvent -> IO ()
send (SSErvice state) key se = readMVar state >>=
	maybe (return ()) (flip writeChan se) . lookupChannel key

lookupChannel :: (Ord key) => key -> State key -> Maybe (Chan ServerEvent)
lookupChannel key = fmap (snd . head) . lookupGroup key
