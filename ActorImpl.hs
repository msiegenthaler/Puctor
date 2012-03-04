module Control.Concurrent.Puctor.ActorImpl (
	ActorImpl,
	sendMsg
) where

class ActorImpl actor where
    sendMsg :: msg -> actor msg -> IO ()

