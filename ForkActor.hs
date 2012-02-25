module ForkActor (
) where

import Actor
import Mailbox
import Control.Monad
import Control.Concurrent
import PostOffice


data ActorImpl a = ActorImpl { actor   :: Actor a
							 , mailbox :: Mailbox a
							 , factory :: ActorFactory
							 , postOffice :: PostOffice
							 }



updateFactory :: (ActorImpl a) -> ActorFactory -> (ActorImpl a)
updateFactory (ActorImpl a mb _ po) f = ActorImpl a mb f po


runActor :: (ActorImpl a) -> (Behaviour a) -> IO ()
runActor a b = let bp = b (actor a) (factory a) in
			   let nextMsg = dequeue $ mailbox a in
	bp `liftM` nextMsg >>= (handleNext a)

handleNext :: (ActorImpl a) -> (Next a) -> IO ()
handleNext a (Terminate es)     = (handleEffect a) `mapM` es >> return ()
handleNext a (Continue b af es) = (handleEffect a) `mapM` es >> runActor a' b
	where a' = updateFactory a af


handleEffect :: (ActorImpl a) -> Effect -> IO ()
handleEffect ai (Spawn a b) = do
		mb <- createMailbox (postOffice ai) a
		af <- actorFactory
		ai <- return $ ActorImpl a mb af (postOffice ai)
		forkIO $ runActor ai b
		return ()
handleEffect ai (Send (Message a msg)) = sendTo ai a msg

sendTo :: (ActorImpl b) -> (Actor a) -> a -> IO ()
sendTo ai a msg = let po = postOffice ai in
		(getMailbox po a >>= send)
	where send (Just mb) = enqueue mb msg
	      send Nothing   = return ()

