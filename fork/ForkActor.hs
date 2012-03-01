module Control.Concurrent.Puctor.ForkActor (
	newActor,
	ForkActor
) where

import Control.Concurrent.Puctor.Pure
import Control.Concurrent.Puctor.Actor
import Control.Concurrent.Puctor.ForkActor.Mailbox
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe


data ForkActor msg = ForkActor ActorId (Mailbox msg)

instance Actor ForkActor where
    actorId   (ForkActor aid _)    = aid
    (!) (ForkActor _ mb) msg = mb `enqueue` msg

type ForkBehaviour msg  = Behaviour ForkActor msg
type ForkSpawn msg = ActorSpawn (ForkActor msg)


newActor :: (ForkBehaviour msg) -> ActorCreator (ForkActor msg)
newActor b af = (afa, ActorSpawn a (startActor a afb b))
    where (af', afa) = splitActorFactory af
          (aid, afb) = newActorId af'
          mb = createMailbox
          a = ForkActor aid mb

--TODO impure forkA

createMailbox :: Mailbox msg
createMailbox = unsafePerformIO newMailbox

actorMailbox :: (ForkActor msg) -> Mailbox msg
actorMailbox (ForkActor _ mb) = mb

nextMsg :: (ForkActor msg) -> IO msg
nextMsg = dequeue . actorMailbox


startActor a af b = (forkIO $ runActor a af b) >> return ()

runActor :: (ForkActor msg) -> ActorFactory -> (ForkBehaviour msg) -> IO ()
runActor a af b = bp `liftM` (nextMsg a) >>= (handleNext a)
    where bp = b a af

handleNext :: (ForkActor msg) -> (Next ForkActor msg) -> IO ()
handleNext a (Terminate es)     = handleEffect `mapM` es >> return ()
handleNext a (Continue b af es) = handleEffect `mapM` es >> runActor a af b

handleEffect :: Effect -> IO ()
handleEffect (Send (Message to msg)) = to ! msg
handleEffect (Spawn (ActorSpawn _ io)) = io
