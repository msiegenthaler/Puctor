module Control.Concurrent.Puctor.ForkActorIO (
    actor,
    ForkActor
) where

import Control.Concurrent.Puctor.IO
import Control.Concurrent.Puctor.Actor
import Control.Concurrent.Puctor.ActorImpl
import Control.Concurrent.Puctor.ForkActor.Mailbox
import Control.Monad
import Control.Applicative
import Control.Concurrent


data ForkActor msg = ForkActor (ActorRef msg) RunActorEnv (Mailbox msg)
instance ActorImpl ForkActor where
    sendMsg msg (ForkActor _ _ mb) = mb `enqueue` msg


actor :: Behaviour msg -> ActorCreate ForkActor msg
actor b ref env run = ForkActor ref run <$> newMailbox >>= spawnActor
    where spawnActor a = (forkIO $ runActor a b env) >> return a

runActor :: ForkActor msg -> Behaviour msg -> ActorEnv -> IO ()
runActor a b env = nextMsg a >>= handle >>= handleNext a
    where handle = b (ref a, env)

nextMsg = dequeue . mailbox
    
mailbox (ForkActor _ _ mb) = mb
ref (ForkActor ref _ _) = ref
runEnv (ForkActor _ r _) = r

handleNext :: ForkActor msg -> Next msg -> IO ()
handleNext a (Continue b env) = runEnv a env >>= runActor a b
handleNext a (Terminate env)  = runEnv a env >> return ()
