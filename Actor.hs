{-# LANGUAGE ExistentialQuantification #-}

module Actor (
    Actor,
    Action(..),
    ActorResult(..),
    spawn,
    send,
    once,
    loop
) where


import Control.Concurrent
import Mailbox

data Actor a = Actor { mailbox :: Mailbox a
                     , actorId :: String
                     }
instance Show (Actor a) where
    show a = "Actor " ++ (actorId a)

data Action = forall a. Send { to :: Actor a
                             , msg :: a}
            | forall a. Create (Behaviour a)

type Behaviour a = a -> ActorResult a

data ActorResult a = Terminate
                   | Continue (Behaviour a) [Action]
                   | ActorIO (IO (ActorResult a))


spawn :: (Behaviour a) -> (IO (Actor a))
spawn b = do
        mb <- newMailbox
        tid  <- forkIO $ runActor mb b
        return $ Actor mb $ tid2id tid
    where tid2id tid = show tid


send :: a -> (Actor a) -> (IO ())
send msg actor = enqueue mb msg
    where mb = mailbox actor 


runActor :: (Mailbox a) -> (Behaviour a) -> (IO ())
runActor mb b = dequeue mb >>= handle . b >>= step
    where step (Just b') = runActor mb b'
          step Nothing = return ()

handle :: (ActorResult a) -> (IO (Maybe (Behaviour a)))
handle Terminate = return Nothing
handle (Continue nb as) = processAction `mapM` as >> (return $ Just nb)
    where processAction (Send (Actor mb _) msg) = enqueue mb msg
          processAction (Create b) = spawn b >> (return ())
handle (ActorIO io) = io >>= handle


once :: (a -> IO b) -> (Behaviour a)
once f msg = ActorIO (f msg >> return Terminate)

loop :: (a -> IO b) -> (Behaviour a)
loop f msg = ActorIO (f msg >> return cont)
    where cont = Continue (loop f) []

