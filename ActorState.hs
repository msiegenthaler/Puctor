module ActorState (
    self,
    spawn,
    send,
    runActor
) where

import Data.Unique.Id
import Control.Monad.State
import Actor
import Mailbox
import Control.Arrow (second)



data ActorAcc a = ActorAcc { myself :: (Actor a)
                           , effects :: [Effect]
                           , factory :: ActorFactory
                           }

--TODO newtype?
type ActorState a r = State (ActorAcc a) r


self :: ActorState a (Actor a)
self = myself `liftM` get

spawn :: Behaviour a -> ActorState b (Actor a)
spawn b = do
        (a, s') <- createActor `liftM` get
        put (addSpawn a b s')
        return a

send :: (Actor a) -> a -> ActorState b ()
send a msg = (addMessage $ Message a msg) `liftM` get >>= put

createActor :: (ActorAcc a) -> (Actor b, ActorAcc a)
createActor acc = (second updateFactory) . newActor . factory $ acc
    where updateFactory fac = acc { factory = fac }

addEffect :: Effect -> (ActorAcc a) -> (ActorAcc a)
addEffect e acc = updateEffects . (e:) . effects $ acc
    where updateEffects es = acc { effects = es }

addMessage = addEffect . Send
addSpawn a b = addEffect $ Spawn a b


runActor :: (ActorState a b) -> (Actor a) -> ActorFactory -> (Effects, ActorFactory)
runActor s a af = out . snd $ runState s initial
    where initial = ActorAcc a [] af
          out acc = (reverse . effects acc, factory acc)
