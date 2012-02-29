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


data NextAction a msg = Loop
                      | ChangeAction (msg -> ActorState a msg (NextAction a msg))
                      | End

data ActorAcc a msg = ActorAcc { myself :: a msg
                               , effects :: [Effect]
                               , factory :: ActorFactory}

--TODO newtype?
type ActorState a msg r = State (ActorAcc a msg) r


self :: Actor a => ActorState a msg (a msg)
self = myself `liftM` get


spawn :: Actor actor => (ActorCreator (actor msg)) -> ActorState a b (actor msg)
spawn c = do
    (na, s') <- (createActor c) `liftM` get
    put s'
    return na

createActor c (ActorAcc a es af) = (actor as, ActorAcc a es' af')
    where (af', as) = c af
          es' = (Spawn as) : es

send :: Actor to => to msg -> msg -> ActorState a b ()
send a msg = (addMessage message) `liftM` get >>= put
    where message = Message a msg


addMessage m = addEffect $ Send m

addEffect :: Effect -> (ActorAcc a msg) -> (ActorAcc a msg)
addEffect e acc = updateEffects . (e:) . effects $ acc
    where updateEffects es = acc { effects = es }


runActor :: Actor a => (msg -> ActorState a msg (NextAction a msg)) -> Behaviour a msg
runActor f a af msg = case next of
        (ChangeAction f') -> cont f'
        Loop              -> cont f
        End               -> Terminate $ effects s
    where (next, s) = runState (f msg) (ActorAcc a [] af)
          cont nf = Continue (runActor nf) (factory s) (effects s)
