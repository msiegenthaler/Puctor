module Control.Concurrent.Puctor.Pure.ActorState (
    ActorState,
    Behaviour,
    Next(..),
    self,
    spawn,
    (!),
    runActor
) where

import Control.Monad.State
import Control.Applicative
import Control.Concurrent.Puctor.Actor
import qualified Control.Concurrent.Puctor.Pure as P


data ActorAcc msg = ActorAcc { selfRef :: ActorRef msg
                             , env :: ActorEnv}

newtype ActorState msg r = ActorState (State (ActorAcc msg) r)
wrap = ActorState
unwrap (ActorState s) = s

instance Monad (ActorState msg) where
  x >>= f = wrap $ (unwrap x) >>= (unwrap . f)
  return r = wrap $ return r


data Next msg = Loop
              | ChangeTo (Behaviour msg)
              | End
type Behaviour msg = msg -> ActorState msg (Next msg)


self :: ActorState msg (ActorRef msg)
self = wrap $ selfRef <$> get

(!) :: ActorRef msg -> msg -> ActorState x ()
to ! msg = wrap $ sendAcc to msg <$> get >>= put

sendAcc to msg = updateEnv $ sendEnv to msg
sendEnv to msg env = performEffect env $ send to msg

updateEnv f acc = acc { env = f (env acc) }


spawn :: ActorImpl a => ActorCreate a msg -> ActorState x (ActorRef msg)
spawn cf = wrap $ do
    s <- get
    let (a, env') = newActor cf $ env s
    put $ s { env = env' }
    return a


runActor :: Behaviour msg -> P.Behaviour msg
runActor b (ref, e) msg = case next of
        Loop          -> P.Continue (runActor b)  e'
        (ChangeTo b') -> P.Continue (runActor b') e'
        End           -> P.Terminate e'
    where initial = ActorAcc ref e
          (next, s') = run (b msg) initial
          e' = env s'
run = runState . unwrap


