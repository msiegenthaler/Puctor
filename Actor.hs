{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Puctor.Actor (
    ActorEnv,
    newEnv,
    splitEnv,
    newActor,
    ActorCreate,
    send,
    Effect    
) where

import Data.Tuple
import Data.Maybe
import Data.Unique.Id
import Control.Monad.State
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM
import Unsafe.Coerce


class Actor actor where
    sendMsg :: msg -> actor msg -> IO ()


type ActorCreate actor msg = ActorRef msg -> IO (actor msg)
type Effect = ActorEnv -> IO ()


data ActorRef msg = ActorRef Id deriving Eq

instance Show (ActorRef msg) where
    show (ActorRef id) = "<" ++ (tail $ show id) ++ ">"



data ActorBox msg = forall a. Actor a => ActorBox (a msg)
instance Actor ActorBox where
    sendMsg msg (ActorBox a) = sendMsg msg a

data NoopActor msg = NoopActor
instance Actor NoopActor where
    sendMsg _ _ = return ()

data ActorBoxU = forall msg. ActorBoxU msg
type ActorMap = Map Id ActorBoxU

data ActorEnv = ActorEnv { actorMap  :: TVar ActorMap
                         , idSupply  :: IdSupply
                         , deferred  :: [ActorEnv -> IO ActorEnv]}



newEnv :: IO ActorEnv
newEnv = ActorEnv <$> newTVarIO Map.empty <*> initIdSupply 'A' <*> return []

splitEnv :: ActorEnv -> (ActorEnv, ActorEnv)
splitEnv env = ((mapBoth $ updateIdSupply env) . splitIdSupply . idSupply) env
mapBoth f (a, b) = (f a, f b)
updateIdSupply env s = env { idSupply = s }


newActor :: Actor actor => ActorCreate actor msg -> ActorEnv -> (ActorRef msg, ActorEnv)
newActor f e = (ref, addTo e')
    where (ref, e') = newActorRef e
          addTo = addDeferred $ createActor f ref
addDeferred f env = env { deferred = deferred env ++ [f] }

createActor :: Actor actor => ActorCreate actor msg -> ActorRef msg -> ActorEnv -> IO ActorEnv
createActor f ref env = f ref >>= registerActor env ref >> return env
registerActor env (ActorRef aid) a = atomically $ do
        m <- readTVar v
        let m' = Map.insert aid (box a) m
        writeTVar v m'
    where v = actorMap env

newActorRef :: ActorEnv -> (ActorRef msg, ActorEnv)
newActorRef = swap . fmap create . splitEnv
    where create = ActorRef . idFromSupply . idSupply




send :: ActorRef msg -> msg -> Effect
send (ActorRef aid) msg env = lookupActor aid <$> readTVarIO (actorMap env) >>= sendMsg msg
lookupActor aid = fromMaybe NoopActor . fmap unbox . Map.lookup aid

box :: Actor a => a msg -> ActorBoxU
box = ActorBoxU . ActorBox
unbox :: Actor a => ActorBoxU -> a msg
unbox (ActorBoxU box) = unsafeCoerce box
