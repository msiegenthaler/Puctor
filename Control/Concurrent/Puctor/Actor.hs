{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Puctor.Actor (
    ActorEnv,
    newEnv,
    boot,
    splitEnv,
    ActorRef,
    newActor,
    ActorCreate,
    ActorStepCtx,
    send,
    Effect,
    performEffect,
    performEffects,
    RunActorEnv,
    ActorImpl
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
import Control.Concurrent.Puctor.ActorImpl


newtype Effect = Effect { runEffect :: ActorEnv -> IO ActorEnv }
type ActorStepCtx msg = (ActorRef msg, ActorEnv)
data ActorRef msg = ActorRef Id deriving Eq

instance Show (ActorRef msg) where
    show (ActorRef aid) = showActorId aid

type RunActorEnv = ActorEnv -> IO ActorEnv
type ActorCreate actor msg = ActorRef msg -> ActorEnv -> RunActorEnv -> IO (actor msg)


data ActorBox msg = forall a. ActorImpl a => ActorBox (a msg)
instance ActorImpl ActorBox where
    sendMsg msg (ActorBox a) = sendMsg msg a

data ActorBoxU = forall msg. ActorBoxU msg
type ActorMap = Map Id ActorBoxU

data ActorEnv = ActorEnv { actorMap  :: TVar ActorMap
                         , idSupply  :: IdSupply
                         , deferred  :: [Effect]}

newEnv :: IO ActorEnv
newEnv = ActorEnv <$> newTVarIO Map.empty <*> initIdSupply 'A' <*> return []

splitEnv :: ActorEnv -> (ActorEnv, ActorEnv)
splitEnv env = (mapBoth (updateIdSupply env) . splitIdSupply . idSupply) env
mapBoth f (a, b) = (f a, f b)
updateIdSupply env s = env { idSupply = s }


showActorId aid = "<" ++ tail (show aid) ++ ">"

newActor :: ActorImpl actor => ActorCreate actor msg -> ActorEnv -> (ActorRef msg, ActorEnv)
newActor f e = (ref, addTo e')
    where (ref, e') = newActorRef e
          addTo = flip performEffect $ Effect $ createActor f ref

createActor :: ActorImpl actor => ActorCreate actor msg -> ActorRef msg -> ActorEnv -> IO ActorEnv
createActor f ref env = f ref nenv applyDeferred >>= registerActor env' ref
  where (nenv, env') = splitEnv env

registerActor env (ActorRef aid) a = atomically $ do
        m <- readTVar v
        let m' = Map.insert aid (box a) m
        writeTVar v m'
        return env
    where v = actorMap env

newActorRef :: ActorEnv -> (ActorRef msg, ActorEnv)
newActorRef = swap . fmap create . splitEnv
    where create = ActorRef . idFromSupply . idSupply
swap (a,b) = (b,a)

performEffect :: ActorEnv -> Effect -> ActorEnv
performEffect env e = performEffects env [e]

performEffects :: ActorEnv -> [Effect] -> ActorEnv
performEffects env e = env { deferred = deferred env ++ e}

applyDeferred :: ActorEnv -> IO ActorEnv
applyDeferred env = foldM (flip ($)) env' (runEffect `map` es)
    where es = deferred env
          env' = env { deferred = [] }

send :: ActorRef msg -> msg -> Effect
send a msg = Effect $ send' a msg
send' (ActorRef aid) msg env = lookupActor aid <$> readTVarIO tv >>= sendToBoxU msg >> return env
    where tv = actorMap env

lookupActor aid = fromMaybe (error errmsg) . Map.lookup aid
    where errmsg = "Actor " ++ showActorId aid ++ " does not exist in the environment"


box :: ActorImpl a => a msg -> ActorBoxU
box = ActorBoxU . ActorBox

sendToBoxU :: msg -> ActorBoxU -> IO ()
sendToBoxU msg (ActorBoxU a) = sendMsg msg (unsafeCoerce a :: ActorBox msg)


boot :: ActorImpl a => ActorCreate a () -> IO ()
boot c = (sendStart . newActor c) <$> newEnv >>= applyDeferred >> return ()
    where sendStart (a, e) = performEffect e $ send a ()
