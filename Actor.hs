{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Puctor.Actor (
    ActorEnv,
    newEnv,
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
    show (ActorRef id) = "<" ++ (tail $ show id) ++ ">"


type RunActorEnv = ActorEnv -> IO ActorEnv
type ActorCreate actor msg = ActorRef msg -> ActorEnv -> RunActorEnv -> IO (actor msg)


data ActorBox msg = forall a. ActorImpl a => ActorBox (a msg)
instance ActorImpl ActorBox where
    sendMsg msg (ActorBox a) = sendMsg msg a

data NoopActor msg = NoopActor
instance ActorImpl NoopActor where
    sendMsg _ _ = return ()

data ActorBoxU = forall msg. ActorBoxU msg
type ActorMap = Map Id ActorBoxU

data ActorEnv = ActorEnv { actorMap  :: TVar ActorMap
                         , idSupply  :: IdSupply
                         , deferred  :: [Effect]}

newEnv :: IO ActorEnv
newEnv = ActorEnv <$> newTVarIO Map.empty <*> initIdSupply 'A' <*> return []

splitEnv :: ActorEnv -> (ActorEnv, ActorEnv)
splitEnv env = ((mapBoth $ updateIdSupply env) . splitIdSupply . idSupply) env
mapBoth f (a, b) = (f a, f b)
updateIdSupply env s = env { idSupply = s }


newActor :: ActorImpl actor => ActorCreate actor msg -> ActorEnv -> (ActorRef msg, ActorEnv)
newActor f e = (ref, addTo e')
    where (ref, e') = newActorRef e
          addTo = addDeferred $ Effect $ createActor f ref
addDeferred f env = env { deferred = deferred env ++ [f] }

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

performEffect :: ActorEnv -> Effect -> ActorEnv
performEffect env e = addDeferred e env

performEffects :: ActorEnv -> [Effect] -> ActorEnv
performEffects env e = foldl performEffect env e

applyDeferred :: ActorEnv -> IO ActorEnv
applyDeferred env = foldM (flip ($)) env (runEffect `map` deferred env)

send :: ActorRef msg -> msg -> Effect
send a msg = Effect $ send' a msg
send' (ActorRef aid) msg env = lookupActor aid <$> readTVarIO (actorMap env) >>= sendMsg msg >> return env
lookupActor aid = fromMaybe NoopActor . fmap unbox . Map.lookup aid

box :: ActorImpl a => a msg -> ActorBoxU
box = ActorBoxU . ActorBox
unbox :: ActorImpl a => ActorBoxU -> a msg
unbox (ActorBoxU box) = unsafeCoerce box
