module Control.Concurrent.Puctor.IO (
	Next(..),
	Behaviour,
	env,
	fromPure,
	loop
) where

import Control.Monad
import Control.Applicative
import Control.Concurrent.Puctor.Actor
import qualified Control.Concurrent.Puctor.Pure as P


type Behaviour msg = ActorStepCtx msg -> msg -> IO (Next msg)
data Next msg = Terminate ActorEnv
              | Continue (Behaviour msg) ActorEnv

env (Terminate env) = env
env (Continue _ env) = env

loop :: (msg -> IO [Effect]) -> Behaviour msg
loop f (_, env) msg = Continue (loop f) <$> performEffects env <$> f msg

fromPure :: P.Behaviour msg -> Behaviour msg
fromPure b ctx msg = return $ case b ctx msg of
	P.Continue b' env -> Continue (fromPure b') env
	P.Terminate env   -> Terminate env
