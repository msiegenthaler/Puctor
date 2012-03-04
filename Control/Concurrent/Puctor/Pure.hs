module Control.Concurrent.Puctor.Pure (
    Next(..),
    Behaviour,
    env,
    loop
) where

import Control.Concurrent.Puctor.Actor

type Behaviour msg = ActorStepCtx msg -> msg -> Next msg
data Next msg = Terminate ActorEnv
              | Continue (Behaviour msg) ActorEnv


env (Terminate env) = env
env (Continue _ env) = env


loop :: (msg -> [Effect]) -> Behaviour msg
loop f (_, env) msg = Continue (loop f) $ performEffects env $ f msg
