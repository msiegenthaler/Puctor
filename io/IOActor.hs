module Control.Concurrent.Puctor.IO (
	Next(..),
	Behaviour,
	direct,
	loop,
	fromPure
) where

import Control.Monad
import Control.Concurrent.Puctor.Actor
import qualified Control.Concurrent.Puctor.Pure as P

data Next actor msg = Terminate Effects
                    | Continue (Behaviour actor msg) ActorFactory Effects
type Behaviour actor msg = (actor msg) -> ActorFactory -> msg -> IO (Next actor msg)


effects (Terminate es) = es
effects (Continue _ _ es) = es

direct :: Actor a => (msg -> IO Effects) -> a msg -> ActorFactory -> msg -> IO Effects
direct f _ _ = f

direct2 :: Actor a => (msg -> IO b) -> a msg -> ActorFactory -> msg -> IO Effects
direct2 f _ _ msg = f msg >> return []

loop :: Actor a => (a msg -> ActorFactory -> msg -> IO Effects) -> Behaviour a msg
loop b a af msg = again `liftM` (b a af msg)
    where again es = Continue (loop b) af es


fromPure :: P.Behaviour a msg -> Behaviour a msg
fromPure b a af msg = return $ case b a af msg of
	P.Continue b' af es -> Continue (fromPure b) af es
	P.Terminate es      -> Terminate es