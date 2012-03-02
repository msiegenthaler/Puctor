module Control.Concurrent.Puctor.Pure (
	Next(..),
	Behaviour,
	direct,
	loop
) where

import Control.Concurrent.Puctor.Actor

data Next actor msg = Terminate Effects
                    | Continue (Behaviour actor msg) ActorFactory Effects
type Behaviour actor msg = (actor msg) -> ActorFactory -> msg -> Next actor msg


effects (Terminate es) = es
effects (Continue _ _ es) = es


direct :: Actor a => (msg -> Effects) -> a msg -> ActorFactory -> msg -> Effects
direct f _ _ = f

loop :: Actor a => (a msg -> ActorFactory -> msg -> Effects) -> Behaviour a msg
loop b a af msg = Continue (loop b) af $ b a af msg
