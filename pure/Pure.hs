module Control.Concurrent.Puctor.Pure (
	Next(..),
	Behaviour
) where

import Control.Concurrent.Puctor.Actor

data Next actor msg = Terminate Effects
                    | Continue (Behaviour actor msg) ActorFactory Effects
type Behaviour actor msg = (actor msg) -> ActorFactory -> msg -> Next actor msg
