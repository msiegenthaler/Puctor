module ActorTest (
	sendSome,
	sendSome2
) where

import Actor

sendSome = do
        a <- spawn printact
        send "Hi" a
        send "Huhu" a
        return a 
    where printact = loop $ print . render
          render = ("Received " ++) . show    

sendSome2 = do
        a <- spawn $ once $ print . render
        send "Claudia" a
        send "Mario" a
        return a
    where render = ("Hi " ++) . show    
