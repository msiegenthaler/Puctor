{-# LANGUAGE NoMonomorphismRestriction #-}

module ActorTest (
) where

import Control.Concurrent.Puctor.Actor
import Control.Concurrent.Puctor.Pure as P
import Control.Concurrent.Puctor.Pure.ActorState
import Control.Concurrent.Puctor.IO as I
import Control.Concurrent.Puctor.ForkActor as F
import Control.Concurrent.Puctor.ForkActorIO as FIO
import Control.Concurrent


start = do
    print "Start"
    boot $ F.actor main
    threadDelay 300000
    print "End"


main :: P.Behaviour ()
main = runActor $ \msg -> do
    e <- spawn $ FIO.actor echo
    e ! "One"
    g <- spawn $ F.actor greet
    g ! (e, "Mario")
    g ! (e, "Claudia")
    e ! "Two"
    --s <- spawn $ F.actor spawner
    --s ! "Two"
    return End


echo :: I.Behaviour String
echo (a, env) msg = do
    print $ "Actor " ++ (show a) ++ " got: " ++ msg
    return $ I.Continue echo env


test :: I.Behaviour ()
test (a, env) _ = do
    print $ "Started test as Actor " ++ (show a)
    return $ I.Terminate env


greet :: P.Behaviour (ActorRef String, String)
greet = runActor $ \(to, name) -> do
    to ! ("Hi " ++ name)
    return Loop

spawner :: P.Behaviour String
spawner = runActor $ \msg -> do
    e <- spawn $ FIO.actor echo
    e ! msg
    return End
