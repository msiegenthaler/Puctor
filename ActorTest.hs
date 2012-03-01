module ActorTest (
) where

import Control.Concurrent.Puctor.Actor
import Control.Concurrent.Puctor.Pure
import Control.Concurrent.Puctor.ForkActor

import System.IO.Unsafe
import Control.Concurrent


handle :: Actor a => Behaviour a String
handle _ af msg = unsafePerformIO $ do 
	i <- myThreadId
	print $ msg ++ " [" ++ (show i) ++ "]"
	return $ Continue handle af []


start = do
	i <- myThreadId
	print $ "Start " ++ (show i)
	a <- boot $ newActor handle
	a ! "Hi"
	a ! "Ho"
	a ! "Let's go"
	print $ "End " ++ (show i)


