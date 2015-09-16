module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Media.FFMpeg
import System.Directory
import System.Environment
import System.Exit

main :: IO ()
main = do
	registerAll

	pname <- getProgName
	args <- getArgs

	case args of
		[fname] -> do
			fexists <- doesFileExist fname
			when (not fexists)$ do
				putStrLn$ "File not found " ++ fname
				exitFailure

			do
				(ctx, dict) <- openInput fname Nothing Nothing
				vals <- dictGetAll =<< getDictField ctx format_metadata
				if (null vals) then
					liftIO.putStrLn$ "No metadata"
				else
					forM_ vals$ \(key, val) -> do
						liftIO.putStrLn$ key ++ "=" ++ val

			`catch` \e -> do
					putStrLn$ show (e :: HSFFError)
					exitFailure

		_ -> do
			putStrLn$ "usage: " ++ pname ++ " <input_file>"
			putStrLn$ "example program to demonstrate the use of the libavformat metadata API.\n"
			exitFailure

