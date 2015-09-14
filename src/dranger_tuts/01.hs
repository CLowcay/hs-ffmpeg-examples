{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Media.FFMpeg
import qualified Data.Map as M
import System.Directory
import System.Environment
import System.Exit
import System.IO

main = do
	registerAll

	args <- getArgs
	pname <- getProgName

	case args of
		[fname] -> do
			fexists <- doesFileExist fname
			when (not fexists)$ do
				putStrLn$ "File not found " ++ fname
				exitFailure
			
			r <- runExceptT$ do
				(formatCtx, _) <- openInput fname Nothing Nothing
				findStreamInfo formatCtx M.empty

				dumpInputFormat formatCtx fname
				vstreams <- getStreams formatCtx >>= (filterM$ \is ->
					withStream formatCtx is$ \s ->
					withStreamCodecContext s$ \codec -> do
						t <- getField codecctx_codec_type codec
						return$ t == AVMediaTypeVideo)

				case vstreams of
					(v:_) -> do
						codecCtx <-
							withStream formatCtx v$ \s ->
							withStreamCodecContext s$ \codec ->
								copyAndOpenCodecContext codec Nothing

						readAndConvertFrames formatCtx codecCtx v
						
					_ -> throwError$ HSFFError HSFFErrorUser "" "No video stream found"

			case r of
				Left err -> do
					putStrLn$ formatError err
					exitFailure
				_ -> return ()
		_ -> do
			putStrLn$ "usage: " ++ pname ++ " <input_file>"
			putStrLn$ "Example program to decode a video stream"
			exitFailure

readAndConvertFrames ::
	AVFormatContext -> AVCodecContext -> StreamIndex -> ExceptT HSFFError IO ()
readAndConvertFrames formatCtx codecCtx videoStream = do
	width <- fromIntegral <$> getField codecctx_width codecCtx
	height <- fromIntegral <$> getField codecctx_height codecCtx
	fmt <- getNamedOption codec_pixel_format codecCtx

	frame <- frameAlloc
	frameRGB <- frameAlloc
	buffSize <- pictureGetSize PixFmtRgb24 width height

	pBuffer <- liftIO$ mallocBytes buffSize
	pictureFill frameRGB pBuffer PixFmtRgb24 width height

	err <- runExceptT$ do
		swsCtx <- getSwsContext
			(width, height, fmt)
			(width, height, PixFmtRgb24)
			(encodeSwsFlags (SwsFastBilinear, [], SwsSrcVChrDrop0))
			Nothing
			Nothing
			Nothing

		packet <- mkAVPacket
		
		let readAndConvert count = do
			isEOF <- readFrame formatCtx packet
			when (not isEOF)$ do
				si <- getField packet_stream_index packet
				if si /= videoStream then readAndConvert count
				else do
					(frameFinished, _) <- decodeVideo codecCtx frame packet
					if not frameFinished then readAndConvert count
					else do
						scale swsCtx frame 0 height frameRGB
						when ((count `mod` 5) == 0)$ do
							saveFrame frameRGB width height count
						readAndConvert$ count + 1

		readAndConvert 0

	liftIO$ free pBuffer	
	case err of
		Left e -> throwError e
		Right x -> return x

saveFrame :: MonadIO m => AVFrame -> Int -> Int -> Int -> m ()
saveFrame frame width height n = liftIO$ do
	hFile <- openFile ("frame" ++ (show n) ++ ".ppm") WriteMode
	hPutStrLn hFile "P6"
	hPutStrLn hFile$ (show width) ++ " " ++ (show height)
	hPutStrLn hFile "255"

	withDataPointers frame$ \((ptr, len):_) ->
		forM [0..(height - 1)]$ \y ->
			hPutBuf hFile (ptr `plusPtr` (y * len)) (width * 3)

	hClose hFile

