{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Monoid
import Media.FFMpeg
import qualified Data.Map as M
import System.Directory
import System.Environment
import System.Exit

logPacket :: (Applicative m, MonadIO m, MonadError String m) => AVFormatContext -> AVPacket -> String -> m ()
logPacket ctx pkt tag = do
	sid <- getField packet_stream_index pkt
	mtime_base <- withStream ctx sid (getField avstream_time_base)

	pts <- ts2str <$> getField packet_pts pkt
	dts <- ts2str <$> getField packet_dts pkt
	dur <- ts2str . AVTimestamp . fromIntegral <$> getField packet_duration pkt
	pts_time <- case mtime_base of
		Just time_base -> ts2timestr <$> getField packet_pts pkt <*> pure time_base
		Nothing -> return "{Invalid time base}"
	dts_time <- case mtime_base of
		Just time_base -> ts2timestr <$> getField packet_dts pkt <*> pure time_base
		Nothing -> return "{Invalid time base}"
	dur_time <- case mtime_base of
		Just time_base -> ts2timestr <$> (AVTimestamp . fromIntegral <$> getField packet_duration pkt) <*> pure time_base
		Nothing -> return "{Invalid time base}"

	liftIO.putStrLn$ tag ++
		": pts:" ++ pts ++ " pts_time:" ++ pts_time ++
		" dts:" ++ dts ++ " dts_time:" ++ dts_time ++
		" duration:" ++ dur ++ " duration_time:" ++ dur_time ++
		" stream_index:" ++ (show sid)

main :: IO ()
main = do
	registerAll

	pname <- getProgName
	args <- getArgs

	case args of
		[inFilename, outFilename] -> do
			fexists <- doesFileExist inFilename
			when (not fexists)$ do
				putStrLn$ "File not found " ++ inFilename
				exitFailure

			r <- runExceptT$ do
				(ictx, _) <- openInput inFilename Nothing Nothing
				findStreamInfo ictx M.empty

				dumpInputFormat ictx inFilename -- write input format to stdout

				octx <- mkAVFormatOutputContext Nothing Nothing (Just outFilename)
				-- This is safe because we always get an AVOutputFormat from
				-- mkAVFormatOutputContext
				ofmt <- fromJust <$> formatGetOutputFormat octx

				streams <- getStreams ictx

				forM streams$ \iin ->
					withStream ictx iin$ \inStream ->
					withStreamCodecContext inStream$ \inCodecCtx -> do
						mcodec <- getField codecctx_codec inCodecCtx
						iout <- newStream octx mcodec
						withStream octx iout$ \outStream ->
							withStreamCodecContext outStream$ \outCodecCtx -> do
								copyCodecContext outCodecCtx inCodecCtx
								setNamedOption codec_codec_tag outCodecCtx 0
								oflags <- getField outformat_flags ofmt
								when (oflags `fhas` AVFmtGlobalheader)$ do
									modNamedOption codec_flags outCodecCtx (<> t CodecFlagGlobalHeader)
									return ()

				dumpOutputFormat octx outFilename -- write output format to stdout

				-- Open the output file if necessary
				oflags <- getField outformat_flags ofmt
				when (not (oflags `fhas` AVFmtNoFile))$ formatIOOpen octx outFilename AVIOFlagWrite

				writeHeader octx Nothing

				pkt <- mkAVPacket
				let rwloop = do
					eof <- readFrame ictx pkt
					when (not eof)$ do
						adjustPacket ictx octx pkt
						r <- lift.runExceptT$ interleavedWriteFrame octx pkt
						case r of
							Left err -> throwError err
							Right _ -> rwloop

				rwloop
				writeTrailer octx

			case r of
				Left err -> do
					putStrLn err
					exitFailure
				_ -> return ()
		_ -> do
			putStrLn$ "usage: " ++ pname ++ " <input_file> <output_file>"
			putStrLn$ "API example program to remux a media file with libavformat and libavcodec."
			putStrLn$ "The output format is guessed according to the file extension."
			exitFailure

adjustPacket :: (Applicative m, MonadIO m, MonadError String m) =>
	AVFormatContext -> AVFormatContext -> AVPacket -> m ()
adjustPacket inctx outctx pkt = do
	sid <- getField packet_stream_index pkt
	min_timebase <- withStream inctx sid$ getField avstream_time_base
	mout_timebase <- withStream outctx sid$ getField avstream_time_base

	case (min_timebase, mout_timebase) of
		(Just in_timebase, Just out_timebase) -> do
			let rflags = AVRoundNearInf <> AVRoundPassMinmax
			logPacket inctx pkt "in"
			modField packet_pts pkt$ \pts -> rescaleTSQRnd pts in_timebase out_timebase rflags
			modField packet_dts pkt$ \dts -> rescaleTSQRnd dts in_timebase out_timebase rflags
			modField packet_duration pkt$ \duration -> fromIntegral$ rescaleQ (fromIntegral duration) in_timebase out_timebase
			setField packet_pos pkt (-1)
			logPacket outctx pkt "out"
		_ -> throwError$ "adjustPacket: invalid timebase"

