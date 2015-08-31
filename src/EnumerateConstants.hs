{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Media.FFMpeg
import qualified Data.Set as S

main :: IO ()
main = do
	codecOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVCodecContext)
	frameOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVFrame)
	subtitleOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVSubtitleRect)
	formatOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVFormatContext)
	swsOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass SwsContext)

	putStrLn "-- AVCodecContext"
	putStrLn "-- ========================"
	printConsts =<< getAllConstants (getClass :: AVClass AVCodecContext) codecOptions
	putStrLn ""

	putStrLn "-- AVFrame"
	putStrLn "-- ========================"
	printConsts =<< getAllConstants (getClass :: AVClass AVFrame) frameOptions
	putStrLn ""

	putStrLn "-- AVSubtitleRect"
	putStrLn "-- ========================"
	printConsts =<< getAllConstants (getClass :: AVClass AVSubtitleRect) subtitleOptions
	putStrLn ""

	putStrLn "-- AVFormatContext"
	putStrLn "-- ========================"
	printConsts =<< getAllConstants (getClass :: AVClass AVFormatContext) formatOptions
	putStrLn ""

	putStrLn "-- SwsContext"
	putStrLn "-- ========================"
	printConsts =<< getAllConstants (getClass :: AVClass SwsContext) swsOptions
	putStrLn ""

printConsts optConsts = forM_ optConsts$ \(opt, consts) -> do
	putStrLn$ "-- " ++ (show$ option_name opt)
	forM_ consts$ \const ->
		putStrLn$ (const_name const) ++ " = " ++ (show$ const_value const)
			++ " :: " ++ (haskType$ const_type const)

getAllConstants clazz = (fmap catMaybes) . (mapM$ \opt -> do
	consts <- getClassAVOptionConsts clazz opt
	if null consts then return Nothing else return$ Just (opt, consts))

filterDuplicates :: [AVOption a t] -> [AVOption a t]
filterDuplicates xs = filterDuplicates0 S.empty xs
	where
		filterDuplicates0 seen [] = []
		filterDuplicates0 seen (x:xs) =
			let n = show$ option_name x
			in if n `S.member` seen
				then filterDuplicates0 seen xs
				else x : filterDuplicates0 (n `S.insert` seen) xs

wordWrap :: Int -> String -> String -> String
wordWrap maxWidth prefix s = wordWrap0 s
	where wordWrap0 s =
		if null$ drop maxWidth s then s else
			let
				spaces = findIndices isSpace$ take maxWidth s
				(l1, l2) = if null spaces then splitAt maxWidth s
					else splitAt (last spaces) s
			in l1 ++ "\n" ++ (wordWrap0$ prefix ++ (dropWhile isSpace l2))

haskType :: AVOptType -> String
haskType x = case x of
	AVOptTypeFlags -> "CInt"
	AVOptTypeInt -> "CInt"
	AVOptTypeInt64 -> "Int64"
	AVOptTypeDouble -> "Double"
	AVOptTypeFloat -> "Float"
	AVOptTypeString -> "String"
	AVOptTypeRational -> "AVRational"
	AVOptTypeBinary -> "B.ByteString"
	AVOptTypeDict -> "AVDictionary"
	AVOptTypeImageSize -> "ImageSize"
	AVOptTypePixelFmt -> "PixelFormat"
	AVOptTypeSampleFmt -> "AVSampleFormat"
	AVOptTypeVideoRate -> "AVRational"
	AVOptTypeDuration -> "Int64"
	AVOptTypeColor -> "String"
	AVOptTypeChannelLayout -> "AVChannelLayout"
	_ -> "(NoType)"

