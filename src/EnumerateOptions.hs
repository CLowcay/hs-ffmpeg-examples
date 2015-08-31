{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Media.FFMpeg
import qualified Data.Set as S

main :: IO ()
main = do
	codecOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVCodecContext)
	frameOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVFrame)
	subtitleOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVSubtitleRect)
	formatOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass AVFormatContext)
	swsOptions <- filterDuplicates <$> getClassAVOptions (getClass :: AVClass SwsContext)

	putStrLn "module Media.FFMpeg.Codec.Names ("
	putStrLn.concat.intersperse ",\n"$
		(optionExportName "codec" <$> codecOptions) ++
		(optionExportName  "frame" <$> frameOptions) ++
		(optionExportName "subtitle" <$> subtitleOptions)
	putStrLn ") where"
	putStrLn ""

	putStrLn "-- AVCodecContext:"
	putStrLn "-- ==========================="
	putStrLn ""

	forM_ codecOptions$ putStrLn.(showAVOption "codec" "AVCodecContext")

	putStrLn "-- AVFrame:"
	putStrLn "-- ==========================="
	putStrLn ""

	forM_ frameOptions$ putStrLn.(showAVOption "frame" "AVFrame")

	putStrLn "-- AVSubtitleRect:"
	putStrLn "-- ==========================="
	putStrLn ""

	forM_ subtitleOptions$ putStrLn.(showAVOption "subtitle" "AVSubtitleRect")

	putStrLn "module Media.FFMpeg.Format.Names ("
	putStrLn.concat.intersperse ",\n"$ optionExportName "format" <$> formatOptions
	putStrLn ") where"
	putStrLn ""

	putStrLn "-- AVFormatContext:"
	putStrLn "-- ==========================="
	putStrLn ""

	forM_ formatOptions$ putStrLn.(showAVOption "format" "AVFormatContext")

	putStrLn "module Media.FFMpeg.SwsContext.Names ("
	putStrLn.concat.intersperse ",\n"$ optionExportName "sws" <$> swsOptions
	putStrLn ") where"
	putStrLn ""

	putStrLn "-- SwsContext:"
	putStrLn "-- ==========================="
	putStrLn ""

	forM_ swsOptions$ putStrLn.(showAVOption "sws" "SwsContext")

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

optionExportName :: String -> AVOption a AVOptionValue -> String
optionExportName prefix (AVOption {..}) = prefix ++ "_" ++ (show option_name)

showAVOption :: String -> String -> AVOption a AVOptionValue -> String
showAVOption prefix classType opt@(AVOption {..}) =
	"-- | Option \"" ++ (show option_name) ++ "\" for " ++ classType
		++ " of type " ++ (show option_type) ++ unit ++ ".\n" ++ 
	(wordWrap 80 "-- " help) ++
	def ++
	varname ++ " :: OptionName " ++ classType ++ " " ++ (haskType option_type) ++ "\n" ++
	varname ++ " = OptionName \"" ++ (show option_name) ++ "\"\n"

	where
		varname = optionExportName prefix opt
		help = case option_help of
			Just h -> "-- " ++ h ++ "\n"
			Nothing -> ""
		def = case option_default of
			Just x -> "-- default value is " ++ (show x) ++ ".\n"
			Nothing -> ""
		unit = case option_unit of
			Just u -> "-" ++ u
			Nothing -> ""

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

