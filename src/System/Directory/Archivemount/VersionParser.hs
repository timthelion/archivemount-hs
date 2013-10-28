{-# LANGUAGE PackageImports #-}
module System.Directory.Archivemount.VersionParser
 (parseVersionInfo)
 where

import System.Directory.Archivemount.Types

import Numeric
 (readDec)

import "base" Data.Functor
 ((<$>))

import "base" Data.List
 (find
 ,isInfixOf)

import "split" Data.List.Split
 (splitOn)

parseVersionInfo
 :: String
 -> Version
parseVersionInfo unparsedVI =
 let
  vd   = versionDictionary unparsedVI
  lapv = lookupAndParseVersion vd
  amvM = lapv "archivemount"
  flvM = lapv "FUSE library"
  fmvM = lapv "fusermount"
  fkvM = lapv "FUSE kernel"
 in
 case (amvM,flvM,fmvM,fkvM) of
  (Just amv, Just flv, Just fmv, Just fkv) ->
   InstalledVersion
   {archivemount = amv
   ,fuse = flv
   ,fusermount = fmv
   ,fuseKernelInterface = fkv}
  _ -> InstalledButVersionInfoCouldNotBeParsed

lookupAndParseVersion
 :: [(String,String)] -- ^ Dictionary from (description, to version)
 -> String -- ^ A fuzzy match pattern to look up.
 -> Maybe [Int]
lookupAndParseVersion
 versionDict
 fuzzyMatch
 = lookupBy (isInfixOf fuzzyMatch) versionDict
 >>= (\upv->parseVersion upv)

parseVersion
 :: String
 -> Maybe [Int]
parseVersion unparsed
 = sequence
 $ map maybeReadInt
 $ splitOn "." unparsed

maybeReadInt
 :: String
 -> Maybe Int
maybeReadInt unparsed =
 case readDec unparsed of
  ((int,_):_) -> Just $ fromInteger int
  _ -> Nothing

lookupBy
 :: (a -> Bool)
 -> [(a,b)]
 -> Maybe b
lookupBy test list
 =   snd
 <$> find (\(a,b)->test a) list

versionDictionary
 :: String
 -> [(String,String)]
versionDictionary unparsedVI
 = map (\(ver,desc)->(reverse desc,reverse ver))
 $ map (\l->break (==' ') l)
 $ lines
 $ reverse unparsedVI