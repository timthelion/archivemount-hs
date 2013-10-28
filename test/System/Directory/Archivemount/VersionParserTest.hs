{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Directory.Archivemount.VersionParserTest where

import System.Directory.Archivemount.VersionParser
import System.Directory.Archivemount.Types

import Test.Framework

test_validVersion = do
 assertEqual
  (InstalledVersion
   {archivemount=[0,8,2]
   ,fuse=[2,9,0]
   ,fusermount=[2,9,0]
   ,fuseKernelInterface=[7,18]})
  (parseVersionInfo
    (unlines
     ["archivemount version 0.8.2"
     ,"FUSE library version: 2.9.0"
     ,"fusermount version: 2.9.0"
     ,"using FUSE kernel interface version 7.18"]))

test_invalidVersion = do
 assertEqual
  (InstalledButVersionInfoCouldNotBeParsed)
  (parseVersionInfo
    (unlines
     ["FUSE library version: 2.9.0"
     ,"fusermount version: 2.9.0"
     ,"using FUSE kernel interface version 7.18"]))