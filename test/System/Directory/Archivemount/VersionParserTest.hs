{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-
This module tests how the version parser(System.Directory.Archivemount.VersionParser) handles valid and invalid input.
GPL3. License info is at the bottom of the file.
-}
module System.Directory.Archivemount.VersionParserTest where

import System.Directory.Archivemount.VersionParser
import System.Directory.Archivemount.Types

import Test.Framework

test_validVersion =
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

{-
-- Copyright (C) 2013 Timothy Hobbs <timothyhobbs@seznam.cz>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}