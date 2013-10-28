{-
This module describes the general shared types.

GPL3. License info is at the bottom of the file.
-}
module System.Directory.Archivemount.Types
 (MountStatus(Mounted,CouldNotMount)
 ,UnmountStatus(Unmounted,CouldNotUnmount)
 ,Version
   (InstalledVersion
      ,archivemount,fuse,fusermount,fuseKernelInterface
   ,NotInstalled
   ,InstalledButVersionInfoCouldNotBeParsed))
 where

data MountStatus = Mounted | CouldNotMount String deriving (Show)

data UnmountStatus = Unmounted | CouldNotUnmount String deriving (Show)

data Version
 = InstalledVersion
    {archivemount        :: [Int]
    ,fuse                :: [Int]
    ,fusermount          :: [Int]
    ,fuseKernelInterface :: [Int]}
 | NotInstalled
 | InstalledButVersionInfoCouldNotBeParsed
 deriving (Show,Eq)


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