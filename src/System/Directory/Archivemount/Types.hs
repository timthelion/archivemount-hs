{-
This module describes the general shared types.
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