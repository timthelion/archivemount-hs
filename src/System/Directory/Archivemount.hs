{-# LANGUAGE PackageImports #-}
module System.Directory.Archivemount
 (Option(ReadOnly,NoBackup,NoSave,Subtree,OtherOption)
 ,MountStatus(Mounted,CouldNotMount)
 ,UnmountStatus(Unmounted,CouldNotUnmount)
 ,Version
   (InstalledVersion
      ,archivemount,fuse,fusermount,fuseKernelInterface
    ,NotInstalled
    ,InstalledButVersionInfoCouldNotBeParsed)
 ,archivemountVersion
 ,mountArchive
 ,unmountArchive
 ,mountArchivePortable
 ,unmountArchivePortable) where

import System.Directory.Archivemount.Types
import System.Directory.Archivemount.VersionParser

import System.Exit
  (ExitCode(ExitSuccess))

import Data.Maybe
 (mapMaybe)

import "base" Data.List
 (intersperse)

import "process" System.Process
 (readProcess
 ,readProcessWithExitCode)

import "process-shortversions" System.Process.ShortVersions
 (commandExists
 ,runCommandInDir)

import "directory" System.Directory
 (createDirectoryIfMissing
 ,doesDirectoryExist
 ,removeDirectory)

-- | See `archivemount -h` for details.
data Option
 = ReadOnly
 | NoBackup
 | NoSave
 | Subtree String
 | OtherOption String
 -- ^ Too lazy to write them all out...
 -- Passed to archivemount like so:
 -- [OtherOption "a",OtherOption "b"] => archivemount -o a,b

optsToArgs
 :: [Option]
 -> [String]
optsToArgs
 options
 = "-o"
 : (intersperse "," $ map optToArg options)

optToArg :: Option -> String
optToArg ReadOnly = "readonly"
optToArg NoBackup = "nobackup"
optToArg NoSave = "nosave"
optToArg (Subtree match) = "subtree="++match
optToArg (OtherOption option) = option

archivemountCommand = "archivemount"
fusermountCommand = "fusermount"
tarCommand = "tar"

-- | Returns Nothing if archivemount is not in the $PATH.
archivemountVersion :: IO Version
archivemountVersion = do
 installed <- archivemountInstalled
 case installed of
  False -> return NotInstalled
  True -> do
   (_,versionInfo,errs) <- readProcessWithExitCode archivemountCommand ["-V"] ""
   return $ parseVersionInfo $ errs ++ versionInfo

archivemountInstalled :: IO Bool
archivemountInstalled =
 commandExists archivemountCommand

mountArchive
 :: [Option] -- ^ Standard options to be passed to archivemount
 -> [String] -- ^ Arguments to be passed to archivemount as raw strings.
 -> FilePath -- ^ What to mount(path to the archive to be mounted)
 -> FilePath -- ^ Where to mount it(the desired mount point).  If it doesn't exist, this directory will be created.
 -> IO MountStatus -- ^ Did the mount succeed
mountArchive options args archive mountPoint
 = do
 createDirectoryIfMissing True mountPoint
 (exitCode,output,errors)
  <- readProcessWithExitCode
      archivemountCommand
      (  args
      ++ optsToArgs options
      ++ [archive
         ,mountPoint])
      ""
 case exitCode of
  ExitSuccess -> return Mounted
  _ -> return $ CouldNotMount
        $ unlines
        [ "Standard ouput:"
        , output
        , "Errors:"
        , errors]

unmountArchive :: FilePath -> IO UnmountStatus
unmountArchive whatToUnmount = do
 (exitCode,output,errors)
  <- readProcessWithExitCode
      fusermountCommand
      ["-u",whatToUnmount]
      ""
 case exitCode of
  ExitSuccess -> return Unmounted
  _ -> return $ CouldNotUnmount
        $ unlines
        [ "Standard ouput:"
        , output
        , "Errors:"
        , errors]

-- | Mount an archive in a portable fashion.
-- If archivemount >= 0.8.2 is available,
-- then mount with archivemount and the nosave option.
-- Otherwise, unpack the archive to the mountpoint.
-- If the mount point does not exist, create it.
-- WARNING: mount/unmountArchivePortable do not save changes.  You must do this yourself.
-- any changes to the archive in it's mounted state will be lost/deleted.
mountArchivePortable
 :: FilePath -- ^ What to mount (path to archive)
 -> FilePath -- ^ Where to mount it (mountpoint)
 -> IO MountStatus
mountArchivePortable archive mountpoint = do
 nss <- nosaveSupported
 case nss of
  True  -> mountArchive [NoSave] [] archive mountpoint
  False -> unpackArchive archive mountpoint

-- | Unpack the archive to the given directory using tar.
unpackArchive
 :: FilePath -- ^ What to unpack
 -> FilePath -- ^ Where to unpack it
 -> IO MountStatus
unpackArchive archive unpackTo = do
 createDirectoryIfMissing True unpackTo
 (exitCode,output,errors)<- readProcessWithExitCode
     tarCommand
     ["-xaf",archive,"-C",unpackTo]
     ""
 case exitCode of
  ExitSuccess -> return Mounted
  _ -> return $ CouldNotMount $ unlines ["Output:",output,"Errors:",errors]

-- | Return true if archivemount is installed and the nosave option is supported.
nosaveSupported :: IO Bool
nosaveSupported = do
 v <- archivemountVersion
 case v of
  InstalledVersion{} ->
   case archivemount v >= [0,8,2] of
    True  -> return True
    False -> return False
  _ -> return False

-- | Unmount a mounted archive or delete an unpacked version of an archive.
-- WARNING: mount/unmountArchivePortable do not save changes.  You must do this yourself.
-- any changes to the archive in it's mounted state will be lost/deleted.
unmountArchivePortable
 :: FilePath -- ^ What to unmount/delete
 -> IO UnmountStatus
unmountArchivePortable toUnmount = do
 nss <- nosaveSupported
 case nss of
  True  -> do
   status <- unmountArchive toUnmount
   case status of
    Unmounted -> do
     stillExists <- doesDirectoryExist toUnmount
     case stillExists of
      True  -> removeDirectory toUnmount
      False -> return ()
    _ -> return ()
   return status
  False -> deleteUnpackedDirectoryTree toUnmount
-- | Delete a directory recursively.  If an exception occures, report CouldNotUnmount
deleteUnpackedDirectoryTree
 :: FilePath -- ^ Path to directory to be deleted.
 -> IO UnmountStatus
deleteUnpackedDirectoryTree dir = do
 (exitCode,output,errors) <-
  readProcessWithExitCode "rm" ["-rf",dir] ""
 case exitCode of
  ExitSuccess -> return Unmounted
  _ -> return $ CouldNotUnmount $ unlines ["Output:",output,"Errors:",errors]