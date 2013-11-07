{-# LANGUAGE PackageImports #-}
{-
This module provides the public interface to the Archivemount library.
It provides both standard archivemount calls(mount/unmount)
as well as portable versions which can unpack a tarbal rather than mounting it,
when mounting is impossible.

GPL3. License info is at the bottom of the file.
-}
module System.Directory.Archivemount
 (Option(ReadOnly,NoBackup,NoSave,Subtree,OtherOption)
 ,MountStatus(Mounted,CouldNotMount)
 ,UnmountStatus(Unmounted,CouldNotUnmount,CouldNotUnmountDeviceOrResourceBusy)
 ,Version
   (InstalledVersion
      ,archivemount,fuse,fusermount,fuseKernelInterface
    ,NotInstalled
    ,InstalledButVersionInfoCouldNotBeParsed)
 ,archivemountVersion
 ,mountArchive
 ,unmountArchive
 ,mountArchivePortable
 ,unmountArchivePortable
 ,mountTarballForSavingPortable
 ,FileTreeMetaData
 ,saveAndUnmountNoncompressedTarballPortable) where

import System.Directory.Archivemount.Types
import System.Directory.Archivemount.VersionParser

import "base" System.Exit
  (ExitCode(ExitSuccess))

import "base" Data.Maybe
 (mapMaybe
 ,fromJust)

import "base" Data.List
 (intersperse
 ,isInfixOf)

import "base" Data.Functor
 ((<$>))

import "base" System.Environment
 (getEnvironment)

import "base" Control.Monad
 (filterM)

import qualified "tar" Codec.Archive.Tar as Tar
 (extract)

import qualified "containers" Data.Map as Map
 (toList
 ,fromList
 ,insert
 ,difference
 ,intersectionWith
 ,Map)

import "process" System.Process
 (readProcess
 ,readProcessWithExitCode
 ,proc
 ,env)

import "process-shortversions" System.Process.ShortVersions
 (commandExists
 ,runCommandInDir
 ,readCreateProcessWithExitCode)

import "directory" System.Directory
 (createDirectoryIfMissing
 ,doesDirectoryExist
 ,doesFileExist
 ,removeDirectory
 ,removeFile)

import "small-print" Control.Exception.SmallPrint
 ((*@)
 ,(*@@)
 ,exception)

import "filepath" System.FilePath
 (pathSeparators
 ,(</>)
 ,takeDirectory
 ,makeRelative)

import "filepath-extrautils" System.FilePath.ExtraUtils
 (getRealDirectoryContents
 ,getDirectoryContentsRecursive)

import qualified "bytestring" Data.ByteString as BS
 (readFile
 ,writeFile
 ,ByteString)

import "temporary" System.IO.Temp
 (withSystemTempDirectory)

import "unix-compat" System.PosixCompat.Files
 (createNamedPipe
 ,unionFileModes
 ,ownerReadMode
 ,ownerWriteMode
 ,FileStatus
 ,getFileStatus
 ,fileSize
 ,modificationTime)

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

-- | Returns NotInstalled if archivemount is not in the $PATH.
archivemountVersion :: IO Version
archivemountVersion =
 (do
  (_,versionInfo,errs) <- readProcessWithExitCode archivemountCommand ["-V"] ""
  return $ parseVersionInfo $ errs ++ versionInfo) *@@ archivemountNotInstalled
 where
 archivemountNotInstalled =
  exception
   (not <$> archivemountInstalled)
   (return NotInstalled)

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
 environment <- getEnvironment
 let unmountCommand = (proc fusermountCommand ["-u",whatToUnmount])
                      {env=Just $ Map.toList $ Map.insert "LANG" "C" $ Map.fromList environment}
 (exitCode,output,errors)
  <- readCreateProcessWithExitCode
      unmountCommand
      ""
 case exitCode of
  ExitSuccess -> return Unmounted
  _ -> (return $ CouldNotUnmount
        $ unlines
        [ "Standard ouput:"
        , output
        , "Errors:"
        , errors]) *@@ (deviceOrResourceBusy errors)
 where
  deviceOrResourceBusy errors =
   exception
    (return $ isInfixOf "Device or resource busy" errors)
    (return $ CouldNotUnmountDeviceOrResourceBusy)

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
mountArchivePortable archive mountpoint =
 mountArchive [NoSave] [] archive mountpoint *@@ nosaveNotSupported
 where
 nosaveNotSupported =
  exception
   (return True) -- (not <$> nosaveSupported) -- TODO fix permission denied bugs with archivemount
   (unpackArchive archive mountpoint)

-- | Unpack the archive to the given directory using tar.
unpackArchive
 :: FilePath -- ^ What to unpack
 -> FilePath -- ^ Where to unpack it
 -> IO MountStatus
unpackArchive archive unpackTo = do
 createDirectoryIfMissing True unpackTo
 Tar.extract unpackTo archive -- TODO Check for errors
 return Mounted

-- | Return true if archivemount is installed and the nosave option is supported.
nosaveSupported :: IO Bool
nosaveSupported = do
 v <- archivemountVersion
 case v of
  InstalledVersion{} -> return $ archivemount v >= [0,8,2]
  _ -> return False

-- | Unmount a mounted archive or delete an unpacked version of an archive.
-- WARNING: mount/unmountArchivePortable do not save changes.  You must do this yourself.
-- any changes to the archive in it's mounted state will be lost/deleted.
unmountArchivePortable
 :: FilePath -- ^ What to unmount/delete
 -> IO UnmountStatus
unmountArchivePortable toUnmount =
 (do
  status <- unmountArchive toUnmount
  removeDirectory toUnmount *@@ mountPointNoLongerExists *@@ archiveNotUnmounted status
  return status) *@@ nosaveNotSupported

  where
  archiveNotUnmounted status =
   exception
    (return $ status /= Unmounted)
    (return ())
  mountPointNoLongerExists =
   exception
    (not <$> doesDirectoryExist toUnmount)
    (return ())
  nosaveNotSupported =
   exception
    (return True) --(not <$> nosaveSupported) -- TODO fix Permission Denied bugs with archivemount
    (deleteUnpackedDirectoryTree toUnmount)

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

type FileTreeMetaData = [(FilePath,FileStatus)]

gatherFileTreeMetaData
 :: FilePath -- ^ root of file tree where we will gather the metadata
 -> IO FileTreeMetaData
gatherFileTreeMetaData root = do
 fileTree' <- getDirectoryContentsRecursive root
 statuses <- mapM getFileStatus fileTree'
 putStrLn $ unwords ["Root:",root]
 putStrLn $ unlines fileTree'
 let fileTree = map (\path->makeRelative root path) fileTree'
 return $ zip fileTree statuses

mountTarballForSavingPortable
 :: FilePath -- ^ what to mount(path to archive)
 -> FilePath -- ^ where to mount it (mountpoint)
 -> IO (MountStatus,Maybe FileTreeMetaData)
mountTarballForSavingPortable archive mountpoint = do
 status <- mountArchivePortable archive mountpoint
 fileTreeMetaData <- (Just <$> gatherFileTreeMetaData mountpoint) *@@ mountFailed status
 return (status,fileTreeMetaData)
 where
 mountFailed status =
  exception
   (return $ status /= Mounted)
   (return $ Nothing)

saveAndUnmountNoncompressedTarballPortable
 :: FileTreeMetaData -- ^ The metadata returned by 
 -> FilePath -- ^ Path to directory tree which we are saving
 -> FilePath -- ^ Path to tarbal to which we are saving
 -> IO UnmountStatus
saveAndUnmountNoncompressedTarballPortable before mountpoint archive = do
 after <- gatherFileTreeMetaData mountpoint
 let diff = mapDiff timeOrSizeChanged (Map.fromList before) (Map.fromList after)
 modifiedFiles <- filterM doesFileExist $ modified diff
 createdFiles <- filterM doesFileExist $ created diff
 modifiedFileContents <- mapM BS.readFile modifiedFiles
 createdFileContents <- mapM BS.readFile createdFiles
 status <- unmountArchivePortable mountpoint
 (do
  putStrLn "Deleted files"
  mapM putStrLn $ deleted diff
  putStrLn "Modified files"
  mapM putStrLn $ modified diff
  putStrLn "Created files"
  mapM putStrLn $ created diff
  deleteFromTar archive (deleted diff++modified diff)
  appendToTar archive $ zip (modifiedFiles ++ createdFiles) (modifiedFileContents ++ createdFileContents)
  return status) *@@ couldNotUnmount status
 where
 couldNotUnmount status = exception
  (return $ not $ status == Unmounted)
  (return status)

data Diff a =
 Diff
 {modified :: [a]
 ,created  :: [a]
 ,deleted  :: [a]}

mapDiff :: (Ord k) => (a->a->Bool)-> Map.Map k a -> Map.Map k a -> Diff k
mapDiff changeCheck before after =
 Diff
 {modified = map fst $ filter snd $ Map.toList $ Map.intersectionWith changeCheck before after
 ,created  = map fst $ Map.toList $ Map.difference after before
 ,deleted  = map fst $ Map.toList $ Map.difference before after}

timeOrSizeChanged :: FileStatus -> FileStatus -> Bool
timeOrSizeChanged before after
 =  fileSize before /= fileSize after
 || modificationTime before /= modificationTime after

deleteFromTar
 :: FilePath -- ^ Archive
 -> [FilePath] -- ^ What to delete from it
 -> IO ()
deleteFromTar archive whatToDelete = do
 _ <- readProcess tarCommand (("--file="++archive):"--delete":whatToDelete) ""
 return ()

appendToTar
 :: FilePath -- ^ archive
 -> [(FilePath,BS.ByteString)] -- ^ objects to append
 -> IO ()
appendToTar archive fileObjects = do
 withSystemTempDirectory (filter (\c->notElem c pathSeparators) archive) appendObjects
 return ()
 where
 appendObjects tempDir = mapM (appendObject tempDir) fileObjects
 appendObject tempDir (path,content) = do
  createDirectoryIfMissing True $ tempDir </> takeDirectory path
  putStrLn $ unwords ["Creating named pipe:",tempDir </> path,"tempdir is:",tempDir]
  createNamedPipe (tempDir </> path) (unionFileModes ownerReadMode ownerWriteMode)
  BS.writeFile (tempDir </> path) content
  runCommandInDir tempDir tarCommand ["-r","--file="++archive,path]
  removeFile (tempDir </> path)

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