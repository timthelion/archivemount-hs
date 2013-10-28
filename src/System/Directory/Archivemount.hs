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
 ,unmountArchive) where

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
 -> FilePath -- ^ Where to mount it(the desired mount point)
 -> IO MountStatus -- ^ Did the mount succeed
mountArchive options args archive mountPoint
 = do
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
