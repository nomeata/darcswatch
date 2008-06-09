{-
Copyright (C) 2008 Joachim Breitner

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.
-}
module LockRestart
	( lockOrMark
	, releaseLock
	) where

import System.FilePath
import System.Directory
import System.IO
import System.IO.Error
import System.Posix.Temp
import Control.Monad
import GHC.IOBase

-- | tries to get the lock. If it fails, notifies the running process
--   to re-start itself afterwards, with the given information
--   returns True if the lock was aquired
lockOrMark :: Show a => FilePath -> a -> IO Bool
lockOrMark dir msg = do 
  (createDirectory lockDir >> return True) `catch` \err -> do
    if isAlreadyExistsError err then markOrLock dir msg
                                else ioError err
  where lockDir = dir </> ".lock"

markOrLock :: Show a => FilePath -> a -> IO Bool
markOrLock dir msg = do 
  (file, fh) <- openTempFile dir "lock.tmp"
  hPutStr fh (show msg)
  hClose fh
  (renameFile file (replaceDirectory file lockDir) >> return False) `catch` \err -> do
    if isDoesNotExistError err then do removeFile file
                                       markOrLock dir msg
                               else    ioError err
  where lockDir = dir </> ".lock"

-- | release the lock. If new events have come in, they are returned
--   in the list, and the lock is still kept. If the list is empty,
--   the lock was successfully released.
releaseLock :: Read a => FilePath -> IO [a]
releaseLock dir = do
  files <- pickFiles `liftM` getDirectoryContents lockDir
  if (null files) then (removeDirectory lockDir >> return []) `catch` \err ->
      if ioe_type err == UnsatisfiedConstraints then releaseLock dir else ioError err
    else do
      mapM (\f -> do
    	c <- readFile f
        when (c==c) $ removeFile f
        return (read c)
        ) files
  where interesting "." = False
        interesting ".." = False
	interesting _ = True
	pickFiles = map (lockDir </>) . filter interesting
        lockDir = dir </> ".lock"
