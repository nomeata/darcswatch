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
	, lockRestart
	) where

import System.FilePath
import System.Directory
import System.IO
import System.IO.Error
import System.Posix.Temp
import Control.Monad
import Control.Exception (finally)
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


-- | Convenience function combining the two functions above.
--
--   Runs the fifth argument repeatedly, locked using the directory in the 
--   first argument, passed the current event the first time, and using the
--   combination function the other times.
--
--   If the forth argument is true, this function will tell the user whats
--   happening (using putStrLn)
lockRestart :: (Show a, Read a) =>
		   FilePath
                -> a
		-> ([a] -> a)
		-> Bool
		-> (a -> IO ())
		-> IO ()
lockRestart dir event combine verbose action = do
	when verbose $ putStrLn "Trying to get lock..."
	l <- lockOrMark dir event
	if l then do
	    when verbose $ putStrLn "Got the lock, going to work..."
	    workAndCheck event
	  else
	    when verbose $ putStrLn "Could not get locked, signaled restart"
  where workAndCheck event = finally (action event) $ do
	when verbose $ putStrLn "Trying to release the lock..."
	msgs <- releaseLock dir
	if null msgs then do
	    when verbose $ putStrLn "Successfully released lock."
	  else do
	    when verbose $ putStrLn "New work to do..."
	    workAndCheck (combine msgs)

