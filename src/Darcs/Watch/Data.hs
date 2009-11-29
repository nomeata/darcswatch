{-
Copyright (C) 2009 Joachim Breitner

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

module Darcs.Watch.Data where

import Data.Time
import Data.Maybe

type BundleHash = String

-- | the path to the storage directory
type StorageConf = String

-- | A history entry is a state change
type BundleHistory = (UTCTime, Source, BundleState)

data BundleState
	= New
	| Applicable
	| Rejected
	| Obsoleted
	| Applied -- ^ Repository URL
	deriving (Read, Show, Ord, Eq)
	
data Source
	= ManualImport
	| ViaEMail String String String (Maybe String) -- ^ From, To, Subject, Message-Id 
	| ViaBugtracker String -- ^ URL
	| ViaWeb String -- ^ OpenID Username
	| ViaRepository RepositoryURL -- ^ Repository-URL
	deriving (Read, Show)

type RepositoryURL = String

type Author = String -- ^ the e-Mail-Adress of an patch author

data BundleList = RepositoryBundleList RepositoryURL
                | AuthorBundleList Author
		| UnmatchedBundleList

data RepositoryInfo = RepositoryInfo 
	{ lastCheck :: Maybe UTCTime
	, lastUpdate :: Maybe UTCTime
	}
	deriving (Read, Show)

data DarcsWatchConfig = DarcsWatchConfig {
        cRepositories :: [RepositoryURL],
	cData :: String,
        cOutput :: String,
        cMails :: String,
	cDarcsWatchURL :: String,
	cDarcsWatchAddress :: String,
	cSendRoundupMails :: Bool
        } deriving (Show, Read)


stateOfRepo history repo = fromMaybe New (lookup repo (repoStates history))

repoStates history = [ (repo, state)
	             | (_,ViaRepository repo, state) <- history ]
