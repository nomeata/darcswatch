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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module MultiMap
	( Singleton
	, empty
	, extend
	, append
	, (!!!!)
	, fromList
	) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.Foldable
import Prelude hiding (foldr)

class Monoid (c a) => Singleton c a where
	singleton :: a -> c a

instance Singleton [] a where
	singleton = (:[])
instance Ord a => Singleton S.Set a where
	singleton = S.singleton

empty :: (Ord k, Singleton c a) => M.Map k (c a) 
empty = M.empty

extend :: (Ord k, Singleton c a, Foldable c2) => k -> c2 a -> M.Map k (c a) -> M.Map k (c a)
extend key addValues map = foldr (append key) map addValues

append :: (Ord k, Singleton c a) => k -> a -> M.Map k (c a) -> M.Map k (c a)
append key addValue = M.insertWith (flip mappend) key (singleton addValue)

fromList :: (Ord k, Singleton c a) => [(k,a)] -> M.Map k (c a)
fromList = foldr (uncurry append) empty

(!!!!) :: (Ord k, Singleton c a) => M.Map k (c a) -> k -> c a
m !!!! key = M.findWithDefault mempty key m
