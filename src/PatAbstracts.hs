{-# LANGUAGE DataKinds, DeriveDataTypeable, TypeFamilies, TypeOperators #-}

module PatAbstracts where

import Control.Monad (mzero)
import qualified Data.Char as C
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Data.Typeable
import Frames.ColumnTypeable (Parseable)
import qualified Data.Vector as V
import Frames.InCore (VectorFor)
import Frames

-- fun types

data Addr = StatePost Text Int | NoAdress
  deriving (Eq, Ord, Show, Typeable)

type instance VectorFor Addr = V.Vector

instance Readable Addr where
  -- fromText t
  --   | T.length t == 5 = if all C.isDigit cs
  --                       then return $ StatePost s p
  --                       else return $ NoAddress
  --   | otherwise = mzero

instance Parseable Addr where

type MyColumns = Addr ': CommonColumns
