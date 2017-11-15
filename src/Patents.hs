{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Patents where

import Data.Tuple
import Control.Applicative
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Proxy (Proxy(..))

import Pipes (Producer, Pipe, runEffect, (>->), mzero)
import qualified Pipes.Prelude as P
import Lens.Micro ((%~))
import Lens.Micro.Extras (view)
import Data.Vinyl.Lens -- ∈ comes from here

import Frames (Text)
import Frames.ColumnUniverse
import Frames.Rec
import Frames.Frame
import Frames.Melt
import Frames.RecF
import Frames.Exploration (select, pr, pipePreview)
import Frames.InCore
import Frames.CSV ( tableTypes'
                  , separator
                  , rowGen
                  , rowTypeName
                  , readTableOpt
                  , columnNames
                  , colQ
                  , tablePrefix
                  , columnUniverse
                  )
import Data.Text hiding (Text, take)
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Readable
import qualified Data.Vector as V

import PatAbstracts

tableTypes' rowGen { rowTypeName = "PA"
                   , columnNames = [ "id", "abstract" ]
                   -- , separator = "|"
                   , tablePrefix = "p"
                   , columnUniverse = $(colQ ''MyColumns) }
  "data/pat_abstracts.csv"

patStream :: Producer PA IO ()
patStream = readTableOpt pAParser "data/pat_abstracts.csv"

loadPat :: IO (Frame PA)
loadPat = inCoreAoS patStream

-- a <- loadPat
-- mapM_ print (take 2 (F.toList a))
