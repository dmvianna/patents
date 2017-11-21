{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Patents where

import           Control.Applicative
import qualified Control.Foldl         as L
import qualified Data.Foldable         as F
import           Data.Proxy            (Proxy (..))
import           Data.Tuple

import           Data.Vinyl.Lens
import           Lens.Micro            ((%~))
import           Lens.Micro.Extras     (view)
import           Pipes                 (Pipe, Producer, mzero, runEffect, (>->))
import qualified Pipes.Prelude         as P

import qualified Data.Char             as C
import           Data.Readable
import           Data.Text             hiding (Text, take)
import qualified Data.Text             as T
import           Data.Typeable
import qualified Data.Vector           as V
import           Data.Vinyl            (Rec)
import           Frames                ((:->), Text)
import           Frames.ColumnUniverse
import           Frames.CSV            (colQ, columnNames, columnUniverse,
                                        declareColumn, readTableMaybe,
                                        readTableOpt, rowGen, rowTypeName,
                                        separator, tablePrefix, tableTypes')
import           Frames.Exploration    (pipePreview, pr, select)
import           Frames.Frame
import           Frames.InCore
import           Frames.Melt
import           Frames.Rec
import           Frames.RecF

import           PatAbstracts

declareColumn "patId" ''Text
declareColumn "abstract" ''Address
type PatColumns = '["id" :-> Text, "abstract" :-> Address]
type PA = Record PatColumns
type PAMaybe = Rec Maybe PatColumns

patStreamM :: Producer PAMaybe IO ()
patStreamM = readTableMaybe "data/pat_abstracts.csv"
-- patStreamM = readTableMaybe "../data/IPGOD.IPGOD122B_PAT_ABSTRACTS.csv"


printValidAddresses :: IO ()
printValidAddresses =
  runEffect $ patStreamM >-> P.map recMaybe >-> P.concat >-> P.print

