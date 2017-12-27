{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module Patents where

import           Addresses
import           Pipes         (Producer, (>->))
import qualified Pipes.Prelude as P

import           Data.Vinyl    (Rec)
import           Frames        ((:->), MonadSafe, Text, runSafeEffect)
import           Frames.CSV    (declareColumn, pipeTableMaybe, readFileLatin1Ln)
import           Frames.Rec
import           PatAbstracts  ()

declareColumn "patId" ''Text
declareColumn "abstract" ''AddressLocation
type PatColumns = '["id" :-> Text, "abstract" :-> AddressLocation]
type PA = Record PatColumns
type PAMaybe = Rec Maybe PatColumns

patStreamM :: MonadSafe m => Producer PAMaybe m ()
patStreamM = readFileLatin1Ln "data/pat_abstracts.csv" >-> pipeTableMaybe
-- patStreamM = readFileLatin1Ln "../data/IPGOD.IPGOD122B_PAT_ABSTRACTS.csv" >-> pipeTableMaybe

printValidAddresses :: IO ()
printValidAddresses =
  runSafeEffect $ patStreamM >-> P.map recMaybe >-> P.concat >-> P.print

