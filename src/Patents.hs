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
import           Pipes             (Pipe, Producer, (>->))
import           Pipes.Internal    (Proxy)
import qualified Pipes.Prelude     as P

import           Data.Vinyl        (Rec)
import           Data.Vinyl.Lens
import           Frames            ((:->), MonadSafe, Text, runSafeEffect)
import           Frames.CSV        (declareColumn, pipeTableMaybe,
                                    readFileLatin1Ln)
import           Frames.Rec
import           Lens.Micro.Extras (view)
import           PatAbstracts      ()

declareColumn "patId" ''Text
declareColumn "abstract" ''AuAddress
type PatColumns = '["id" :-> Text, "abstract" :-> AuAddress]
type PA = Record PatColumns
type PAMaybe = Rec Maybe PatColumns

patStreamM :: MonadSafe m => Producer PAMaybe m ()
patStreamM = readFileLatin1Ln "data/pat_abstracts.csv" >-> pipeTableMaybe
-- patStreamM = readFileLatin1Ln "../data/IPGOD.IPGOD122B_PAT_ABSTRACTS.csv" >-> pipeTableMaybe

printValidAddresses :: IO ()
printValidAddresses =
  runSafeEffect $ patStreamM >-> P.map recMaybe >-> P.concat >-> P.print

-- filtering

isAuAddress :: AuAddress -> Bool
isAuAddress (AuAddress _ _) = True
isAuAddress _               = False

addresses :: (Abstract ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
addresses = P.filter (isAuAddress . view abstract)

deMaybe :: Monad m => Proxy () (Rec Maybe cs) () (Record cs) m r
deMaybe = P.map recMaybe >-> P.concat

runMaybeTake :: Int -> IO ()
runMaybeTake n =
  runSafeEffect $ patStreamM >-> deMaybe >-> addresses >-> P.take n >->  P.print
