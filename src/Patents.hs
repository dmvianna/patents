{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module Patents where

import           Addresses
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.IO.Class (MonadIO)
import           Pipes                  (Pipe, Producer, (>->))
import           Pipes.Internal         (Proxy)
import qualified Pipes.Prelude          as P

-- import           AddressCsv
import           Data.Vinyl             (Rec)
import           Data.Vinyl.Lens
import           Frames                 ((:->), MonadSafe, SafeT, Text,
                                         runSafeEffect)
import           Frames.CSV             (declareColumn, pipeTableMaybe,
                                         readFileLatin1Ln)
import           Frames.Rec
import           Lens.Micro.Extras      (view)
import           PatAbstracts           ()

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

auAddresses :: (Abstract ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
auAddresses = P.filter (isAuAddress . view abstract)

deMaybe :: Monad m => Proxy () (Rec Maybe cs) () (Record cs) m r
deMaybe = P.map recMaybe >-> P.concat

runMaybeTake :: Int -> IO ()
runMaybeTake n =
  runSafeEffect $
  patStreamM >->
  deMaybe >->
  auAddresses >->
  P.take n >->
  P.print

-- further modularising

patStreamTake :: (Show b, MonadMask m, MonadIO m) => Proxy () (Record PatColumns) () b (SafeT m) () -> Int -> m ()
patStreamTake target n =
  runSafeEffect $
  patStreamM >->
  deMaybe >->
  target >->
  P.take n >->
  P.print

isStreetAddress :: AddressLocation -> Bool
isStreetAddress (AStreetAddress _) = True
isStreetAddress _                  = False

streetAddresses :: (Abstract ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
streetAddresses = P.filter (isStreetAddress . _addrLocation . view abstract)

vicAddr :: (Abstract ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
vicAddr = P.filter ((== (State "vic")) . _state . _addrLocality . view abstract)

-- patStreamTake vicAddr 3 -- will render Victorian addresses
-- patStreamTake streetAddresses 3 -- will render Street addresses
-- patStreamTake (streetAddresses >-> vicAddr) 3 -- the intersection

isPobox :: AddressLocation -> Bool
isPobox (APobox _) = True
isPobox _          = False

poboxes :: (Abstract ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
poboxes = P.filter (isPobox . _addrLocation . view abstract)

