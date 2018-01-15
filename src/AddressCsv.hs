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

module AddressCsv where

import           Addresses
import           Data.Functor.Identity
import qualified Pipes.Prelude         as P

import           Data.Vinyl
import           Frames                ((:->), pattern Nil, Text)
import           Frames.CSV            (produceCSV)
import           Frames.Rec
import           PatAbstracts          ()

-- sample data

tPobox :: AuAddress
tPobox = AuAddress { _addrLocation = APobox (Gpo "1285")
                        , _addrLocality = Locality { _suburb = Suburb "melbourne"
                                                   , _state = State "vic"
                                                   , _postcode = Postcode "3001"}
                        }

tAddr :: AuAddress
tAddr = AuAddress {_addrLocation =
                      AStreetAddress (
                      StAddr { _streetNumber = One
                               (Single (Prefix "") (Number "1") (Suffix "")
                               )
                             , _streetName = StreetName "little collins"
                             , _streetType = StreetType "street"}
                      )
                  , _addrLocality = Locality { _suburb = Suburb "melbourne"
                                             , _state = State "vic"
                                             , _postcode = Postcode "3000"}
                  }

-- flatten functions

flattenStreetNumber :: StreetNumber
                    -> Record
                    '[ "sPrefix" :-> Prefix
                     , "sNumber" :-> Number
                     , "sSuffix" :-> Suffix
                     , "ePrefix" :-> Prefix
                     , "eNumber" :-> Number
                     , "eSuffix" :-> Suffix
                     ]
flattenStreetNumber x =
  case x of
    One (Single p n s) ->
      p &: n &: s &:
      Prefix mempty &: Number mempty &: Suffix mempty &: Nil
    Range (Single p n s) (Single p' n' s') ->
      p &: n &: s &: p' &: n' &: s' &: Nil

flattenStAddr :: StreetAddress
              -> Record
              '[ "sPrefix" :-> Prefix
               , "sNumber" :-> Number
               , "sSuffix" :-> Suffix
               , "ePrefix" :-> Prefix
               , "eNumber" :-> Number
               , "eSuffix" :-> Suffix
               , "streetName" :-> StreetName
               , "streetType" :-> StreetType
               ]
flattenStAddr (StAddr
              { _streetNumber = nb
              , _streetName = sn
              , _streetType = st
              }) =
  flattenStreetNumber nb <+> sn &: st &: Nil

flattenLocality :: Locality
                -> Record
                '[ "suburb" :-> Suburb
                 , "state" :-> State
                 , "postcode" :-> Postcode
                 ]
flattenLocality x = _suburb x &: _state x &: _postcode x &: Nil

-- Maybe for PO box and street addresses

auStAddrRec :: AuAddress
              -> Maybe (Record
              '[ "sPrefix" :-> Prefix
               , "sNumber" :-> Number
               , "sSuffix" :-> Suffix
               , "ePrefix" :-> Prefix
               , "eNumber" :-> Number
               , "eSuffix" :-> Suffix
               , "streetName" :-> StreetName
               , "streetType" :-> StreetType
               , "suburb" :-> Suburb
               , "state" :-> State
               , "postcode" :-> Postcode
               ])
auStAddrRec AuAddress
  { _addrLocation = x'
  , _addrLocality = y } =
  case x' of
    AStreetAddress x ->
      Just $ flattenStAddr x <+> flattenLocality y
    _ -> Nothing

auPoboxRec :: AuAddress
            -> Maybe (Record
                      '[ "boxtype" :-> Text
                       , "boxnumber" :-> Box
                       , "suburb" :-> Suburb
                       , "state" :-> State
                       , "postcode" :-> Postcode
                       ])
auPoboxRec AuAddress
  { _addrLocation = x
  , _addrLocality = y } =
  case x of
    APobox (Gpo boxnumber) ->
      Just $ "GPO" &: boxnumber &: flattenLocality y
    APobox (Po  boxnumber) ->
      Just $ "PO"  &: boxnumber &: flattenLocality y
    _              -> Nothing

-- to csv

streetCSV :: AuAddress -> [String]
streetCSV = runIdentity . P.toListM . produceCSV . auStAddrRec

poboxCSV :: AuAddress -> [String]
poboxCSV = runIdentity . P.toListM . produceCSV . auPoboxRec

