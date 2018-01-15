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

import           Frames                ((:->), pattern Nil, Text)
import           Frames.CSV            (produceCSV)
import           Frames.Rec
import           PatAbstracts          ()
import           Patents

flattenLocality :: Locality -> Record
  '[ "suburb" :-> Suburb
   , "state" :-> State
   , "postcode" :-> Postcode ]
flattenLocality x = _suburb x &: _state x &: _postcode x &: Nil

dummyAddressRec :: AuAddress
                -> Maybe (Record '[ "Box" :-> Text, "PostCode" :-> Text ])
dummyAddressRec x =
  case _addrLocation x of
    APobox (Gpo b) -> Just ("GPO" &: b &: Nil)
    APobox (Po b)  -> Just ("PO"  &: b &: Nil)
    _              -> Nothing

dAddressRec :: AuAddress
            -> Maybe (Record
                      '[ "boxtype" :-> Text
                       , "boxnumber" :-> Box
                       , "suburb" :-> Suburb
                       , "state" :-> State
                       , "postcode" :-> Postcode
                       ])
dAddressRec AuAddress
  { _addrLocation = x
  , _addrLocality = y } =
  case x of
    APobox (Gpo boxnumber) ->
      Just $ "GPO" &: boxnumber &: flattenLocality y
    APobox (Po  boxnumber) ->
      Just $ "PO"  &: boxnumber &: flattenLocality y
    -- AStreetAddress x -> undefined
    _              -> Nothing

dummyAddressCSV :: AuAddress -> [String]
dummyAddressCSV = runIdentity . P.toListM . produceCSV . dAddressRec

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
