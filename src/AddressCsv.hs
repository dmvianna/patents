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

dummyAddressRec :: AuAddress
                -> Maybe (Record '[ "Box" :-> Text, "PostCode" :-> Text ])
dummyAddressRec x =
  case _addrLocation x of
    APobox (Gpo b) -> Just ("GPO" &: b &: Nil)
    APobox (Po b)  -> Just ("PO" &: b &: Nil)
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
      Just $ "GPO"
             &: boxnumber &: f y
    APobox (Po  boxnumber) ->
      Just $ "PO"
             &: boxnumber &: f y
    _              ->
      Nothing
  where
    f y' = _suburb y' &: _state y' &: _postcode y' &: Nil

dummyAddressCSV :: AuAddress -> [String]
dummyAddressCSV = runIdentity . P.toListM . produceCSV . dAddressRec

testAddress :: AuAddress
testAddress = AuAddress { _addrLocation = APobox (Gpo "1285")
                        , _addrLocality = Locality { _suburb = Suburb "melbourne"
                                                   , _state = State "vic"
                                                   , _postcode = Postcode "3001"}
                        }

