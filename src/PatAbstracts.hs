{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module PatAbstracts where

-- Parsing

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Data.Char
import qualified Data.Text             as T hiding (Text)
import           Data.Text.Encoding
import           Test.Hspec
import           Text.Parser.LookAhead
import           Text.RawString.QQ
import           Text.Trifecta

-- Frames
import           Control.Monad         (mzero)
import           Data.Readable         (Readable (fromText))
import           Data.Typeable
import qualified Data.Vector           as V
import           Frames
import           Frames.ColumnTypeable (Parseable)
import           Frames.InCore         (VectorFor)

-- column types

import           Addresses

instance Readable AddressLocation where
  fromText t =
    case
      parseByteString step mempty (
      (encodeUtf8 . T.toCaseFold) t) of
      Success x -> pure x
      Failure e -> mzero

instance Parseable AddressLocation where

-- parsing

addEx :: ByteString
addEx = [r|abstract (11 document no. au-a-10803/92 (19) australian patent office (54) title film cartridge bar code scanner and controller for a digital imaging system international patent classification(s) (51) 5 g03b007/24 g06k009/18 (21) application no. 10803/92 (22) application date 06.02.92 priority data (31) number (32) date (33) country 656605 19.02.91 us united states of america (43) publication date 27.08.92 (71) applicant(s) minnesota mining and manufacturing company (72) inventor(s) richard randall lemberger; terrence harold joyce (74) attorney or agent spruson ferguson, gpo box 3898, sydney nsw 2001 (57) claim 1. a laser imaging system, comprising: a cartridge of photographic film; a machine readable information bearing medium associated with the cartridge and including information characterizing the cartridge and/or film; a laser imager, including: a cartridge receiving mechanism; a laser scanning system including a laser for imaging the film; and a reading device for reading the information from the information bearing medium; and an image management system responsive to image input data and coupled to the laser imager, for controlling the laser imager as a function of the input data and the information read from the information bearing medium. i i|]

