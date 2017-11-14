{-# LANGUAGE DataKinds, DeriveDataTypeable, TypeFamilies, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PatAbstracts where

-- Parsing

import Control.Applicative
import Data.ByteString (ByteString)
import Text.RawString.QQ
import Text.Trifecta
import qualified Data.Text as T hiding (Text)
import Test.Hspec

-- Frames
import Control.Monad (mzero)
import Data.Readable (Readable(fromText))
import Data.Typeable
import Frames.ColumnTypeable (Parseable)
import qualified Data.Vector as V
import Frames.InCore (VectorFor)
import Frames

-- column types

data Addr = StatePost Text Int | NoAdress
  deriving (Eq, Ord, Show, Typeable)

type instance VectorFor Addr = V.Vector

instance Readable Addr where
  fromText t = undefined
  -- fromText t
  --   | T.length t == 5 = if all C.isDigit cs
  --                       then return $ StatePost s p
  --                       else return $ NoAddress
  --   | otherwise = mzero

instance Parseable Addr where

type MyColumns = Addr ': CommonColumns

-- parsing

addEx :: ByteString
addEx = [r|ABSTRACT (11 Document No. AU-A-10803/92 (19) AUSTRALIAN PATENT OFFICE (54) Title FILM CARTRIDGE BAR CODE SCANNER AND CONTROLLER FOR A DIGITAL IMAGING SYSTEM International Patent Classification(s) (51) 5 G03B007/24 G06K009/18 (21) Application No. 10803/92 (22) Application Date 06.02.92 Priority Data (31) Number (32) Date (33) Country 656605 19.02.91 US UNITED STATES OF AMERICA (43) Publication Date 27.08.92 (71) Applicant(s) MINNESOTA MINING AND MANUFACTURING COMPANY (72) Inventor(s) RICHARD RANDALL LEMBERGER; TERRENCE HAROLD JOYCE (74) Attorney or Agent SPRUSON FERGUSON, GPO Box 3898, SYDNEY NSW 2001 (57) Claim 1. A laser imaging system, comprising: a cartridge of photographic film; a machine readable information bearing medium associated with the cartridge and including information characterizing the cartridge and/or film; a laser imager, including: a cartridge receiving mechanism; a laser scanning system including a laser for imaging the film; and a reading device for reading the information from the information bearing medium; and an image management system responsive to image input data and coupled to the laser imager, for controlling the laser imager as a function of the input data and the information read from the information bearing medium. I I|]

type POBox = Int
type PostCode = Int
type City = ByteString
type StateTerritory = ByteString

data Address = POBoxAddress POBox City StateTerritory PostCode deriving (Eq, Ord, Show)

poBox :: Parser Text
poBox = do
  skipOptional (oneOf "Gg")
  _ <- spaces
  p <- oneOf "Pp"
  _ <- spaces
  o <- oneOf "Oo"
  _ <- spaces
  b <- oneOf "Bb"
  _ <- spaces
  o' <- oneOf "Oo"
  _ <- spaces
  x <- oneOf "Xx"
  return $ T.pack [p,o,b,o',x]

digits :: Parser Text
digits = spaces >> T.pack <$> some digit

skipUntil :: Parser Text -> Parser Text
skipUntil p = try p <|> (T.singleton <$> anyChar >> skipUntil p)

parsePobox :: Parser Text
parsePobox = do
  _ <- skipUntil poBox
  d <- digits
  return d

-- parseByteString (parsePOBox) mempty addEx
-- parseByteString (many $ notFollowedBy poBox >> parsePOBox >> many anyChar >> EOF) mempty addEx
