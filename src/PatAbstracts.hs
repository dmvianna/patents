{-# LANGUAGE DataKinds, DeriveDataTypeable, TypeFamilies, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PatAbstracts where

-- Parsing

import Control.Applicative
import Data.Char
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
addEx = [r|ABSTRACT (11 DOCUMENT NO. AU-A-10803/92 (19) AUSTRALIAN PATENT OFFICE (54) TITLE FILM CARTRIDGE BAR CODE SCANNER AND CONTROLLER FOR A DIGITAL IMAGING SYSTEM INTERNATIONAL PATENT CLASSIFICATION(S) (51) 5 G03B007/24 G06K009/18 (21) APPLICATION NO. 10803/92 (22) APPLICATION DATE 06.02.92 PRIORITY DATA (31) NUMBER (32) DATE (33) COUNTRY 656605 19.02.91 US UNITED STATES OF AMERICA (43) PUBLICATION DATE 27.08.92 (71) APPLICANT(S) MINNESOTA MINING AND MANUFACTURING COMPANY (72) INVENTOR(S) RICHARD RANDALL LEMBERGER; TERRENCE HAROLD JOYCE (74) ATTORNEY OR AGENT SPRUSON FERGUSON, GPO BOX 3898, SYDNEY NSW 2001 (57) CLAIM 1. A LASER IMAGING SYSTEM, COMPRISING: A CARTRIDGE OF PHOTOGRAPHIC FILM; A MACHINE READABLE INFORMATION BEARING MEDIUM ASSOCIATED WITH THE CARTRIDGE AND INCLUDING INFORMATION CHARACTERIZING THE CARTRIDGE AND/OR FILM; A LASER IMAGER, INCLUDING: A CARTRIDGE RECEIVING MECHANISM; A LASER SCANNING SYSTEM INCLUDING A LASER FOR IMAGING THE FILM; AND A READING DEVICE FOR READING THE INFORMATION FROM THE INFORMATION BEARING MEDIUM; AND AN IMAGE MANAGEMENT SYSTEM RESPONSIVE TO IMAGE INPUT DATA AND COUPLED TO THE LASER IMAGER, FOR CONTROLLING THE LASER IMAGER AS A FUNCTION OF THE INPUT DATA AND THE INFORMATION READ FROM THE INFORMATION BEARING MEDIUM. I I|]

type POBox = Text
type PostCode = Text
type City = Text
type StateTerritory = Text

data Address = POBoxAddress POBox City StateTerritory PostCode deriving (Eq, Ord, Show)

spaceOrStop :: Parser String
spaceOrStop = many $ oneOf ". ,"

poBox :: Parser Text
poBox = do
  skipOptional $ char 'G'
  _ <- spaceOrStop
  p <- char 'P'
  _ <- spaceOrStop
  o <- char 'O'
  _ <- spaceOrStop
  b <- char 'B'
  _ <- spaceOrStop
  o' <- char 'O'
  _ <- spaceOrStop
  x <- char 'X'
  return $ T.pack [p,o,b,o',x]

digits :: Parser Text
digits = T.pack <$> some digit

skipUntil :: Parser Text -> Parser Text
skipUntil p = try p <|> (T.singleton <$> anyChar >> skipUntil p)

takeUntil :: Parser Text -> Parser Text
takeUntil p' =
  go p' T.empty
  where
    go p xs =
      (try p >> return xs) <|> do -- need to find a way not to consume p
      c <- anyChar
      go p (T.snoc xs c)
  
poboxAddress :: Parser Address
poboxAddress = do
  _ <- skipUntil poBox
  _ <- spaceOrStop
  b <- digits
  _ <- spaceOrStop
  a' <- takeUntil (spaceOrStop >> digits)
  let (a'', s') = T.breakOnEnd " " a'
      a = T.stripEnd a''
      s = T.stripStart s'
  _ <- spaceOrStop
  -- p <- digits -- p should be consumed here
  return $ POBoxAddress b a s s -- don't forget to include p here


main :: IO ()
main = hspec $ do

  describe "Test address parsing" $ do
    it "can ignore leading text" $ do
      let (Success x) =
            parseByteString (skipUntil poBox >> digits) mempty addEx
      x `shouldBe` "3898"
  describe "Test city and state/territory parsing" $ do
    it "can extract the city and state from in between po box and postcode" $ do
      let (Success x) =
            parseByteString (skipUntil poBox
                             >> digits
                             >> spaceOrStop
                             >> takeUntil digits) mempty addEx
      x `shouldBe` "SYDNEY NSW"
            
