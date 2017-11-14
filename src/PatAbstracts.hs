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
import Text.Parser.LookAhead
import qualified Data.Text as T hiding (Text)
import Data.Text.Encoding
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

type instance VectorFor Address = V.Vector

instance Readable Address where
  fromText t =
    case
      parseByteString poboxAddress mempty (
      (encodeUtf8 . T.toCaseFold) t) of
      Success x -> pure x
      Failure _ -> pure NoAddress

instance Parseable Address where

type MyColumns = Address ': CommonColumns

-- parsing

addEx :: ByteString
addEx = [r|abstract (11 document no. au-a-10803/92 (19) australian patent office (54) title film cartridge bar code scanner and controller for a digital imaging system international patent classification(s) (51) 5 g03b007/24 g06k009/18 (21) application no. 10803/92 (22) application date 06.02.92 priority data (31) number (32) date (33) country 656605 19.02.91 us united states of america (43) publication date 27.08.92 (71) applicant(s) minnesota mining and manufacturing company (72) inventor(s) richard randall lemberger; terrence harold joyce (74) attorney or agent spruson ferguson, gpo box 3898, sydney nsw 2001 (57) claim 1. a laser imaging system, comprising: a cartridge of photographic film; a machine readable information bearing medium associated with the cartridge and including information characterizing the cartridge and/or film; a laser imager, including: a cartridge receiving mechanism; a laser scanning system including a laser for imaging the film; and a reading device for reading the information from the information bearing medium; and an image management system responsive to image input data and coupled to the laser imager, for controlling the laser imager as a function of the input data and the information read from the information bearing medium. i i|]

type POBox = Text
type PostCode = Text
type City = Text
type StateTerritory = Text

data Address =
  POBoxAddress POBox City StateTerritory PostCode
  | NoAddress
  deriving (Eq, Ord, Show)

spaceOrStop :: Parser String
spaceOrStop = many $ oneOf ". ,"

poBox :: Parser Text
poBox = do
  skipOptional $ char 'g'
  _ <- spaceOrStop
  p <- char 'p'
  _ <- spaceOrStop
  o <- char 'o'
  _ <- spaceOrStop
  b <- char 'b'
  _ <- spaceOrStop
  o' <- char 'o'
  _ <- spaceOrStop
  x <- char 'x'
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
      (try (lookAhead p) >> return xs) <|> do -- need to find a way not to consume p
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
  p <- digits -- p should be consumed here
  return $ POBoxAddress b a s p


main :: IO ()
main = hspec $ do

  describe "Test GPO BOX address parsing" $ do
    it "can ignore leading text" $ do
      let (Success x) =
            parseByteString (skipUntil poBox >> spaceOrStop >> digits) mempty addEx
      x `shouldBe` "3898"
  describe "Test city and state/territory parsing" $ do
    it "can extract the city and state from in between po box and postcode" $ do
      let (Success x) =
            parseByteString (skipUntil poBox
                             >> spaceOrStop
                             >> digits
                             >> spaceOrStop
                             >> takeUntil digits) mempty addEx
      x `shouldBe` "sydney nsw "
  describe "Test PO Box address parsing" $ do
    it "can extract a type corresponding to a full PO Box address" $ do
      let (Success x) =
            parseByteString poboxAddress mempty addEx
      x `shouldBe` POBoxAddress "3898" "sydney" "nsw" "2001"
            
