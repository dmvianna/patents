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

-- type instance VectorFor Address = V.Vector

instance Readable Address where
  fromText t =
    case
      parseByteString poboxAddress mempty (
      (encodeUtf8 . T.toCaseFold) t) of
      Success x -> pure x
      -- Failure e -> pure $ NoAddress e
      Failure e -> mzero

instance Parseable Address where

-- parsing

addEx :: Text
addEx = [r|abstract (11 document no. au-a-10803/92 (19) australian patent office (54) title film cartridge bar code scanner and controller for a digital imaging system international patent classification(s) (51) 5 g03b007/24 g06k009/18 (21) application no. 10803/92 (22) application date 06.02.92 priority data (31) number (32) date (33) country 656605 19.02.91 us united states of america (43) publication date 27.08.92 (71) applicant(s) minnesota mining and manufacturing company (72) inventor(s) richard randall lemberger; terrence harold joyce (74) attorney or agent spruson ferguson, gpo box 3898, sydney nsw 2001 (57) claim 1. a laser imaging system, comprising: a cartridge of photographic film; a machine readable information bearing medium associated with the cartridge and including information characterizing the cartridge and/or film; a laser imager, including: a cartridge receiving mechanism; a laser scanning system including a laser for imaging the film; and a reading device for reading the information from the information bearing medium; and an image management system responsive to image input data and coupled to the laser imager, for controlling the laser imager as a function of the input data and the information read from the information bearing medium. i i|]

type POBox = Text
type PostCode = Text
type City = Text
type StateTerritory = Text

data Address =
  POBoxAddress POBox City StateTerritory PostCode
  | NoAddress ErrInfo
  deriving Show

instance Eq Address where
  NoAddress _ == _ = False
  _ == NoAddress _ = False
  POBoxAddress a b c d == POBoxAddress a' b' c' d' =
    (a == a') && (b == b') && (c == c') && (d == d')


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
      (try (lookAhead p) >> return xs) <|> do
      c <- anyChar
      go p (T.snoc xs c)

poboxAddress :: Parser Address
poboxAddress = do
  _ <- skipUntil poBox
  _ <- spaceOrStop
  b <- digits
  _ <- optional (text "k")
  _ <- spaceOrStop
  a' <- takeUntil (spaceOrStop >> digits)
  let (a'', s') = T.breakOnEnd " " a'
      a = T.stripEnd a''
      s = T.stripStart s'
  _ <- spaceOrStop
  p <- digits
  return $ POBoxAddress b a s p


main :: IO ()
main = hspec $ do

  describe "Test GPO BOX address parsing" $ do
    it "can ignore leading text" $ do
      let (Success x) =
            parseByteString (skipUntil poBox >> spaceOrStop >> digits) mempty
            $ (encodeUtf8 . T.toCaseFold) addEx
      x `shouldBe` "3898"
  describe "Test city and state/territory parsing" $ do
    it "can extract the city and state from in between po box and postcode" $ do
      let (Success x) =
            parseByteString (skipUntil poBox
                             >> spaceOrStop
                             >> digits
                             >> spaceOrStop
                             >> takeUntil digits) mempty $
            (encodeUtf8 . T.toCaseFold) addEx
      x `shouldBe` "sydney nsw "
  describe "Test PO Box address parsing" $ do
    it "can extract a type corresponding to a full PO Box address" $ do
      let (Success x) =
            parseByteString poboxAddress mempty $
            (encodeUtf8 . T.toCaseFold) addEx
      x `shouldBe` POBoxAddress "3898" "sydney" "nsw" "2001"
    it "ignores the character K when appended to PO BOX number" $ do
      let (Success x) =
            parseByteString poboxAddress mempty $
            (encodeUtf8 . T.toCaseFold) "po box 1234k melbourne vic 3001"
      x `shouldBe` POBoxAddress "1234" "melbourne" "vic" "3001"
    it "parses kingston" $ do
      let (Success x) =
            parseByteString poboxAddress mempty $
            (encodeUtf8 . T.toCaseFold) "po box 1234 kingston vic 3001"
      x `shouldBe` POBoxAddress "1234" "kingston" "vic" "3001"


