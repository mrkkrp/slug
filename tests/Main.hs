--
-- Slug tests.
--
-- Copyright © 2015–2017 Mark Karpov
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow (..))
import Data.Char (isAlphaNum, isUpper)
import Data.Function (on)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Web.PathPieces
import Web.Slug
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

main :: IO ()
main = defaultMain [tests]

tests :: Test
tests = testGroup "Slug properties"
  [ -- Properties of valid slugs
    testProperty "Slug cannot be empty" prop_nonEmpty
  , testProperty "Only dashes and alpha-numeric" prop_validContent
  , testProperty "Slug cannot begin with a dash" prop_noBegDash
  , testProperty "Slug cannot end with a dash" prop_noEndDash
  , testProperty "Non-empty word between dashes" prop_noEmptyWords
  , testProperty "No upper-cased chars in slugs" prop_noUpperCase
    -- Properties of helper-functions
  , testProperty "Slug of slug is the slug" prop_changeStops
  , testProperty "Parsing of slugs (success)" prop_parsing0
  , testProperty "Parsing of slugs (failure)" prop_parsing1
  , testProperty "Slug truncation" prop_truncation
  , testProperty "Reading of slugs (success)" prop_read0
  , testProperty "Reading of slugs (failure)" prop_read1
    -- Additional properties
  , testProperty "Rendering of slugs" prop_showSlug
  , testProperty "Alpha-numerics are necessary and sufficient" prop_needAlphaNum
  , testProperty "Path pieces accept correct slugs" prop_pathPiece
  ]

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Slug where
  arbitrary = fromJust <$> ((mkSlug <$> arbitrary) `suchThat` isJust)

----------------------------------------------------------------------------
-- Properties of valid slugs

prop_nonEmpty :: Slug -> Property
prop_nonEmpty = expectFailure . T.null . unSlug

prop_validContent :: Slug -> Property
prop_validContent = property . T.all f . unSlug
  where f x = isAlphaNum x || x == '-'

prop_noBegDash :: Slug -> Property
prop_noBegDash s = expectFailure $ T.head (unSlug s) === '-'

prop_noEndDash :: Slug -> Property
prop_noEndDash s = expectFailure $ T.last (unSlug s) === '-'

prop_noEmptyWords :: Slug -> Property
prop_noEmptyWords = expectFailure . any T.null . T.splitOn "-" . unSlug

prop_noUpperCase :: Slug -> Property
prop_noUpperCase = expectFailure . T.any isUpper . unSlug

----------------------------------------------------------------------------
-- Properties of helper-functions

infix 4 ====

(====) :: (Show a, Show b, Eq b) => Either a b -> Either a b -> Property
(====) = (===) `on` displayLeft

displayLeft :: Show a => Either a b -> Either String b
displayLeft = either (Left . show) Right

prop_changeStops :: Text -> Property
prop_changeStops x = f x ==== g x
  where f = mkSlug
        g = mkSlug >=> mkSlug . unSlug

prop_parsing0 :: Slug -> Property
prop_parsing0 s = parseSlug (unSlug s) === Just s

prop_parsing1 :: Text -> Property
prop_parsing1 x = (unSlug <$> mkSlug x) `notElem` [Nothing, Just x]
  ==> parseSlug x ==== throwM (InvalidSlug x)

prop_truncation :: Int -> Slug -> Property
prop_truncation n s =
  case truncateSlug n s of
    Left e -> n < 1 ==> show e === show (InvalidLength n)
    Right t -> n > 0 ==> T.length (unSlug t) <= n

prop_read0 :: Slug -> Property
prop_read0 s = read (show s) === s

prop_read1 :: Text -> Property
prop_read1 s = isNothing (parseSlug s)
  ==> (reads (show s) :: [(Slug, String)]) === []

----------------------------------------------------------------------------
-- Additional properties

prop_showSlug :: Slug -> Property
prop_showSlug s = show s === show (unSlug s)

prop_needAlphaNum :: Text -> Property
prop_needAlphaNum x = hasAlphaNum x ==> isJust (mkSlug x)
  where hasAlphaNum = isJust . T.find isAlphaNum

prop_pathPiece :: Text -> Property
prop_pathPiece x =
  case mkSlug x of
    Nothing -> property True
    Just slug -> fromPathPiece (unSlug slug) === Just slug
