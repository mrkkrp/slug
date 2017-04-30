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
import Data.Char (isAlphaNum, isUpper)
import Data.Function (on)
import Data.Maybe (isJust, isNothing)
import Data.Semigroup
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck
import Web.HttpApiData
import Web.PathPieces
import Web.Slug
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "slug properties" $ do
    it "cannot be empty" $
      property $ \slug ->
        unSlug slug `shouldNotSatisfy` T.null
    it "contains only dashes and alpha-numeric characters" $
      property $ \slug ->
        let f x = isAlphaNum x || x == '-'
        in unSlug slug `shouldSatisfy` T.all f
    it "does not begin with a dash" $
      property $ \slug ->
        T.head (unSlug slug) `shouldNotBe` '-'
    it "does not end with a dash" $
      property $ \slug ->
        T.last (unSlug slug) `shouldNotBe` '-'
    it "does not contain empty words between dashes" $
      property $ \slug ->
        T.splitOn "-" (unSlug slug) `shouldNotSatisfy` any T.null
    it "no upper-cased chars found in slugs" $
      property $ \slug ->
        unSlug slug `shouldNotSatisfy` T.any isUpper
    it "showed Slug looks the same as its inner Text" $
      property $ \slug ->
        show slug === show (unSlug slug)
    it "showed Slug can be read back again" $
      property $ \slug ->
        read (show slug) === (slug :: Slug)
    it "incorrect Slug won't be read successfully" $
      property $ \x -> isNothing (parseSlug x) ==>
        (reads (show x) :: [(Slug, String)]) === []
    it "valid Slug text is a valid path piece" $
      property $ \slug ->
        fromPathPiece (unSlug slug) === Just slug
    it "valid Slug text is a valid HTTP API data" $
      property $ \slug ->
        parseUrlPiece (toUrlPiece slug) === Right (slug :: Slug)
  describe "Semigroup instance of Slug" $
    it "the (<>) operation produces valid slugs in all cases" $
      property $ \x y -> do
        let slug = unSlug (x <> y)
        slug' <- unSlug <$> parseSlug slug
        slug' `shouldBe` slug
  describe "mkSlug" $ do
    it "Slug transformation in idempotent" $
      property $ \x ->
        let f = mkSlug
            g = mkSlug >=> mkSlug . unSlug
        in f x ==== g x
    it "text containing at least one alpha-num char is Sluggable" $ do
      let hasAlphaNum = isJust . T.find isAlphaNum
      property $ \x -> hasAlphaNum x ==>
        isJust (mkSlug x) `shouldBe` True
  describe "parseSlug" $ do
    it "succeeds on valid input" $
      property $ \slug ->
        parseSlug (unSlug slug) `shouldReturn` slug
    it "fails on invalid input" $
      property $ \x ->
        (unSlug <$> mkSlug x) `notElem` [Nothing, Just x] ==>
          parseSlug x `shouldThrow` (== InvalidSlug x)
  describe "truncateSlug" $ do
    context "when required length is less than 0" $
      it "throws InvalidLength" $
        property $ \n slug -> (n < 1) ==>
          truncateSlug n slug `shouldThrow` (== InvalidLength n)
    context "when required length is OK" $
      it "truncates to this length or one less" $
        property $ \n slug -> (n > 0) ==> do
          t <- truncateSlug n slug
          T.length (unSlug t) `shouldSatisfy` (<= n)

----------------------------------------------------------------------------
-- Helpers

infix 4 ====

(====) :: (Show a, Show b, Eq b) => Either a b -> Either a b -> Property
(====) = (===) `on` displayLeft

displayLeft :: Show a => Either a b -> Either String b
displayLeft = either (Left . show) Right

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
