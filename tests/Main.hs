-- -*- Mode: Haskell; -*-
--
-- Slug tests.
--
-- Copyright © 2015 Mark Karpov
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
import Data.Char (isAlphaNum)
import Data.Maybe (isJust)
import Data.Text (Text)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, path)
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
  [ testProperty "Slug of slug is the slug" prop_changeStops
  , testProperty "Alpha-numeric chars are necessary and sufficient"
    prop_needAlphaNum
  , testProperty "Path pieces accept correct slugs" prop_pathPiece
  , testCase "Removal of apostrophe" prop_apostropheRemoval
  , testCase "Path pieces ignore case of slugs" prop_pathPieceCase
  ]

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

prop_changeStops :: Text -> Property
prop_changeStops x = v (f x) === v (g x)
  where f = mkSlug
        g = mkSlug >=> mkSlug . unSlug
        v = either (Left . show) Right

prop_needAlphaNum :: Text -> Property
prop_needAlphaNum x = hasAlphaNum x ==> isJust (mkSlug x)
  where hasAlphaNum = isJust . T.find isAlphaNum

prop_pathPiece :: Text -> Property
prop_pathPiece x =
  case mkSlug x of
    Nothing -> property True
    Just slug -> fromPathPiece (unSlug slug) === Just slug

prop_apostropheRemoval :: Assertion
prop_apostropheRemoval = unSlug <$> mkSlug text @?= Just slug
  where text = "That's what I thought about doin' that stuff!"
        slug = "thats-what-i-thought-about-doin-that-stuff"

prop_pathPieceCase :: Assertion
prop_pathPieceCase = fromPathPiece text @?= mkSlug text
  where text = "thiS-Строка-hAs-коМбиНацию-of-DiffErent-Cases" :: Text
