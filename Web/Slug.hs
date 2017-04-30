-- |
-- Module      :  Web.Slug
-- Copyright   :  © 2015–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Type-safe slug implementation for Yesod ecosystem.

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}

module Web.Slug
  ( Slug
  , mkSlug
  , unSlug
  , parseSlug
  , truncateSlug
  , SlugException (..) )
where

import Control.Exception (Exception (..))
import Control.Monad ((>=>), liftM)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson.Types (ToJSON (..), FromJSON (..))
import Data.Char (isAlphaNum)
import Data.Data (Data)
import Data.Maybe (isJust, fromJust)
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import Database.Persist.Types (SqlType (..))
import Test.QuickCheck
import Web.HttpApiData
import Web.PathPieces
import qualified Data.Aeson as A
import qualified Data.Text  as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- | This exception is thrown by 'mkSlug' when its input cannot be converted
-- into a proper 'Slug'.

data SlugException
  = InvalidInput Text  -- ^ Slug cannot be generated for given text
  | InvalidSlug  Text  -- ^ Input is not a valid slug, see 'parseSlug'
  | InvalidLength Int  -- ^ Requested slug length is not a positive number
 deriving (Eq, Show, Typeable)

instance Exception SlugException where
#if MIN_VERSION_base(4,8,0)
  displayException (InvalidInput text) = "Cannot build slug for " ++ show text
  displayException (InvalidSlug  text) = "The text is not a valid slug " ++ show text
  displayException (InvalidLength n)   = "Invalid slug length: " ++ show n
#endif

-- | Slug. Textual value inside is always guaranteed to have the following
-- qualities:
--
--     * it's not empty;
--     * it consists only of alpha-numeric groups of characters (words)
--     separated by @\'-\'@ dashes in such a way that entire slug cannot
--     start or end in a dash and also two dashes in a row cannot be found;
--     * every character with defined notion of case is lower-cased.
--
-- Slugs are good for semantic URLs and also can be used as identifier of a
-- sort in some cases.

newtype Slug = Slug Text deriving (Eq, Ord, Data, Typeable)

instance Semigroup Slug where
  x <> y = Slug (unSlug x <> "-" <> unSlug y)

-- | Create a 'Slug' from a 'Text' value, all necessary transformations are
-- applied. Argument of this function can be title of an article or
-- something like that.
--
-- Note that the result is inside 'MonadThrow', that means you can just get
-- it in 'Maybe', in more complex contexts it will throw 'SlugException'
-- exception using 'InvalidInput' constructor.
--
-- This function also has a useful property:
--
-- > mkSlug = mkSlug >=> mkSlug . unSlug

mkSlug :: MonadThrow m => Text -> m Slug
mkSlug text =
  let ws = getSlugWords text
  in if null ws
     then throwM (InvalidInput text)
     else return . Slug . T.intercalate "-" $ ws

-- | Get textual representation of a 'Slug'.

unSlug :: Slug -> Text
unSlug (Slug x) = x

-- | Convert 'Text' to a possibly empty collection of words. Every word is
-- guaranteed to be non-empty alpha-numeric lower-cased sequence of
-- characters.

getSlugWords :: Text -> [Text]
getSlugWords = T.words . T.toLower . T.map f . T.replace "'" ""
  where f x = if isAlphaNum x then x else ' '

-- | Convert a 'Text' into a 'Slug' only when it is already valid slug.
--
-- This function can throw the 'SlugException' exception using 'InvalidSlug'
-- constructor.

parseSlug :: MonadThrow m => Text -> m Slug
parseSlug v = mkSlug v >>= check
  where check s =
          if unSlug s == v
          then return s
          else throwM (InvalidSlug v)

-- | Ensure that given 'Slug' is not longer than given maximum number of
-- characters. If truncated slug ends in a dash, remove that dash too. (Dash
-- at the end would violate properties described in documentation for
-- 'Slug'.)
--
-- If the first argument is not a positive number, 'SlugException' is thrown
-- using 'InvalidLength' constructor.

truncateSlug :: MonadThrow m
  => Int               -- ^ Maximum length of slug, must be greater than 0
  -> Slug              -- ^ Original non-truncated slug
  -> m Slug            -- ^ Truncated slug
truncateSlug n v
  | n < 1     = throwM (InvalidLength n)
  | otherwise = mkSlug . T.take n . unSlug $ v

instance Show Slug where
  show = show . unSlug

instance Read Slug where
  readsPrec n = (readsPrec n :: ReadS Text) >=> f
    where f (s, t) = (,t) `liftM` parseSlug s

instance ToJSON Slug where
  toJSON = toJSON . unSlug

instance FromJSON Slug where
  parseJSON = A.withText "Slug" $ \txt ->
    case parseSlug txt of
      Left err -> fail (show err)
      Right val -> return val

instance PersistField Slug where
  toPersistValue   = toPersistValue . unSlug
  fromPersistValue =
    fromPersistValue >=> either (Left . T.pack . f) Right . parseSlug
    where
#if MIN_VERSION_base(4,8,0)
      f = displayException
#else
      f = show
#endif

instance PersistFieldSql Slug where
  sqlType = const SqlString

instance PathPiece Slug where
  fromPathPiece = parseSlug
  toPathPiece   = unSlug

instance ToHttpApiData Slug where
  toUrlPiece = unSlug

instance FromHttpApiData Slug where
  parseUrlPiece = either (Left . T.pack . f) Right . parseSlug
    where
#if MIN_VERSION_base(4,8,0)
      f = displayException
#else
      f = show
#endif

instance Arbitrary Slug where
  arbitrary = fromJust <$> (mkSlug . T.pack <$> arbitrary) `suchThat` isJust
