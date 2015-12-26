-- |
-- Module      :  Web.Slug
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Type-safe slug implementation for Yesod ecosystem.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}

module Web.Slug
  ( Slug
  , mkSlug
  , unSlug
  , parseSlug
  , SlugException (..) )
where

import Control.Exception (Exception)
import Control.Monad (mzero, (>=>))
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson.Types (ToJSON (..), FromJSON (..))
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import Database.Persist.Types (SqlType (..))
import Web.PathPieces
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

-- | This exception is thrown by 'mkSlug' when its input cannot be converted
-- into proper 'Slug'.

data SlugException
  = InvalidInput Text  -- ^ Slug cannot be generated for given text
  | InvalidSlug  Text  -- ^ Input is not a valid slug, see 'parseSlug'
  deriving (Typeable)

instance Show SlugException where
  show (InvalidInput text) = "Cannot build slug for " ++ show text
  show (InvalidSlug  text) = "The text is not a valid slug " ++ show text

instance Exception SlugException

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

newtype Slug = Slug
  { unSlug :: Text     -- ^ Get textual representation of 'Slug'.
  } deriving (Eq, Typeable)

-- | Create 'Slug' from 'Text', all necessary transformations are
-- applied. Argument of this function can be title of an article or
-- something like that.
--
-- Note that result is inside 'MonadThrow', that means you can just get it
-- in 'Maybe', in more complex contexts it will throw 'SlugException'.
--
-- This function also have a useful property:
--
-- > mkSlug = mkSlug >=> mkSlug . unSlug

mkSlug :: MonadThrow m => Text -> m Slug
mkSlug text =
  let ws = getSlugWords text
  in if null ws
     then throwM (InvalidInput text)
     else return . Slug . T.intercalate "-" $ ws

-- | Convert 'Text' to possibly empty collection of words. Every word is
-- guaranteed to be non-empty alpha-numeric lowercased sequence of
-- characters.

getSlugWords :: Text -> [Text]
getSlugWords = T.words . T.toLower . T.map f . T.replace "'" ""
  where f x = if isAlphaNum x then x else ' '

-- | Convert 'Text' into 'Slug' only when it is already valid slug. This
-- function is case-insensitive, which means that @\"Something\"@ is valid
-- input producing @\"something\"@ slug, while @\"Something?\"@ is invalid
-- input.
--
-- This function can throw 'SlugException' exception using 'InvalidSlug'
-- constructor.

parseSlug :: MonadThrow m => Text -> m Slug
parseSlug v = mkSlug v >>= check
  where check s =
          if unSlug s == T.toLower v
          then return s
          else throwM (InvalidSlug v)

instance Show Slug where
  show = show . unSlug

instance Read Slug where
  readsPrec n = (readsPrec n :: ReadS Text) >=> f
    where f (s, t) = (,t) <$> parseSlug s

instance ToJSON Slug where
  toJSON = toJSON . unSlug

instance FromJSON Slug where
  parseJSON (A.String v) = maybe mzero return (parseSlug v)
  parseJSON _            = mzero

instance PersistField Slug where
  toPersistValue   = toPersistValue . unSlug
  fromPersistValue =
    fromPersistValue >=> either (Left . T.pack . show) Right . parseSlug

instance PersistFieldSql Slug where
  sqlType = const SqlString

instance PathPiece Slug where
  fromPathPiece = parseSlug
  toPathPiece   = unSlug
