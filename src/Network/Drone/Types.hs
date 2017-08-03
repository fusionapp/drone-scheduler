module Network.Drone.Types
  ( BuildInfo(..)
  , BuildNumber(..)
  ) where

import Data.Aeson (FromJSON(..), genericParseJSON, defaultOptions)
import Data.Aeson.Types (Options(..), camelTo2)
import Data.String (String)
import Data.Time.Clock.POSIX (POSIXTime)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

import Lib.Prelude

-- | Converts @prefixedCamelCase@ to @camel-case@.
modifier :: Char -> String -> String
modifier sep = drop 1 . dropWhile (/= sep) . camelTo2 sep

data BuildInfo = BuildInfo
  { biID :: Integer
  , biNumber :: Integer
  , biParent :: Integer
  , biEvent :: Text
  , biStatus :: Text
  , biError :: Text
  , biEnqueuedAt :: POSIXTime
  , biCreatedAt :: POSIXTime
  , biStartedAt :: POSIXTime
  , biFinishedAt :: POSIXTime
  , biDeployTo :: Text
  , biCommit :: Text
  , biBranch :: Text
  , biRef :: Text
  , biRefspec :: Text
  , biRemote :: Text
  , biTitle :: Text
  , biMessage :: Text
  , biTimestamp :: POSIXTime
  , biSender :: Text
  , biAuthor :: Text
  , biAuthorAvatar :: Text
  , biAuthorEmail :: Text
  , biLinkUrl :: Text
  , biReviewedBy :: Text
  , biReviewedAt :: POSIXTime
  --, biProcs :: [Procs]
  } deriving (Show, Eq, Generic)

instance FromJSON BuildInfo where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = modifier '_' }

data BuildNumber = BuildLatest | Build Integer
  deriving (Show, Eq)

instance FromHttpApiData BuildNumber where
  parseUrlPiece "latest" = Right BuildLatest
  parseUrlPiece id = Build <$> parseUrlPiece id

instance ToHttpApiData BuildNumber where
  toUrlPiece BuildLatest = "latest"
  toUrlPiece (Build id) = toUrlPiece id
