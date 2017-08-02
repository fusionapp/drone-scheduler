module Lib.Config (
  -- * Scheduler configuration
  Config(..)
  , BuildSchedule(..)
  ) where

import Control.Monad.Fail (MonadFail(fail))
import Data.Aeson.Types (FromJSON(..), (.:), withObject)
import Network.URI (URI)
import System.Cron (CronSchedule)
import System.Cron.Parser (parseCronSchedule)

import Lib.Prelude

-- | Scheduler configuration.
data Config = Config
  { configServer :: URI
  , configAuthToken :: Text
  , configSchedules :: FilePath
  } deriving (Show, Eq)

-- | A schedule for a particular build.
data BuildSchedule = BuildSchedule
  { scheduleUser :: Text
  , scheduleRepo :: Text
  , scheduleBranch :: Text
  , scheduleSchedule :: CronSchedule
  } deriving (Show, Eq)

instance FromJSON BuildSchedule where
  parseJSON = withObject "BuildSchedule" $
    \o -> BuildSchedule
         <$> o .: "user"
         <*> o .: "repo"
         <*> o .: "branch"
         <*> (either fail return . parseCronSchedule =<< o .: "schedule")
