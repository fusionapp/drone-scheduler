module Lib.Config (
  -- * Scheduler configuration
  App(..)
  , Config(..)
  , BuildSchedule(..)
  ) where

import Control.Monad.Fail (MonadFail(fail))
import Data.Aeson.Types (FromJSON(..), (.:), withObject)
import Network.HTTP.Client (Manager)
import Network.URI (URI)
import System.Cron (CronSchedule)
import System.Cron.Parser (parseCronSchedule)

import Lib.Prelude
import Network.Drone (DroneClient)

-- | Application type
data App = App
  { appConfig :: Config
  , appManager :: Manager
  , appDroneClient :: DroneClient
  }

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
  , scheduleEnvironment :: Text
  , scheduleSchedule :: CronSchedule
  } deriving (Show, Eq)

instance FromJSON BuildSchedule where
  parseJSON = withObject "BuildSchedule" $
    \o -> BuildSchedule
         <$> o .: "user"
         <*> o .: "repo"
         <*> o .: "branch"
         <*> o .: "environment"
         <*> (either fail return . parseCronSchedule =<< o .: "schedule")
