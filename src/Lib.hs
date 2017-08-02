{-|
Module      : Lib
Description : Lib's main module
-}
module Lib
    ( appMain
    ) where

import Control.Monad.Fail (MonadFail(fail))
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
  ( Parser, ParserInfo, ReadM, strOption, long, metavar, help, execParser, info
  , helper, fullDesc, progDesc, header, eitherReader, option)
import System.Cron (Job(Job), Schedule, execSchedule)

import Lib.Config (Config(..), BuildSchedule(..))
import Lib.Prelude

url :: ReadM URI
url = eitherReader $ maybeToRight "invalid URL" . parseAbsoluteURI

configParser :: Parser Config
configParser = Config
  <$> option url
  ( long "server"
    <> metavar "URL"
    <> help "Drone API base URL"
  )
  <*> strOption
  ( long "token"
    <> metavar "TOKEN"
    <> help "Drone API token"
  )
  <*> strOption
  ( long "schedules"
    <> metavar "FILE"
    <> help "File with build schedules"
  )

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "Schedule builds for Drone"
  <> header "drone-scheduler - a scheduler for Drone builds"
  )

addJob :: Config -> BuildSchedule -> Schedule ()
addJob config schedule = modify (Job (scheduleSchedule schedule) job:)
  where job = triggerBuild config schedule

triggerBuild :: Config -> BuildSchedule -> IO ()
triggerBuild Config{..} BuildSchedule{..} = undefined
  -- withDrone configServer configAuthToken $
  -- \ctx -> print =<< request ctx (lastBuild (Repository scheduleUser scheduleRepo) scheduleBranch)

-- | Main function for the scheduler
--
appMain :: IO ()
appMain = do
  config <- execParser opts
  print config
  schedules <- either (fail . prettyPrintParseException) return =<<
    decodeFileEither (configSchedules config)
  print (schedules :: [BuildSchedule])
  --void $ execSchedule $ for_ schedules (addJob config)
  --forever $ threadDelay 10000000
  for_ schedules (triggerBuild config)
