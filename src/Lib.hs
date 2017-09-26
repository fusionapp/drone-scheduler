{-|
Module      : Lib
Description : Lib's main module
-}
module Lib
    ( appMain
    ) where

import Control.Monad.Fail (MonadFail(fail))
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)
import Options.Applicative
  ( Parser, ParserInfo, ReadM, strOption, long, metavar, help, execParser, info
  , helper, fullDesc, progDesc, header, eitherReader, option)
import Servant.Client (ClientM, ClientEnv(ClientEnv), runClientM)
import Servant.Common.BaseUrl (BaseUrl(..), Scheme(..))
import System.Cron (Job(Job), Schedule, execSchedule)

import Lib.Config (App(..), Config(..), BuildSchedule(..))
import Lib.Prelude
import Network.Drone (DroneClient(..), RepoClient(..), droneClient)
import Network.Drone.Types (BuildNumber(..), BuildInfo(..))

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

addJob :: App -> BuildSchedule -> Schedule ()
addJob app schedule = modify (Job (scheduleSchedule schedule) job:)
  where job = triggerBuild app schedule

triggerBuild :: App -> BuildSchedule -> IO ()
triggerBuild app@App{appDroneClient=client} BuildSchedule{..} =
  void $ runDrone app $ do
    let RepoClient{..} = droneRepo client scheduleUser scheduleRepo
    BuildInfo{biNumber} <- getBuild BuildLatest (Just scheduleBranch)
    deploy (Build biNumber) scheduleEnvironment

baseUrlFromURI :: URI -> BaseUrl
baseUrlFromURI uri = BaseUrl
  { baseUrlScheme = scheme
  , baseUrlHost = uriRegName auth
  , baseUrlPort = port
  , baseUrlPath = uriPath uri
  }
  where scheme = case uriScheme uri of
          "http:" -> Http
          "https:" -> Https
          _ -> panic "Unsupported scheme"
        auth = unsafeFromJust (uriAuthority uri)
        port = case uriPort auth of
          "" -> case scheme of
            Http -> 80
            Https -> 443
          (':':p) -> unsafeFromJust (readMaybe p)
          _ -> panic "Invalid port"

runDrone :: App -> ClientM a -> IO a
runDrone app a = do
  res <- runClientM a (ClientEnv manager baseUrl)
  case res of
    Left err -> throwIO err
    Right r -> return r
  where manager = appManager app
        baseUrl = (baseUrlFromURI . configServer . appConfig) app
  
-- | Main function for the scheduler
--
appMain :: IO ()
appMain = do
  config <- execParser opts
  manager <- newTlsManager
  let app = App config manager (droneClient (configAuthToken config))
  schedules <- either (fail . prettyPrintParseException) return =<<
    decodeFileEither (configSchedules config)
  void $ execSchedule $ for_ (schedules :: [BuildSchedule]) (addJob app)
  forever $ threadDelay 10000000
