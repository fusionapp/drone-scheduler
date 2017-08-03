module Network.Drone
  ( droneClient
  , DroneClient(..)
  , RepoClient(..)
  ) where

import           Servant.API
  ( (:>), (:<|>)(..), Capture, EmptyAPI, Get, Post, JSON, QueryParam
  , AuthProtect)
import           Servant.Client
  (ClientM, EmptyClient(..), AuthClientData, client, mkAuthenticateReq)
import           Servant.Common.Req (Req, addHeader)

import           Lib.Prelude
import           Network.Drone.Types (BuildInfo, BuildNumber(..))

type instance AuthClientData (AuthProtect "oauth2-bearer") = Text

type DroneAPI = AuthProtect "oauth2-bearer" :> "api" :>
  (UserAPI :<|> ReposAPI :<|> UsersAPI :<|> BuildQueueAPI)

type UserAPI = EmptyAPI

type ReposAPI = "repos" :> Capture "user" Text :> Capture "repo" Text
  :> BuildsAPI

type BuildsAPI = "builds" :> (
  Get '[JSON] [BuildInfo]
  :<|> Capture "id" BuildNumber :> QueryParam "branch" Text :> Get '[JSON] BuildInfo
  :<|> Capture "id" BuildNumber
    :> QueryParam "fork" Text
    :> QueryParam "event" Text
    :> QueryParam "deploy_to" Text
    :> Post '[JSON] BuildInfo
  )

type UsersAPI = EmptyAPI

type BuildQueueAPI = EmptyAPI

data DroneClient = DroneClient
  { droneRepo :: Text -> Text -> RepoClient
  }

data RepoClient = RepoClient
  { getBuilds :: ClientM [BuildInfo]
  , getBuild :: BuildNumber -> Maybe Text -> ClientM BuildInfo
  , startBuild :: BuildNumber -> Bool -> Maybe Text -> Maybe Text -> ClientM BuildInfo
  , restartBuild :: BuildNumber -> ClientM BuildInfo
  , forkBuild :: BuildNumber -> ClientM BuildInfo
  , deploy :: BuildNumber -> Text -> ClientM BuildInfo
  }

oauth2Bearer :: Text -> Req -> Req
oauth2Bearer token = addHeader "Authorization" ("Bearer " <> token)

droneClient :: Text -> DroneClient
droneClient token = DroneClient{..}
  where
    droneClient' = client (Proxy :: Proxy DroneAPI)
    auth = mkAuthenticateReq token oauth2Bearer
    EmptyClient :<|> repoClient :<|> EmptyClient :<|> EmptyClient = droneClient' auth
    droneRepo user repo = RepoClient{..}
      where
        getBuilds :<|> getBuild :<|> startBuild' = repoClient user repo
        startBuild bid fork = startBuild' bid (bool Nothing (Just "true") fork)
        restartBuild bid = startBuild bid False Nothing Nothing
        forkBuild bid = startBuild bid True Nothing Nothing
        deploy bid env = startBuild bid True (Just "deployment") (Just env)
