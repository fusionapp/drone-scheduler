module Network.Drone
  ( droneClient
  ) where

import           Network.URI
  (URI, uriToString, parseRelativeReference, relativeTo)
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S
import           Servant.API
  ((:>), (:<|>)(..), Capture, EmptyAPI, Get, JSON, QueryParam, AuthProtect)
import           Servant.Client
  (ClientM, EmptyClient(..), AuthClientData, client, mkAuthenticateReq)
import           Servant.Common.Req (Req, addHeader)

import           Lib.Prelude
import           Network.Drone.Types (BuildInfo, BuildID(..))

type instance AuthClientData (AuthProtect "oauth2-bearer") = Text

type DroneAPI = AuthProtect "oauth2-bearer" :> "api" :>
  (UserAPI :<|> ReposAPI :<|> UsersAPI :<|> BuildQueueAPI)

type UserAPI = EmptyAPI

type ReposAPI = "repos" :> Capture "user" Text :> Capture "repo" Text
  :> BuildsAPI
  
type BuildsAPI = "builds" :> (
  Get '[JSON] [BuildInfo]
  :<|> Capture "id" BuildID :> QueryParam "branch" Text :> Get '[JSON] BuildInfo
  )

type UsersAPI = EmptyAPI

type BuildQueueAPI = EmptyAPI

data DroneClient = DroneClient
  { droneRepo :: Text -> Text -> RepoClient
  }

data RepoClient = RepoClient
  { getBuilds :: ClientM [BuildInfo]
  , getBuild :: BuildID -> Maybe Text -> ClientM BuildInfo
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
      where getBuilds :<|> getBuild = repoClient user repo
    

--(EmptyClient :<|> (getBuilds :<|> getBuild) :<|> (EmptyClient :<|> EmptyClient)) = client api

{--
class DroneRequest a where
  type Response a
  request :: FromJSON (Response a) => DroneCtx -> a -> IO (Response a)

data DroneCtx = DroneCtx S.Session URI W.Options

withDrone :: URI -> ByteString -> (DroneCtx -> IO a) -> IO a
withDrone server token f = S.withSession $ \sess ->
  f (DroneCtx sess server opts)
  where opts = W.defaults & W.auth ?~ W.oauth2Bearer token

getR :: FromJSON a => S.Session -> URI -> W.Options -> IO a
getR sess url opts = do
  r <- W.asJSON =<< S.getWith opts sess url'
  return (r ^. W.responseBody)
  where url' = uriToString identity url ""

data Repository = Repository
  { repoUser :: Text
  , repoName :: Text
  } deriving (Show, Eq)

data Build = Build
  { buildRepo :: Repository
  , buildBranch :: Maybe Text
  , buildID :: Maybe Integer
  } deriving (Show, Eq)


instance DroneRequest Build where
  type Response Build = BuildInfo
  request (DroneCtx sess server opts) Build{..} = getR sess url opts'
    where
      opts' = opts & W.param "branch" .~ maybeToList buildBranch
      url = (`relativeTo` server) . unsafeFromJust . parseRelativeReference . toS $ mconcat
        [ "api/repos/"
        , repoUser buildRepo
        , "/"
        , repoName buildRepo
        , "/builds/"
        , bid
        ]
      bid = maybe "latest" show buildID

lastBuild :: Repository -> Text -> Build
lastBuild repo branch = Build repo (Just branch) Nothing
--}
