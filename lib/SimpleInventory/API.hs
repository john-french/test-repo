{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleInstances, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveTraversable, FlexibleContexts, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=304 #-}
module SimpleInventory.API (
  -- * Client and Server
  ServerConfig(..),
  SimpleInventoryBackend,
  createSimpleInventoryClient,
  runSimpleInventoryServer,
  runSimpleInventoryClient,
  runSimpleInventoryClientWithManager,
  SimpleInventoryClient,
  -- ** Servant
  SimpleInventoryAPI,
  ) where

import SimpleInventory.Types

import Data.Aeson (Value)
import Data.Coerce (coerce)
import Servant.API
import Servant (serve, ServantErr)
import Web.HttpApiData
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import Data.Text (Text)
import Servant.Common.BaseUrl(BaseUrl(..))
import Servant.Client (ServantError, client, Scheme(Http))
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class
import Data.Function ((&))
import GHC.Exts (IsString(..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import Servant.API.Verbs (Verb, StdMethod(..))
import Control.Monad.Except (ExceptT)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types.Method (methodOptions)

instance ReflectMethod 'OPTIONS where
  reflectMethod _ = methodOptions




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either Text b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> key <> " in form data"
    Just value -> parseQueryParam value

-- | Servant type-level API, generated from the Swagger spec for SimpleInventory.
type SimpleInventoryAPI
    =    "inventory" :> ReqBody '[JSON] InventoryItem :> Verb 'POST 200 '[JSON] () -- 'addInventory' route
    :<|> "increment" :> Verb 'GET 200 '[JSON] () -- 'incrementGet' route
    :<|> "inventory" :> QueryParam "searchString" Text :> QueryParam "skip" Int :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] [InventoryItem] -- 'searchInventory' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig {
    configHost :: String,  -- ^ Hostname to serve on, e.g. "127.0.0.1"
    configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList { fromQueryList :: [a] }
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat = CommaSeparated -- ^ CSV format for multiple parameters.
                      | SpaceSeparated -- ^ Also called "SSV"
                      | TabSeparated -- ^ Also called "TSV"
                      | PipeSeparated -- ^ `value1|value2|value2`
                      | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
    parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
    parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
    parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
    parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
    parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
    toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
    toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
    toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
    toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
    toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for SimpleInventory.
-- The backend can be used both for the client and the server. The client generated from the SimpleInventory Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createSimpleInventoryClient@). Alternatively, provided
-- a backend, the API can be served using @runSimpleInventoryServer@.
data SimpleInventoryBackend m = SimpleInventoryBackend {
    addInventory :: InventoryItem -> m (){- ^ Adds an item to the system -},
    incrementGet :: m (){- ^  -},
    searchInventory :: Maybe Text -> Maybe Int -> Maybe Int -> m [InventoryItem]{- ^ By passing in the appropriate options, you can search for available inventory in the system  -}
  }

newtype SimpleInventoryClient a = SimpleInventoryClient { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a }
    deriving Functor

instance Applicative SimpleInventoryClient where
    pure x = SimpleInventoryClient (\_ _ -> pure x)
    (SimpleInventoryClient f) <*> (SimpleInventoryClient x) = SimpleInventoryClient (\manager url -> f manager url <*> x manager url)

instance Monad SimpleInventoryClient where
    (SimpleInventoryClient a) >>= f = SimpleInventoryClient (\manager url -> do
        value <- a manager url
        runClient (f value) manager url)

instance MonadIO SimpleInventoryClient where
    liftIO io = SimpleInventoryClient (\_ _ -> liftIO io)

createSimpleInventoryClient :: SimpleInventoryBackend SimpleInventoryClient
createSimpleInventoryClient = SimpleInventoryBackend{..}
  where
    ((coerce -> addInventory) :<|>
     (coerce -> incrementGet) :<|>
     (coerce -> searchInventory)) = client (Proxy :: Proxy SimpleInventoryAPI)

-- | Run requests in the SimpleInventoryClient monad.
runSimpleInventoryClient :: ServerConfig -> SimpleInventoryClient a -> ExceptT ServantError IO a
runSimpleInventoryClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runSimpleInventoryClientWithManager manager clientConfig cl

-- | Run requests in the SimpleInventoryClient monad using a custom manager.
runSimpleInventoryClientWithManager :: Manager -> ServerConfig -> SimpleInventoryClient a -> ExceptT ServantError IO a
runSimpleInventoryClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the SimpleInventory server at the provided host and port.
runSimpleInventoryServer :: MonadIO m => ServerConfig -> SimpleInventoryBackend (ExceptT ServantErr IO)  -> m ()
runSimpleInventoryServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy SimpleInventoryAPI) (serverFromBackend backend)

  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend SimpleInventoryBackend{..} =
      (coerce addInventory :<|>
       coerce incrementGet :<|>
       coerce searchInventory)
