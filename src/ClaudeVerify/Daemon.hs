{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Daemon
-- Description : HTTP daemon for browser extension and editor integration
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module implements a local HTTP daemon that provides:
--
-- * Health check endpoint
-- * Code verification endpoint
-- * Feedback submission endpoint
-- * Known issues query endpoint
-- * Echomesh sync endpoint
--
-- The daemon is designed to be lightweight and run in the background,
-- providing verification services to browser extensions and editors.

module ClaudeVerify.Daemon
    ( -- * Configuration
      DaemonConfig(..)
    , defaultDaemonConfig

      -- * Running
    , runDaemon
    , runDaemonBackground

      -- * Endpoints
    , ApiEndpoint(..)
    , apiEndpoints
    ) where

import ClaudeVerify.Internal.Types
import ClaudeVerify.EchidnaClient
import ClaudeVerify.Feedback
import ClaudeVerify.Integration.Echomesh
import ClaudeVerify.Integration.EchidnaProtocol

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, try, SomeException)
import Control.Monad (forever, when)
import Data.Aeson (FromJSON, ToJSON, encode, decode, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

-- | Daemon configuration
data DaemonConfig = DaemonConfig
    { dcPort          :: Int
    , dcHost          :: String
    , dcVerbose       :: Bool
    , dcEchidnaPath   :: Maybe FilePath
    , dcEchidnaUrl    :: Maybe Text
    , dcFeedbackDb    :: Maybe FilePath
    , dcEchomeshPath  :: Maybe FilePath
    , dcAllowedOrigins :: [Text]
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Default configuration
defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig
    { dcPort = 9847
    , dcHost = "127.0.0.1"
    , dcVerbose = False
    , dcEchidnaPath = Nothing
    , dcEchidnaUrl = Nothing
    , dcFeedbackDb = Nothing
    , dcEchomeshPath = Nothing
    , dcAllowedOrigins = ["https://claude.ai", "http://localhost:*"]
    }

-- | API endpoint definition
data ApiEndpoint = ApiEndpoint
    { epPath   :: Text
    , epMethod :: Method
    , epDesc   :: Text
    }

-- | List of all endpoints
apiEndpoints :: [ApiEndpoint]
apiEndpoints =
    [ ApiEndpoint "/api/health" methodGet "Health check"
    , ApiEndpoint "/api/verify" methodPost "Verify code"
    , ApiEndpoint "/api/feedback" methodPost "Submit feedback"
    , ApiEndpoint "/api/feedback" methodGet "Get feedback"
    , ApiEndpoint "/api/issues" methodGet "Get known issues"
    , ApiEndpoint "/api/context" methodGet "Get project context"
    , ApiEndpoint "/api/sync" methodPost "Sync with Echomesh"
    ]

-- | Daemon state
data DaemonState = DaemonState
    { dsEchidnaClient :: Maybe EchidnaClient
    , dsFeedbackDb    :: Maybe FeedbackDB
    , dsEchomeshConfig :: EchomeshConfig
    , dsRequestCount  :: IORef Int
    }

-- | Run the daemon
runDaemon :: DaemonConfig -> IO ()
runDaemon config = do
    when (dcVerbose config) $
        putStrLn $ "Starting claude-verify daemon on " <>
                   dcHost config <> ":" <> show (dcPort config)

    -- Initialize state
    state <- initState config

    -- Configure Warp
    let settings = setPort (dcPort config) $
                   setHost (fromString $ dcHost config) $
                   setBeforeMainLoop (when (dcVerbose config) $
                       putStrLn "Daemon ready") $
                   defaultSettings

    -- Run server
    runSettings settings (app config state)

-- | Run daemon in background
runDaemonBackground :: DaemonConfig -> IO ()
runDaemonBackground config = do
    _ <- forkIO $ runDaemon config
    putStrLn $ "Daemon started on port " <> show (dcPort config)

-- | Initialize daemon state
initState :: DaemonConfig -> IO DaemonState
initState config = do
    -- Initialize ECHIDNA client
    echidnaClient <- case dcEchidnaPath config of
        Just path -> Just <$> createClient EchidnaConfig
            { ecBinaryPath = path
            , ecDefaultProvers = [Z3, CVC5]
            , ecTimeoutMs = 30000
            , ecStrategy = defaultStrategy
            , ecUseTempFiles = True
            , ecVerbose = dcVerbose config
            }
        Nothing -> pure Nothing

    -- Initialize feedback database
    feedbackDb <- openDatabase (dcFeedbackDb config)

    -- Initialize Echomesh config
    echomeshConfig <- defaultEchomeshConfig

    -- Request counter
    requestCount <- newIORef 0

    pure DaemonState
        { dsEchidnaClient = echidnaClient
        , dsFeedbackDb = Just feedbackDb
        , dsEchomeshConfig = echomeshConfig
        , dsRequestCount = requestCount
        }

-- | Main application
app :: DaemonConfig -> DaemonState -> Application
app config state req respond = do
    -- Log request
    when (dcVerbose config) $ do
        count <- atomicModifyIORef' (dsRequestCount state) $ \n -> (n+1, n+1)
        putStrLn $ "[" <> show count <> "] " <>
                   show (requestMethod req) <> " " <>
                   show (rawPathInfo req)

    -- Add CORS headers
    let corsHeaders = addCorsHeaders config

    -- Route request
    case (requestMethod req, pathInfo req) of
        ("GET", ["api", "health"]) ->
            handleHealth state >>= respond . corsHeaders

        ("POST", ["api", "verify"]) ->
            handleVerify config state req >>= respond . corsHeaders

        ("POST", ["api", "feedback"]) ->
            handlePostFeedback state req >>= respond . corsHeaders

        ("GET", ["api", "feedback"]) ->
            handleGetFeedback state req >>= respond . corsHeaders

        ("GET", ["api", "issues"]) ->
            handleGetIssues state req >>= respond . corsHeaders

        ("GET", ["api", "context"]) ->
            handleGetContext state req >>= respond . corsHeaders

        ("POST", ["api", "sync"]) ->
            handleSync state req >>= respond . corsHeaders

        ("OPTIONS", _) ->
            respond $ corsHeaders $ responseLBS status200 [] ""

        _ ->
            respond $ corsHeaders $ jsonError status404 "Not found"

-- | Add CORS headers
addCorsHeaders :: DaemonConfig -> Response -> Response
addCorsHeaders config = mapResponseHeaders addHeaders
  where
    addHeaders headers = headers ++
        [ ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        , ("Access-Control-Allow-Headers", "Content-Type")
        ]

-- | Health check handler
handleHealth :: DaemonState -> IO Response
handleHealth state = do
    let echidnaStatus = case dsEchidnaClient state of
            Just _ -> "connected"
            Nothing -> "not_configured"

    let dbStatus = case dsFeedbackDb state of
            Just _ -> "connected"
            Nothing -> "not_configured"

    pure $ jsonResponse status200 $ Aeson.object
        [ "status" .= ("ok" :: Text)
        , "echidna" .= (echidnaStatus :: Text)
        , "database" .= (dbStatus :: Text)
        , "version" .= ("0.1.0" :: Text)
        ]

-- | Verify code handler
handleVerify :: DaemonConfig -> DaemonState -> Request -> IO Response
handleVerify config state req = do
    body <- strictRequestBody req

    case decode body of
        Nothing ->
            pure $ jsonError status400 "Invalid request body"

        Just (VerifyRequest code language project) -> do
            case dsEchidnaClient state of
                Nothing ->
                    pure $ jsonError status503 "ECHIDNA not configured"

                Just client -> do
                    -- Create verification condition
                    uuid <- UUID.nextRandom
                    now <- getCurrentTime

                    let vc = VerificationCondition
                            { vcId = VCId $ T.pack $ show uuid
                            , vcDescription = "User-submitted code"
                            , vcFormula = VCFormula SMTLIB2 code
                            , vcLocation = SourceLocation "<input>" 1 Nothing Nothing Nothing
                            , vcKind = "user_submitted"
                            }

                    -- Verify
                    result <- verify client vc

                    pure $ jsonResponse status200 $ Aeson.object
                        [ "status" .= toStatusText (erAggregateStatus result)
                        , "confidence" .= erConfidence result
                        , "message" .= toStatusMessage (erAggregateStatus result)
                        , "provers" .= map proverToJson (erProverResponses result)
                        ]
  where
    toStatusText = \case
        Proved -> "proved" :: Text
        Counterexample -> "counterexample"
        Unknown -> "unknown"
        Error _ -> "error"

    toStatusMessage = \case
        Proved -> "Code verified correct" :: Text
        Counterexample -> "Issue found in code"
        Unknown -> "Could not verify"
        Error e -> "Error: " <> e

    proverToJson pr = Aeson.object
        [ "prover" .= prpProver pr
        , "status" .= show (prpStatus pr)
        , "time_ms" .= prpTimeMs pr
        ]

-- | Verify request body
data VerifyRequest = VerifyRequest
    { vrCode     :: Text
    , vrLanguage :: Text
    , vrProject  :: Maybe Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON)

-- | Post feedback handler
handlePostFeedback :: DaemonState -> Request -> IO Response
handlePostFeedback state req = do
    body <- strictRequestBody req

    case decode body of
        Nothing ->
            pure $ jsonError status400 "Invalid request body"

        Just (FeedbackRequest output issue correction category project) -> do
            case dsFeedbackDb state of
                Nothing ->
                    pure $ jsonError status503 "Database not configured"

                Just db -> do
                    item <- recordUserCorrection db project Nothing
                        output issue (fromMaybe "" correction)
                        (textToCategory category)

                    pure $ jsonResponse status200 $ Aeson.object
                        [ "id" .= show (fiId item)
                        , "status" .= ("recorded" :: Text)
                        ]
  where
    textToCategory = \case
        "syntax_error" -> SyntaxError
        "type_error" -> TypeError
        "logic_error" -> LogicError
        "security_flaw" -> SecurityFlaw
        "performance" -> PerformanceIssue
        "semantic" -> SemanticMismatch
        "style" -> StyleViolation
        other -> Other other

-- | Feedback request body
data FeedbackRequest = FeedbackRequest
    { frOutput     :: Text
    , frIssue      :: Text
    , frCorrection :: Maybe Text
    , frCategory   :: Text
    , frProject    :: Maybe Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON)

-- | Get feedback handler
handleGetFeedback :: DaemonState -> Request -> IO Response
handleGetFeedback state req = do
    let params = queryString req
        limit = maybe 20 (read . T.unpack . TE.decodeUtf8) $
                lookup "limit" params >>= id

    case dsFeedbackDb state of
        Nothing ->
            pure $ jsonError status503 "Database not configured"

        Just db -> do
            items <- getRecentFeedback db limit
            pure $ jsonResponse status200 $ Aeson.toJSON items

-- | Get known issues handler
handleGetIssues :: DaemonState -> Request -> IO Response
handleGetIssues state req = do
    let params = queryString req
        project = lookup "project" params >>= fmap TE.decodeUtf8

    case (dsFeedbackDb state, project) of
        (Nothing, _) ->
            pure $ jsonError status503 "Database not configured"

        (Just db, Just p) -> do
            issues <- getKnownIssues db p
            pure $ jsonResponse status200 $ Aeson.toJSON issues

        (Just db, Nothing) -> do
            issues <- getKnownIssues db "default"
            pure $ jsonResponse status200 $ Aeson.toJSON issues

-- | Get context handler
handleGetContext :: DaemonState -> Request -> IO Response
handleGetContext state _req = do
    context <- buildSessionContext (dsEchomeshConfig state)
    pure $ jsonResponse status200 $ Aeson.object
        [ "context" .= context
        ]

-- | Sync with Echomesh handler
handleSync :: DaemonState -> Request -> IO Response
handleSync state _req = do
    case dsFeedbackDb state of
        Nothing ->
            pure $ jsonError status503 "Database not configured"

        Just db -> do
            feedback <- getRecentFeedback db 100
            ctx <- syncWithEchomesh (dsEchomeshConfig state) feedback

            pure $ jsonResponse status200 $ Aeson.object
                [ "status" .= ("synced" :: Text)
                , "items" .= length (ecItems ctx)
                ]

-- | JSON response helper
jsonResponse :: Status -> Aeson.Value -> Response
jsonResponse status body =
    responseLBS status
        [("Content-Type", "application/json")]
        (encode body)

-- | JSON error helper
jsonError :: Status -> Text -> Response
jsonError status message =
    jsonResponse status $ Aeson.object
        [ "error" .= message
        ]

-- | fromString for Host
fromString :: String -> HostPreference
fromString "*" = HostAny
fromString s = Host s
