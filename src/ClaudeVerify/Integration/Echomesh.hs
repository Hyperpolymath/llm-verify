{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Integration.Echomesh
-- Description : Echomesh integration for cross-session context persistence
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module integrates claude-verify with Echomesh, a cross-session
-- context persistence system. It enables:
--
-- * Exporting feedback and known issues to Echomesh format
-- * Importing context from Echomesh at session start
-- * Syncing verification results across sessions
-- * Building project memory that persists beyond conversations
--
-- Echomesh Format:
--
-- Echomesh uses a hierarchical context structure with:
--
-- * Layers: project > session > conversation
-- * Types: facts, issues, decisions, artifacts
-- * Confidence: weighted by recency and verification status

module ClaudeVerify.Integration.Echomesh
    ( -- * Configuration
      EchomeshConfig(..)
    , defaultEchomeshConfig

      -- * Context Types
    , EchomeshContext(..)
    , ContextLayer(..)
    , ContextItem(..)
    , ContextType(..)

      -- * Export
    , exportToEchomesh
    , exportFeedbackToContext
    , exportKnownIssuesToContext

      -- * Import
    , importFromEchomesh
    , loadProjectContext
    , loadSessionContext

      -- * Sync
    , syncWithEchomesh
    , pushVerificationResult
    , pullKnownIssues

      -- * Context Building
    , buildSessionContext
    , mergeContexts
    , pruneStaleContext
    ) where

import ClaudeVerify.Internal.Types
import ClaudeVerify.Feedback (FeedbackItem(..), FeedbackCategory(..), FeedbackSource(..))

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, nominalDay)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

-- | Echomesh configuration
data EchomeshConfig = EchomeshConfig
    { emBasePath      :: FilePath      -- ^ Base path for Echomesh storage
    , emProjectId     :: Maybe Text    -- ^ Current project ID
    , emSessionId     :: Maybe Text    -- ^ Current session ID
    , emMaxAge        :: Int           -- ^ Max age in days for context items
    , emMaxItems      :: Int           -- ^ Max items per layer
    , emConfidenceDecay :: Double      -- ^ Daily confidence decay factor
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Default configuration
defaultEchomeshConfig :: IO EchomeshConfig
defaultEchomeshConfig = do
    home <- getHomeDirectory
    pure EchomeshConfig
        { emBasePath = home </> ".echomesh"
        , emProjectId = Nothing
        , emSessionId = Nothing
        , emMaxAge = 30
        , emMaxItems = 100
        , emConfidenceDecay = 0.95
        }

-- | Context layer in the hierarchy
data ContextLayer
    = LayerGlobal      -- ^ Applies to all projects
    | LayerProject     -- ^ Project-specific
    | LayerSession     -- ^ Session-specific
    | LayerConversation -- ^ Current conversation only
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Type of context item
data ContextType
    = TypeFact         -- ^ Established fact about the codebase
    | TypeIssue        -- ^ Known issue or bug pattern
    | TypeDecision     -- ^ Design/architectural decision
    | TypeArtifact     -- ^ Generated code or document
    | TypeFeedback     -- ^ Correction feedback
    | TypeVerification -- ^ Verification result
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A single context item
data ContextItem = ContextItem
    { ciId          :: UUID
    , ciType        :: ContextType
    , ciLayer       :: ContextLayer
    , ciContent     :: Text
    , ciMetadata    :: Map Text Text
    , ciConfidence  :: Double          -- ^ 0.0 to 1.0
    , ciCreatedAt   :: UTCTime
    , ciUpdatedAt   :: UTCTime
    , ciExpiresAt   :: Maybe UTCTime
    , ciSourceFile  :: Maybe FilePath
    , ciSourceLine  :: Maybe Int
    , ciTags        :: [Text]
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Full Echomesh context
data EchomeshContext = EchomeshContext
    { ecProjectId   :: Maybe Text
    , ecSessionId   :: Maybe Text
    , ecItems       :: [ContextItem]
    , ecVersion     :: Int
    , ecUpdatedAt   :: UTCTime
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Export feedback items to Echomesh context
exportFeedbackToContext :: [FeedbackItem] -> IO EchomeshContext
exportFeedbackToContext items = do
    now <- getCurrentTime
    contextItems <- mapM feedbackToContextItem items
    pure EchomeshContext
        { ecProjectId = Nothing
        , ecSessionId = Nothing
        , ecItems = contextItems
        , ecVersion = 1
        , ecUpdatedAt = now
        }

-- | Convert a feedback item to a context item
feedbackToContextItem :: FeedbackItem -> IO ContextItem
feedbackToContextItem fb = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime
    pure ContextItem
        { ciId = uuid
        , ciType = TypeFeedback
        , ciLayer = LayerProject
        , ciContent = formatFeedbackContent fb
        , ciMetadata = Map.fromList
            [ ("category", categoryToText $ fiCategory fb)
            , ("detected_by", sourceToText $ fiDetectedBy fb)
            , ("verified", if fiVerified fb then "true" else "false")
            ]
        , ciConfidence = if fiVerified fb then 0.9 else 0.7
        , ciCreatedAt = fiTimestamp fb
        , ciUpdatedAt = now
        , ciExpiresAt = Nothing
        , ciSourceFile = fiSourceFile fb
        , ciSourceLine = fmap fst $ fiSourceLines fb
        , ciTags = categoryToTags (fiCategory fb)
        }
  where
    formatFeedbackContent fb =
        "Issue: " <> fiActualIssue fb <>
        maybe "" ("\nCorrection: " <>) (fiCorrection fb)

    categoryToText = \case
        SyntaxError -> "syntax_error"
        TypeError -> "type_error"
        LogicError -> "logic_error"
        SecurityFlaw -> "security_flaw"
        PerformanceIssue -> "performance"
        SemanticMismatch -> "semantic"
        StyleViolation -> "style"
        Other t -> t

    sourceToText = \case
        Compiler -> "compiler"
        TypeChecker -> "typechecker"
        Linter -> "linter"
        TestSuite -> "test"
        FormalProver -> "prover"
        StaticAnalyzer -> "analyzer"
        UserReported -> "user"
        RuntimeError -> "runtime"

    categoryToTags = \case
        SyntaxError -> ["syntax", "parse"]
        TypeError -> ["type", "safety"]
        LogicError -> ["logic", "correctness"]
        SecurityFlaw -> ["security", "vulnerability"]
        PerformanceIssue -> ["performance", "optimization"]
        SemanticMismatch -> ["semantic", "specification"]
        StyleViolation -> ["style", "convention"]
        Other _ -> []

-- | Export known issues to Echomesh context
exportKnownIssuesToContext :: [KnownIssue] -> IO EchomeshContext
exportKnownIssuesToContext issues = do
    now <- getCurrentTime
    contextItems <- mapM issueToContextItem issues
    pure EchomeshContext
        { ecProjectId = Nothing
        , ecSessionId = Nothing
        , ecItems = contextItems
        , ecVersion = 1
        , ecUpdatedAt = now
        }

-- | Convert a known issue to context item
issueToContextItem :: KnownIssue -> IO ContextItem
issueToContextItem ki = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime
    pure ContextItem
        { ciId = uuid
        , ciType = TypeIssue
        , ciLayer = LayerProject
        , ciContent = kiDescription ki <> "\nGuidance: " <> kiGuidance ki
        , ciMetadata = Map.fromList
            [ ("pattern", kiPattern ki)
            , ("occurrences", T.pack $ show $ kiOccurrences ki)
            ]
        , ciConfidence = min 1.0 (0.5 + 0.1 * fromIntegral (kiOccurrences ki))
        , ciCreatedAt = now
        , ciUpdatedAt = now
        , ciExpiresAt = Nothing
        , ciSourceFile = Nothing
        , ciSourceLine = Nothing
        , ciTags = ["known-issue", "avoid"]
        }

-- | Export context to Echomesh storage
exportToEchomesh :: EchomeshConfig -> EchomeshContext -> IO FilePath
exportToEchomesh config ctx = do
    let basePath = emBasePath config
    createDirectoryIfMissing True basePath

    let projectPath = case ecProjectId ctx of
            Just pid -> basePath </> T.unpack pid
            Nothing -> basePath </> "global"

    createDirectoryIfMissing True projectPath

    let filename = case ecSessionId ctx of
            Just sid -> "session-" <> T.unpack sid <> ".json"
            Nothing -> "context.json"

    let fullPath = projectPath </> filename
    BL.writeFile fullPath (encode ctx)
    pure fullPath

-- | Import context from Echomesh
importFromEchomesh :: EchomeshConfig -> IO (Maybe EchomeshContext)
importFromEchomesh config = do
    let basePath = emBasePath config

    -- Try session context first
    sessionCtx <- case (emProjectId config, emSessionId config) of
        (Just pid, Just sid) -> do
            let path = basePath </> T.unpack pid </> ("session-" <> T.unpack sid <> ".json")
            loadContext path
        _ -> pure Nothing

    -- Fall back to project context
    projectCtx <- case emProjectId config of
        Just pid -> do
            let path = basePath </> T.unpack pid </> "context.json"
            loadContext path
        _ -> pure Nothing

    -- Merge if both exist
    case (sessionCtx, projectCtx) of
        (Just s, Just p) -> Just <$> mergeContexts p s
        (Just s, Nothing) -> pure $ Just s
        (Nothing, Just p) -> pure $ Just p
        (Nothing, Nothing) -> pure Nothing

-- | Load context from file
loadContext :: FilePath -> IO (Maybe EchomeshContext)
loadContext path = do
    exists <- doesFileExist path
    if exists
        then decode <$> BL.readFile path
        else pure Nothing

-- | Load project-level context
loadProjectContext :: EchomeshConfig -> Text -> IO (Maybe EchomeshContext)
loadProjectContext config projectId = do
    let path = emBasePath config </> T.unpack projectId </> "context.json"
    loadContext path

-- | Load session-level context
loadSessionContext :: EchomeshConfig -> Text -> Text -> IO (Maybe EchomeshContext)
loadSessionContext config projectId sessionId = do
    let path = emBasePath config </> T.unpack projectId </> ("session-" <> T.unpack sessionId <> ".json")
    loadContext path

-- | Sync local feedback with Echomesh
syncWithEchomesh :: EchomeshConfig -> [FeedbackItem] -> IO EchomeshContext
syncWithEchomesh config feedback = do
    -- Load existing context
    existing <- importFromEchomesh config

    -- Convert new feedback
    newCtx <- exportFeedbackToContext feedback

    -- Merge
    merged <- case existing of
        Just e -> mergeContexts e newCtx
        Nothing -> pure newCtx

    -- Apply decay and pruning
    now <- getCurrentTime
    let decayed = applyConfidenceDecay config now merged
        pruned = pruneStaleContext config now decayed

    -- Save
    _ <- exportToEchomesh config pruned
    pure pruned

-- | Push a verification result to Echomesh
pushVerificationResult :: EchomeshConfig -> VerificationResult -> IO ()
pushVerificationResult config result = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime

    let item = ContextItem
            { ciId = uuid
            , ciType = TypeVerification
            , ciLayer = LayerSession
            , ciContent = formatVerificationResult result
            , ciMetadata = Map.fromList
                [ ("status", T.pack $ show $ vrAggregateStatus result)
                , ("confidence", T.pack $ show $ vrConfidence result)
                , ("vc_id", unVCId $ vrVCId result)
                ]
            , ciConfidence = vrConfidence result
            , ciCreatedAt = now
            , ciUpdatedAt = now
            , ciExpiresAt = Nothing
            , ciSourceFile = Nothing
            , ciSourceLine = Nothing
            , ciTags = ["verification", statusToTag (vrAggregateStatus result)]
            }

    -- Load, add, save
    existing <- importFromEchomesh config
    let updated = case existing of
            Just ctx -> ctx { ecItems = item : ecItems ctx, ecUpdatedAt = now }
            Nothing -> EchomeshContext
                { ecProjectId = emProjectId config
                , ecSessionId = emSessionId config
                , ecItems = [item]
                , ecVersion = 1
                , ecUpdatedAt = now
                }

    _ <- exportToEchomesh config updated
    pure ()
  where
    formatVerificationResult r =
        "VC " <> unVCId (vrVCId r) <> ": " <>
        T.pack (show $ vrAggregateStatus r) <>
        " (" <> T.pack (show $ round (vrConfidence r * 100) :: Int) <> "% confidence)"

    statusToTag = \case
        Proved -> "proved"
        Counterexample -> "counterexample"
        Unknown -> "unknown"
        Error _ -> "error"

-- | Pull known issues from Echomesh
pullKnownIssues :: EchomeshConfig -> IO [KnownIssue]
pullKnownIssues config = do
    ctx <- importFromEchomesh config
    pure $ case ctx of
        Just c -> mapMaybe contextItemToKnownIssue (ecItems c)
        Nothing -> []

-- | Convert context item back to known issue
contextItemToKnownIssue :: ContextItem -> Maybe KnownIssue
contextItemToKnownIssue item
    | ciType item == TypeIssue || ciType item == TypeFeedback =
        Just KnownIssue
            { kiPattern = fromMaybe "" $ Map.lookup "pattern" (ciMetadata item)
            , kiDescription = ciContent item
            , kiGuidance = ciContent item
            , kiOccurrences = maybe 1 (read . T.unpack) $
                Map.lookup "occurrences" (ciMetadata item)
            }
    | otherwise = Nothing

-- | Build session context for injection
buildSessionContext :: EchomeshConfig -> IO Text
buildSessionContext config = do
    ctx <- importFromEchomesh config
    now <- getCurrentTime

    case ctx of
        Nothing -> pure ""
        Just c -> do
            let relevant = filter (isRelevant now) (ecItems c)
                sorted = sortByConfidence relevant
                formatted = map formatContextItem sorted

            pure $ T.unlines $
                [ "## Session Context"
                , ""
                , "*Loaded from Echomesh*"
                , ""
                ] ++ formatted
  where
    isRelevant now item =
        case ciExpiresAt item of
            Just expires -> expires > now
            Nothing -> True

    sortByConfidence = sortOn (negate . ciConfidence)

    formatContextItem item =
        "- [" <> typeLabel (ciType item) <> "] " <>
        T.take 100 (ciContent item) <>
        " (" <> T.pack (show $ round (ciConfidence item * 100) :: Int) <> "% conf)"

    typeLabel = \case
        TypeFact -> "FACT"
        TypeIssue -> "ISSUE"
        TypeDecision -> "DECISION"
        TypeArtifact -> "ARTIFACT"
        TypeFeedback -> "FEEDBACK"
        TypeVerification -> "VERIFIED"

-- | Merge two contexts, preferring newer items
mergeContexts :: EchomeshContext -> EchomeshContext -> IO EchomeshContext
mergeContexts older newer = do
    now <- getCurrentTime

    -- Deduplicate by content similarity
    let olderItems = ecItems older
        newerItems = ecItems newer
        merged = newerItems ++ filter (not . isDuplicateOf newerItems) olderItems

    pure EchomeshContext
        { ecProjectId = ecProjectId newer <|> ecProjectId older
        , ecSessionId = ecSessionId newer <|> ecSessionId older
        , ecItems = merged
        , ecVersion = max (ecVersion older) (ecVersion newer) + 1
        , ecUpdatedAt = now
        }
  where
    isDuplicateOf items item =
        any (\i -> similarity (ciContent i) (ciContent item) > 0.8) items

    similarity t1 t2 =
        let words1 = T.words $ T.toLower t1
            words2 = T.words $ T.toLower t2
            common = length $ filter (`elem` words2) words1
            total = max (length words1) (length words2)
        in if total == 0 then 0 else fromIntegral common / fromIntegral total

    (<|>) = \case
        Just x -> const $ Just x
        Nothing -> id

-- | Apply confidence decay based on age
applyConfidenceDecay :: EchomeshConfig -> UTCTime -> EchomeshContext -> EchomeshContext
applyConfidenceDecay config now ctx =
    ctx { ecItems = map decay (ecItems ctx) }
  where
    decay item =
        let age = diffUTCTime now (ciUpdatedAt item)
            days = realToFrac age / realToFrac nominalDay
            factor = emConfidenceDecay config ** days
            newConfidence = max 0.1 (ciConfidence item * factor)
        in item { ciConfidence = newConfidence }

-- | Remove stale context items
pruneStaleContext :: EchomeshConfig -> UTCTime -> EchomeshContext -> EchomeshContext
pruneStaleContext config now ctx =
    ctx { ecItems = take (emMaxItems config) $ filter isValid (ecItems ctx) }
  where
    isValid item =
        let age = diffUTCTime now (ciCreatedAt item)
            maxAge = fromIntegral (emMaxAge config) * nominalDay
        in age < maxAge && ciConfidence item > 0.1

-- | Sort by confidence (descending)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy' (comparing fst) . map (\x -> (f x, x))
  where
    sortBy' _ [] = []
    sortBy' cmp (x:xs) =
        let (lesser, greater) = partition (\y -> cmp y x == LT) xs
        in sortBy' cmp lesser ++ [x] ++ sortBy' cmp greater

    comparing f x y = compare (f x) (f y)

    partition _ [] = ([], [])
    partition p (x:xs) =
        let (yes, no) = partition p xs
        in if p x then (x:yes, no) else (yes, x:no)
