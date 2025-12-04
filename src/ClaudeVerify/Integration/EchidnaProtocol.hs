{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Integration.EchidnaProtocol
-- Description : ECHIDNA communication protocol
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module defines the protocol for communicating with the ECHIDNA
-- neurosymbolic theorem proving platform.
--
-- ECHIDNA supports multiple communication modes:
--
-- * CLI: Invoke echidna binary with arguments
-- * JSON-RPC: HTTP API for interactive use
-- * gRPC: Streaming protocol for long-running verification
-- * Pipe: Unix pipe for high-throughput batch processing
--
-- The protocol is designed to be:
--
-- * Language-agnostic (SMT-LIB, TPTP, native prover formats)
-- * Prover-agnostic (unified result format)
-- * Streaming-capable (partial results, progress updates)

module ClaudeVerify.Integration.EchidnaProtocol
    ( -- * Protocol Types
      EchidnaRequest(..)
    , EchidnaResponse(..)
    , ProverTask(..)
    , ProverResult(..)
    , ProverStatus(..)
    , ProofWitness(..)
    , CounterModel(..)

      -- * Communication Modes
    , CommunicationMode(..)
    , selectMode

      -- * CLI Protocol
    , runEchidnaCLI
    , parseEchidnaOutput

      -- * JSON-RPC Protocol
    , JsonRpcRequest(..)
    , JsonRpcResponse(..)
    , sendJsonRpc

      -- * Streaming Protocol
    , StreamingSession(..)
    , openStreamingSession
    , closeStreamingSession
    , submitTask
    , receiveResult
    , receiveProgress

      -- * Format Conversion
    , toSMTLIB2
    , toTPTP
    , fromProverOutput
    ) where

import ClaudeVerify.Internal.Types hiding (ProverResult, ProverStatus)
import qualified ClaudeVerify.Internal.Types as Types

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception (bracket, try, SomeException)
import Control.Monad (forever, when, void)
import Data.Aeson (FromJSON, ToJSON, encode, decode, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetLine, hPutStrLn, hClose, hFlush, hIsEOF)
import System.Process
import System.Timeout (timeout)

-- | Prover status in ECHIDNA format
data ProverStatus
    = StatusProved
    | StatusCounterexample
    | StatusUnknown
    | StatusTimeout
    | StatusError Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Proof witness from a prover
data ProofWitness = ProofWitness
    { pwProver     :: Text
    , pwFormat     :: Text       -- ^ e.g., "lean4", "coq", "smt2"
    , pwContent    :: Text       -- ^ Serialized proof
    , pwCheckable  :: Bool       -- ^ Can be independently verified
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Counter-model from a prover
data CounterModel = CounterModel
    { cmProver     :: Text
    , cmFormat     :: Text       -- ^ e.g., "smt2-model", "alloy"
    , cmBindings   :: Map Text Text  -- ^ Variable -> Value
    , cmTrace      :: Maybe Text     -- ^ Execution trace (if available)
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Result from a single prover
data ProverResult = ProverResult
    { prProver        :: Text
    , prStatus        :: ProverStatus
    , prTimeMs        :: Int
    , prWitness       :: Maybe ProofWitness
    , prCounterModel  :: Maybe CounterModel
    , prRawOutput     :: Text
    , prMemoryKb      :: Maybe Int
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | A verification task for ECHIDNA
data ProverTask = ProverTask
    { ptTaskId       :: UUID
    , ptFormula      :: Text
    , ptFormat       :: Text           -- ^ "smtlib2", "tptp", "lean4", etc.
    , ptProvers      :: [Text]         -- ^ Provers to use
    , ptTimeoutMs    :: Int
    , ptRequireProof :: Bool           -- ^ Require proof certificate
    , ptMetadata     :: Map Text Text  -- ^ Additional metadata
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Request to ECHIDNA
data EchidnaRequest = EchidnaRequest
    { reqVersion   :: Text
    , reqMethod    :: Text
    , reqTasks     :: [ProverTask]
    , reqOptions   :: Map Text Aeson.Value
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Response from ECHIDNA
data EchidnaResponse = EchidnaResponse
    { resVersion     :: Text
    , resStatus      :: Text
    , resResults     :: [ProverResult]
    , resAggregated  :: Maybe AggregatedResult
    , resTimestamp   :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Aggregated result across provers
data AggregatedResult = AggregatedResult
    { arStatus      :: ProverStatus
    , arConfidence  :: Double
    , arAgreement   :: Int    -- ^ Number of provers that agree
    , arDisagreement :: Int   -- ^ Number that disagree
    , arExplanation :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Communication mode
data CommunicationMode
    = ModeCLI FilePath        -- ^ CLI binary path
    | ModeJsonRpc Text        -- ^ HTTP endpoint
    | ModeGrpc Text Int       -- ^ Host and port
    | ModePipe Handle Handle  -- ^ Input and output handles
    deriving stock (Show)

-- | Select appropriate communication mode
selectMode :: Maybe FilePath -> Maybe Text -> IO CommunicationMode
selectMode mBinary mEndpoint = case (mBinary, mEndpoint) of
    (Just bin, _) -> pure $ ModeCLI bin
    (_, Just endpoint) -> pure $ ModeJsonRpc endpoint
    _ -> do
        -- Try to find echidna in PATH
        (exitCode, stdout, _) <- readProcessWithExitCode "which" ["echidna"] ""
        case exitCode of
            ExitSuccess -> pure $ ModeCLI $ init stdout  -- Remove newline
            ExitFailure _ -> pure $ ModeJsonRpc "http://localhost:9848"

-- | Run ECHIDNA via CLI
runEchidnaCLI :: FilePath -> ProverTask -> IO (Either Text EchidnaResponse)
runEchidnaCLI binary task = do
    let args =
            [ "prove"
            , "--format", T.unpack (ptFormat task)
            , "--timeout", show (ptTimeoutMs task)
            , "--json"
            ] ++
            concatMap (\p -> ["--prover", T.unpack p]) (ptProvers task) ++
            (if ptRequireProof task then ["--require-proof"] else [])

    result <- try $ do
        (exitCode, stdout, stderr) <- readProcessWithExitCode binary args
            (T.unpack $ ptFormula task)
        pure (exitCode, stdout, stderr)

    case result of
        Left (e :: SomeException) ->
            pure $ Left $ "Failed to run ECHIDNA: " <> T.pack (show e)
        Right (ExitSuccess, stdout, _) ->
            case decode (BLC.pack stdout) of
                Just response -> pure $ Right response
                Nothing -> pure $ Left $ "Failed to parse ECHIDNA output: " <> T.pack stdout
        Right (ExitFailure code, _, stderr) ->
            pure $ Left $ "ECHIDNA failed with code " <> T.pack (show code) <>
                          ": " <> T.pack stderr

-- | Parse ECHIDNA output (when not JSON)
parseEchidnaOutput :: Text -> Either Text [ProverResult]
parseEchidnaOutput output = do
    let lines = T.lines output
    Right $ mapMaybe parseLine lines
  where
    parseLine line
        | "PROVED" `T.isInfixOf` line = Just $ mkResult StatusProved line
        | "COUNTEREXAMPLE" `T.isInfixOf` line = Just $ mkResult StatusCounterexample line
        | "TIMEOUT" `T.isInfixOf` line = Just $ mkResult StatusTimeout line
        | "ERROR" `T.isInfixOf` line = Just $ mkResult (StatusError line) line
        | otherwise = Nothing

    mkResult status line = ProverResult
        { prProver = extractProver line
        , prStatus = status
        , prTimeMs = extractTime line
        , prWitness = Nothing
        , prCounterModel = Nothing
        , prRawOutput = line
        , prMemoryKb = Nothing
        }

    extractProver line =
        fromMaybe "unknown" $ listToMaybe $
            filter (`T.isInfixOf` T.toLower line)
                ["z3", "cvc5", "lean", "coq", "agda", "vampire"]

    extractTime line =
        case T.breakOn "ms" line of
            (before, "ms") ->
                let numStr = T.takeWhileEnd (`elem` ['0'..'9']) before
                in fromMaybe 0 $ readMaybe $ T.unpack numStr
            _ -> 0

    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

    mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []

-- | JSON-RPC request
data JsonRpcRequest = JsonRpcRequest
    { jrpcVersion :: Text
    , jrpcMethod  :: Text
    , jrpcParams  :: Aeson.Value
    , jrpcId      :: Int
    } deriving stock (Eq, Show, Generic)

instance ToJSON JsonRpcRequest where
    toJSON req = Aeson.object
        [ "jsonrpc" .= jrpcVersion req
        , "method" .= jrpcMethod req
        , "params" .= jrpcParams req
        , "id" .= jrpcId req
        ]

-- | JSON-RPC response
data JsonRpcResponse = JsonRpcResponse
    { jrpcRespVersion :: Text
    , jrpcResult      :: Maybe Aeson.Value
    , jrpcError       :: Maybe JsonRpcError
    , jrpcRespId      :: Int
    } deriving stock (Eq, Show, Generic)

instance FromJSON JsonRpcResponse where
    parseJSON = Aeson.withObject "JsonRpcResponse" $ \v -> JsonRpcResponse
        <$> v .: "jsonrpc"
        <*> v .:? "result"
        <*> v .:? "error"
        <*> v .: "id"

-- | JSON-RPC error
data JsonRpcError = JsonRpcError
    { errCode    :: Int
    , errMessage :: Text
    , errData    :: Maybe Aeson.Value
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Send JSON-RPC request
sendJsonRpc :: Manager -> Text -> Text -> Aeson.Value -> IO (Either Text Aeson.Value)
sendJsonRpc manager endpoint method params = do
    let request = JsonRpcRequest
            { jrpcVersion = "2.0"
            , jrpcMethod = method
            , jrpcParams = params
            , jrpcId = 1
            }

    req <- parseRequest $ T.unpack endpoint
    let req' = req
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode request
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                ]
            }

    result <- try $ httpLbs req' manager

    case result of
        Left (e :: SomeException) ->
            pure $ Left $ "HTTP request failed: " <> T.pack (show e)
        Right response ->
            case decode (responseBody response) of
                Just (JsonRpcResponse _ (Just result) Nothing _) ->
                    pure $ Right result
                Just (JsonRpcResponse _ _ (Just err) _) ->
                    pure $ Left $ errMessage err
                _ ->
                    pure $ Left $ "Invalid JSON-RPC response"

-- | Streaming session for long-running verification
data StreamingSession = StreamingSession
    { ssProcess     :: ProcessHandle
    , ssStdin       :: Handle
    , ssStdout      :: Handle
    , ssResultChan  :: Chan (Either Text ProverResult)
    , ssProgressRef :: IORef (Map UUID Double)
    , ssReaderThread :: ThreadId
    }

-- | Open a streaming session with ECHIDNA
openStreamingSession :: FilePath -> IO (Either Text StreamingSession)
openStreamingSession binary = do
    result <- try $ createProcess (proc binary ["serve", "--stream"])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

    case result of
        Left (e :: SomeException) ->
            pure $ Left $ "Failed to start ECHIDNA: " <> T.pack (show e)
        Right (Just stdin, Just stdout, _, ph) -> do
            resultChan <- newChan
            progressRef <- newIORef Map.empty

            -- Start reader thread
            readerThread <- forkIO $ forever $ do
                eof <- hIsEOF stdout
                if eof
                    then pure ()
                    else do
                        line <- hGetLine stdout
                        case decode (BLC.pack line) of
                            Just (pr :: ProverResult) ->
                                writeChan resultChan (Right pr)
                            Nothing ->
                                when ("progress:" `T.isPrefixOf` T.pack line) $ do
                                    -- Parse progress update
                                    pure ()

            pure $ Right StreamingSession
                { ssProcess = ph
                , ssStdin = stdin
                , ssStdout = stdout
                , ssResultChan = resultChan
                , ssProgressRef = progressRef
                , ssReaderThread = readerThread
                }
        Right _ ->
            pure $ Left "Failed to create pipes"

-- | Close streaming session
closeStreamingSession :: StreamingSession -> IO ()
closeStreamingSession ss = do
    killThread (ssReaderThread ss)
    hClose (ssStdin ss)
    hClose (ssStdout ss)
    terminateProcess (ssProcess ss)

-- | Submit a task to streaming session
submitTask :: StreamingSession -> ProverTask -> IO ()
submitTask ss task = do
    BL.hPut (ssStdin ss) (encode task)
    hPutStrLn (ssStdin ss) ""
    hFlush (ssStdin ss)

-- | Receive next result (blocking)
receiveResult :: StreamingSession -> IO (Either Text ProverResult)
receiveResult ss = readChan (ssResultChan ss)

-- | Get current progress for a task
receiveProgress :: StreamingSession -> UUID -> IO (Maybe Double)
receiveProgress ss taskId = do
    progress <- readIORef (ssProgressRef ss)
    pure $ Map.lookup taskId progress

-- | Convert formula to SMT-LIB2 format
toSMTLIB2 :: VCFormula -> Text
toSMTLIB2 formula = case vcfFormat formula of
    SMTLIB2 -> vcfContent formula
    _ -> "; Conversion from " <> T.pack (show $ vcfFormat formula) <>
         " not implemented\n" <> vcfContent formula

-- | Convert formula to TPTP format
toTPTP :: VCFormula -> Text
toTPTP formula = case vcfFormat formula of
    TPTP -> vcfContent formula
    SMTLIB2 -> smtlibToTPTP (vcfContent formula)
    _ -> "% Conversion from " <> T.pack (show $ vcfFormat formula) <>
         " not implemented\n" <> vcfContent formula

-- | Convert SMT-LIB to TPTP (simplified)
smtlibToTPTP :: Text -> Text
smtlibToTPTP smt =
    -- This is a simplified converter
    -- Real implementation would need full SMT-LIB parser
    let lines = T.lines smt
        converted = map convertLine lines
    in T.unlines converted
  where
    convertLine line
        | "(assert" `T.isPrefixOf` line =
            "fof(assertion, axiom, " <> extractFormula line <> ")."
        | "(declare-" `T.isPrefixOf` line = "% " <> line
        | otherwise = "% " <> line

    extractFormula line =
        let content = T.drop 8 $ T.dropEnd 1 line  -- Remove "(assert " and ")"
        in sexpToTPTP content

    sexpToTPTP sexp = sexp  -- Simplified: would need full converter

-- | Parse prover output into our types
fromProverOutput :: Text -> Text -> Either Text Types.ProverResult
fromProverOutput prover output = Right Types.ProverResult
    { Types.prProver = prover
    , Types.prStatus = detectStatus output
    , Types.prTimeMs = extractTimeMs output
    , Types.prCertificate = extractCertificate output
    , Types.prCounterexample = extractCounterexample output
    , Types.prRawOutput = output
    }
  where
    detectStatus txt
        | "unsat" `T.isInfixOf` T.toLower txt = Types.Proved
        | "sat" `T.isInfixOf` T.toLower txt = Types.Counterexample
        | "timeout" `T.isInfixOf` T.toLower txt = Types.Unknown
        | "error" `T.isInfixOf` T.toLower txt =
            Types.Error $ T.take 100 txt
        | otherwise = Types.Unknown

    extractTimeMs _ = 0  -- Would parse from output

    extractCertificate txt
        | "proof" `T.isInfixOf` T.toLower txt =
            Just $ Types.ProofCertificate txt
        | otherwise = Nothing

    extractCounterexample txt
        | "model" `T.isInfixOf` T.toLower txt =
            Just $ Types.Counterexample txt
        | otherwise = Nothing
