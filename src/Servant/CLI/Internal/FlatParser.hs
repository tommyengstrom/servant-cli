{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Servant.CLI.Internal.FlatParser
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Flat (curl-style) command line parser for servant APIs.  Instead of
-- nested subcommands, presents the API as flat routes that can be
-- browsed and executed using URL-style paths.
module Servant.CLI.Internal.FlatParser
  ( -- * Route display
    PathSeg (..),
    RouteInfo (..),
    enumerateRoutes,

    -- * Route matching
    MatchOutcome (..),
    matchRoute,

    -- * Main entry point
    flatStructParser,

    -- * Helpers
    runReadM,
    preprocessArgs,
    extractBaseUrl,
    resolveBaseUrl,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Functor.Combinator (Day (..))
import Data.List (intercalate, isPrefixOf, nub)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics ((:+:) (..))
import qualified Network.HTTP.Types as HTTP
import Options.Applicative
import Servant.CLI.Internal.PStruct
import Servant.Client.Core (BaseUrl (..), parseBaseUrl, showBaseUrl)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | A segment in a route pattern.
data PathSeg
  = PathLit String
  | PathCap String
  deriving (Eq, Show)

-- | Information about a single route for display purposes.
data RouteInfo = RouteInfo
  { riPattern :: [PathSeg],
    riMethods :: [String],
    riInfo :: [String]
  }
  deriving (Show)

-- | Enumerate all routes in a 'PStruct' for display.
enumerateRoutes :: PStruct a -> [RouteInfo]
enumerateRoutes = go [] []
  where
    go :: [String] -> [PathSeg] -> PStruct a -> [RouteInfo]
    go accInfo prefix (PStruct infos comps caps eps) =
      epRoutes ++ compRoutes ++ capRoutes
      where
        allInfo = nub (accInfo ++ infos)
        methods = map methodStr (M.keys (epmGiven eps))
        hasRaw = case epmRaw eps of
          Nothing -> False
          Just _ -> True
        epRoutes
          | null methods && not hasRaw = []
          | otherwise =
              let ms = methods ++ ["RAW" | hasRaw]
               in [RouteInfo (reverse prefix) ms allInfo]
        compRoutes =
          M.foldMapWithKey
            (\k sub -> go allInfo (PathLit k : prefix) sub)
            comps
        capRoutes = case caps of
          Nothing -> []
          Just cap -> case cap of
            L1 (Day arg sub _) ->
              go allInfo (PathCap (argName arg) : prefix) sub
            R1 (Day (MultiArg arg) epMap _) ->
              let ms = map methodStr (M.keys (epmGiven epMap))
                  hasRaw' = case epmRaw epMap of
                    Nothing -> False
                    Just _ -> True
                  ms' = ms ++ ["RAW" | hasRaw']
                  pat = reverse (PathCap (argName arg ++ "...") : prefix)
               in [RouteInfo pat ms' allInfo | not (null ms')]

-- | Result of matching a path against the PStruct tree.
data MatchOutcome a
  = -- | Path fully resolved to an endpoint, with collected info
    FullMatch [String] (EndpointMap a)
  | -- | Path is a prefix; here's the subtree
    PrefixMatch (PStruct a)
  | -- | No match
    NoMatch String

-- | Match a list of path segments against a 'PStruct'.
--
-- For literal segments, walks 'psComponents'.
-- For captures, runs the capture's 'argRead' via 'runReadM' and applies
-- the parsed value.
matchRoute :: PStruct a -> [String] -> MatchOutcome a
matchRoute root = go [] [] root True
  where
    go _matched acc ps _isRoot [] =
      let eps = psEndpoints ps
          hasMethods = not (M.null (epmGiven eps)) || case epmRaw eps of
            Nothing -> False
            Just _ -> True
          hasChildren =
            not (M.null (psComponents ps))
              || case psCaptures ps of
                Nothing -> False
                Just _ -> True
          allInfo = nub (acc ++ psInfo ps)
       in if hasMethods
            then FullMatch allInfo eps
            else
              if hasChildren
                then PrefixMatch ps
                else NoMatch "No endpoints or sub-paths found at this path"
    go matched acc ps isRoot (seg : rest) =
      let acc' = if isRoot then acc else nub (acc ++ psInfo ps)
       in case M.lookup seg (psComponents ps) of
            Just sub -> go (matched ++ [seg]) acc' sub False rest
            Nothing -> case psCaptures ps of
              Just (L1 (Day arg sub f)) ->
                case runReadM (argRead arg) seg of
                  Left err ->
                    NoMatch $
                      "Could not parse capture :"
                        ++ argName arg
                        ++ ": "
                        ++ err
                  Right val ->
                    go (matched ++ [seg]) acc' (fmap (\g -> f val g) sub) False rest
              Just (R1 (Day (MultiArg arg) epMap f)) ->
                -- CaptureAll: consume all remaining segments
                case traverse (runReadM (argRead arg)) (seg : rest) of
                  Left err ->
                    NoMatch $
                      "Could not parse capture :"
                        ++ argName arg
                        ++ ": "
                        ++ err
                  Right vals ->
                    FullMatch acc' (fmap (\g -> f vals g) epMap)
              Nothing ->
                let prefixSegs = map PathLit matched
                    availRoutes = enumerateRoutes ps
                    routeLines =
                      map
                        (\r -> formatRoute (prefixSegs ++ riPattern r))
                        availRoutes
                    prefixPath = formatRoute prefixSegs
                    availMsg = case routeLines of
                      [] -> ""
                      xs ->
                        "\n  Available routes:"
                          ++ concatMap (\x -> "\n    " ++ x) xs
                 in NoMatch $
                      "No match for \""
                        ++ seg
                        ++ "\" after "
                        ++ prefixPath
                        ++ availMsg

-- | Run a 'ReadM' parser on a single string value.
runReadM :: ReadM a -> String -> Either String a
runReadM rm s = case execParserPure defaultPrefs p [s] of
  Success a -> Right a
  Failure e -> Left (fst (renderFailure e ""))
  CompletionInvoked _ -> Left "completion invoked"
  where
    p = info (argument rm (metavar "X")) mempty

-- | Pre-process argv into (maybe method, path segments, extra args).
--
-- Handles:
--   * @-X METHOD@ for specifying HTTP method (curl-style)
--   * Path string split on @/@
--   * Query params from @?key=value@ in the path
--   * @-H "name: value"@ rewritten to @--header-name value@
preprocessArgs :: [String] -> (Maybe HTTP.Method, [String], [String])
preprocessArgs [] = (Nothing, [], [])
preprocessArgs args =
  let (pathStr, extraArgs) = case args of
        (p : xs)
          | "/" `isPrefixOf` p -> (p, xs)
          | not ("-" `isPrefixOf` p) -> ("/" ++ p, xs)
        _ -> ("", args)
      -- Split ?query from path
      (pathPart, queryPart) = break (== '?') pathStr
      -- Parse path segments
      pathSegs = filter (not . null) $ splitOn '/' pathPart
      -- Parse query string into --option value args
      queryArgs = case queryPart of
        ('?' : qs) -> concatMap queryToArg (splitQueryString qs)
        _ -> []
      -- Extract -X METHOD from extra args
      (mMethod, extraArgs') = extractMethod extraArgs
      -- Rewrite -H "name: value" into --header-name value
      rewrittenArgs = rewriteHeaders extraArgs'
   in (mMethod, pathSegs, queryArgs ++ rewrittenArgs)

-- | Extract @-X METHOD@ from a list of arguments.
extractMethod :: [String] -> (Maybe HTTP.Method, [String])
extractMethod [] = (Nothing, [])
extractMethod ("-X" : m : rest) = (Just (T.encodeUtf8 (T.pack m)), rest)
extractMethod (x : rest) =
  let (mMethod, rest') = extractMethod rest
   in (mMethod, x : rest')

-- | Split a string on a delimiter.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn d s =
  let (chunk, rest) = break (== d) s
   in chunk : case rest of
        [] -> []
        (_ : xs) -> splitOn d xs

-- | Split a query string on @&@.
splitQueryString :: String -> [(String, String)]
splitQueryString [] = []
splitQueryString qs =
  [ case break (== '=') kv of
      (k, '=' : v) -> (k, v)
      (k, _) -> (k, "")
    | kv <- splitOn '&' qs,
      not (null kv)
  ]

-- | Convert a query parameter to command-line arguments.
queryToArg :: (String, String) -> [String]
queryToArg (k, v)
  | null v = ["--" ++ k]
  | otherwise = ["--" ++ k, v]

-- | Rewrite @-H "name: value"@ to @--header-name value@.
rewriteHeaders :: [String] -> [String]
rewriteHeaders [] = []
rewriteHeaders ("-H" : hval : rest) =
  case break (== ':') hval of
    (name, ':' : val) ->
      let trimmed = dropWhile (== ' ') val
       in ("--header-" ++ map toLowerAscii name) : trimmed : rewriteHeaders rest
    _ -> "-H" : hval : rewriteHeaders rest
  where
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
rewriteHeaders (x : rest) = x : rewriteHeaders rest

-- | Convert an HTTP method to a display string.
methodStr :: HTTP.Method -> String
methodStr = T.unpack . T.decodeUtf8

-- | Format a route pattern for display.
formatRoute :: [PathSeg] -> String
formatRoute [] = "/"
formatRoute segs = concatMap fmt segs
  where
    fmt (PathLit s) = "/" ++ s
    fmt (PathCap s) = "/:" ++ s

-- | Extract @-v@/@--verbose@ from a list of arguments.
extractVerbose :: [String] -> (Bool, [String])
extractVerbose [] = (False, [])
extractVerbose ("-v" : rest) = (True, snd (extractVerbose rest))
extractVerbose ("--verbose" : rest) = (True, snd (extractVerbose rest))
extractVerbose (x : rest) =
  let (v, rest') = extractVerbose rest
   in (v, x : rest')

-- | Extract @--base-url URL@ from a list of arguments.
extractBaseUrl :: [String] -> (Maybe String, [String])
extractBaseUrl [] = (Nothing, [])
extractBaseUrl ("--base-url" : u : rest) =
  let (_, rest') = extractBaseUrl rest
   in (Just u, rest')
extractBaseUrl (x : rest) =
  let (mu, rest') = extractBaseUrl rest
   in (mu, x : rest')

-- | Resolve the base URL from CLI arg and optional default.
-- CLI arg takes priority over default. If neither is provided, error.
resolveBaseUrl :: Maybe BaseUrl -> Maybe String -> IO BaseUrl
resolveBaseUrl mDefault mCliArg = case mCliArg of
  Just s -> do
    result <- try (parseBaseUrl s) :: IO (Either SomeException BaseUrl)
    case result of
      Right url -> pure url
      Left err -> do
        hPutStrLn stderr $ "Error: invalid --base-url: " ++ show err
        exitFailure
  Nothing -> case mDefault of
    Just url -> pure url
    Nothing -> do
      hPutStrLn stderr "Error: --base-url is required (no default configured)"
      exitFailure

-- | Pretty-print a list of routes, one line per method.
printRoutes :: Bool -> [RouteInfo] -> IO ()
printRoutes verbose routes = mapM_ printRoute routes
  where
    printRoute RouteInfo {..} = do
      let path = formatRoute riPattern
      mapM_ (\m -> putStrLn $ "  " ++ m ++ "\t" ++ path) riMethods
      when (verbose && not (null riInfo)) $
        putStr $ concatMap (\s -> "    " ++ s ++ "\n") riInfo

-- | Print endpoint help (query params, headers, docs).
printEndpointHelp :: [PathSeg] -> HTTP.Method -> Endpoint a -> [String] -> IO ()
printEndpointHelp pat method ep epInfo = do
  let path = formatRoute pat
      mStr = methodStr method
  putStrLn $ "  " ++ mStr ++ " " ++ path
  case epInfo of
    [] -> pure ()
    infos ->
      putStr $ concatMap (\s -> "    " ++ s ++ "\n") infos
  -- Show options from the endpoint parser's help
  let parser = endpointToParser ep
      pinfo = info (parser <**> helper) (fullDesc <> header (mStr ++ " " ++ path))
  case execParserPure defaultPrefs pinfo ["--help"] of
    Failure e -> do
      let (helpText, _) = renderFailure e ""
          helpLines = lines helpText
          -- Filter to show just the options section
          optLines = dropWhile (not . isOptionLine) helpLines
      mapM_ (\l -> putStrLn $ "  " ++ l) optLines
    _ -> pure ()
  where
    isOptionLine l =
      let stripped = dropWhile (== ' ') l
       in "Available options:" `isPrefixOf` stripped
            || "--" `isPrefixOf` stripped
            || "-" `isPrefixOf` stripped

-- | Main entry point: flat curl-style parser for a 'PStruct'.
--
-- Takes an optional default 'BaseUrl'. If provided, @--base-url@ becomes
-- optional on the CLI; otherwise it is required.
-- Returns a pair of the resolved 'BaseUrl' and the parsed result.
flatStructParser :: Maybe BaseUrl -> PStruct a -> InfoMod a -> IO (BaseUrl, a)
flatStructParser mDefaultUrl ps _im = do
  argv <- getArgs
  let (verbose, argv1) = extractVerbose argv
      (mCliUrl, argv') = extractBaseUrl argv1
  case argv' of
    [] -> do
      printUsage verbose mDefaultUrl ps
      exitSuccess
    ["--help"] -> do
      printUsage verbose mDefaultUrl ps
      exitSuccess
    ["-h"] -> do
      printUsage verbose mDefaultUrl ps
      exitSuccess
    _ -> do
      baseUrl <- resolveBaseUrl mDefaultUrl mCliUrl
      let (mMethod, pathSegs, extraArgs) = preprocessArgs argv'
          mMethod' = mMethod <|> if "-d" `elem` extraArgs then Just (T.encodeUtf8 (T.pack "POST")) else Nothing
      case matchRoute ps pathSegs of
        NoMatch err -> do
          hPutStrLn stderr $ "Error: " ++ err
          exitFailure
        PrefixMatch sub -> do
          let routes = enumerateRoutes sub
              -- Prepend matched prefix to each route pattern
              prefixSegs = map PathLit pathSegs
              routes' = map (\r -> r {riPattern = prefixSegs ++ riPattern r}) routes
          if null routes'
            then do
              hPutStrLn stderr "No endpoints found at this path"
              exitFailure
            else do
              printRoutes verbose routes'
              exitSuccess
        FullMatch epInfo epMap -> do
          -- Check for --help / -h
          if "--help" `elem` extraArgs || "-h" `elem` extraArgs
            then do
              printEndpointMapHelp pathSegs epMap epInfo
              exitSuccess
            else do
              result <- runEndpoint mMethod' epMap extraArgs
              pure (baseUrl, result)

-- | Print usage: list all routes, followed by global options.
printUsage :: Bool -> Maybe BaseUrl -> PStruct a -> IO ()
printUsage verbose mDefaultUrl ps = do
  let routes = enumerateRoutes ps
  if null routes
    then putStrLn "No endpoints available"
    else printRoutes verbose routes
  printGlobalOptions mDefaultUrl

-- | Print the global options block.
printGlobalOptions :: Maybe BaseUrl -> IO ()
printGlobalOptions mDefaultUrl = do
  putStrLn ""
  putStrLn "Global options:"
  let urlDefault = case mDefaultUrl of
        Just url -> "(default: " ++ showBaseUrl url ++ ")"
        Nothing -> "(required)"
  putStrLn $ "  --base-url URL    Server base URL " ++ urlDefault
  putStrLn   "  -X METHOD         HTTP method (GET, POST, PUT, DELETE, etc.)"
  putStrLn   "  -v, --verbose     Show route descriptions"
  putStrLn   "  -H \"Name: Value\"  Set a request header"
  putStrLn   "  -h, --help        Show this help text"

-- | Print help for all methods in an endpoint map.
printEndpointMapHelp :: [String] -> EndpointMap a -> [String] -> IO ()
printEndpointMapHelp pathSegs epMap epInfo = do
  let pat = map PathLit pathSegs  -- approximate; captures already resolved
  mapM_ (\(m, ep) -> printEndpointHelp pat m ep epInfo) (M.toList (epmGiven epMap))

-- | Run an endpoint with the given method and remaining args.
runEndpoint :: Maybe HTTP.Method -> EndpointMap a -> [String] -> IO a
runEndpoint mMethod (EPM eps rw) extraArgs = do
  let methods = M.keys eps
  case mMethod of
    Just m ->
      case M.lookup m eps of
        Just ep -> runEndpointParser ep extraArgs
        Nothing ->
          case rw of
            Just rawEp -> do
              -- Raw endpoint: apply method
              let ep' = fmap ($ m) rawEp
              runEndpointParser ep' extraArgs
            Nothing -> do
              hPutStrLn stderr $
                "Method "
                  ++ methodStr m
                  ++ " not available. Available: "
                  ++ intercalate ", " (map (\x -> "-X " ++ methodStr x) methods)
              exitFailure
    Nothing ->
      case methods of
        [m] ->
          case M.lookup m eps of
            Just ep -> runEndpointParser ep extraArgs
            Nothing -> do
              hPutStrLn stderr "Internal error: method not found"
              exitFailure
        [] ->
          case rw of
            Just _ -> do
              hPutStrLn stderr "Raw endpoint requires explicit method (-X GET, -X POST, etc.)"
              exitFailure
            Nothing -> do
              hPutStrLn stderr "No endpoints at this path"
              exitFailure
        _ -> do
          hPutStrLn stderr $
            "Multiple methods available. Specify one: "
              ++ intercalate ", " (map (\x -> "-X " ++ methodStr x) methods)
          exitFailure

-- | Run an endpoint's option parser on the remaining args.
runEndpointParser :: Endpoint a -> [String] -> IO a
runEndpointParser ep args =
  let parser = endpointToParser ep
      pinfo = info (parser <**> helper) fullDesc
   in case execParserPure defaultPrefs pinfo args of
        Success a -> pure a
        Failure e -> do
          let (msg, _) = renderFailure e ""
          hPutStrLn stderr msg
          exitFailure
        CompletionInvoked _ -> do
          hPutStrLn stderr "Completion not supported in flat mode"
          exitFailure
