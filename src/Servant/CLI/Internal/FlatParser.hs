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
  )
where

import Data.Functor.Combinator (Day (..))
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics ((:+:) (..))
import qualified Network.HTTP.Types as HTTP
import Options.Applicative
import Servant.CLI.Internal.PStruct
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
enumerateRoutes = go []
  where
    go :: [PathSeg] -> PStruct a -> [RouteInfo]
    go prefix (PStruct infos comps caps eps) =
      epRoutes ++ compRoutes ++ capRoutes
      where
        methods = map methodStr (M.keys (epmGiven eps))
        hasRaw = case epmRaw eps of
          Nothing -> False
          Just _ -> True
        epRoutes
          | null methods && not hasRaw = []
          | otherwise =
              let ms = methods ++ ["RAW" | hasRaw]
               in [RouteInfo (reverse prefix) ms infos]
        compRoutes =
          M.foldMapWithKey
            (\k sub -> go (PathLit k : prefix) sub)
            comps
        capRoutes = case caps of
          Nothing -> []
          Just cap -> case cap of
            L1 (Day arg sub _) ->
              go (PathCap (argName arg) : prefix) sub
            R1 (Day (MultiArg arg) epMap _) ->
              let ms = map methodStr (M.keys (epmGiven epMap))
                  hasRaw' = case epmRaw epMap of
                    Nothing -> False
                    Just _ -> True
                  ms' = ms ++ ["RAW" | hasRaw']
                  pat = reverse (PathCap (argName arg ++ "...") : prefix)
               in [RouteInfo pat ms' [] | not (null ms')]

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
          allInfo = acc ++ psInfo ps
       in if hasMethods
            then FullMatch allInfo eps
            else
              if hasChildren
                then PrefixMatch ps
                else NoMatch "No endpoints or sub-paths found at this path"
    go matched acc ps isRoot (seg : rest) =
      -- Only accumulate info from non-root nodes (root has merged info from all :<|> branches)
      let acc' = if isRoot then acc else acc ++ psInfo ps
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

-- | Pretty-print a list of routes, one line per method.
printRoutes :: [RouteInfo] -> IO ()
printRoutes routes = mapM_ printRoute routes
  where
    printRoute RouteInfo {..} =
      let path = formatRoute riPattern
       in mapM_ (\m -> putStrLn $ "  " ++ m ++ "\t" ++ path) riMethods

-- | Print endpoint help (query params, headers, docs).
printEndpointHelp :: [PathSeg] -> HTTP.Method -> Endpoint a -> [String] -> IO ()
printEndpointHelp pat method ep epInfo = do
  let path = formatRoute pat
      mStr = methodStr method
  putStrLn $ "  " ++ mStr ++ " " ++ path
  case epInfo of
    [] -> pure ()
    infos -> do
      let desc = intercalate " — " infos
      putStrLn $ "    " ++ desc
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
flatStructParser :: PStruct a -> InfoMod a -> IO a
flatStructParser ps _im = do
  argv <- getArgs
  case argv of
    [] -> do
      printUsage ps
      exitSuccess
    ["--help"] -> do
      printUsage ps
      exitSuccess
    _ -> do
      let (mMethod, pathSegs, extraArgs) = preprocessArgs argv
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
              printRoutes routes'
              exitSuccess
        FullMatch epInfo epMap -> do
          -- Check for --help
          if "--help" `elem` extraArgs
            then do
              printEndpointMapHelp pathSegs epMap epInfo
              exitSuccess
            else
              runEndpoint mMethod' epMap extraArgs

-- | Print usage: list all routes.
printUsage :: PStruct a -> IO ()
printUsage ps = do
  let routes = enumerateRoutes ps
  if null routes
    then putStrLn "No endpoints available"
    else printRoutes routes

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
