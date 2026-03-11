{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

import           Control.Concurrent
import           Control.Exception (bracket)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Proxy
import           Data.Text                (Text)
import           Data.Vinyl               (Rec(RNil, (:&)))
import           GHC.Generics
import           Network.HTTP.Client      (newManager, defaultManagerSettings)
import           Network.HTTP.Types.Status (statusCode, statusMessage)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (header, progDesc)
import           Servant.API
import           Servant.CLI
import           Servant.Client
import           Servant.Server
import           System.Exit               (exitFailure)
import           System.IO                (hPutStrLn, stderr)
import           System.Random
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map                 as M
import qualified Data.Text                as T


-- * Example

-- | A greet message data type
newtype Greet = Greet Text
  deriving (Generic, Show)

instance ParseBody Greet where
    parseBody = Greet <$> parseBody

-- | We can get JSON support automatically. This will be used to parse
-- and encode a Greeting as 'JSON'.
instance FromJSON Greet
instance ToJSON Greet

-- We add some useful annotations to our captures,
-- query parameters and request body to make the docs
-- really helpful.
instance ToCapture (Capture "name" Text) where
    toCapture _ = DocCapture "name" "name of the person to greet"

instance ToParam (QueryParam "capital" Bool) where
    toParam _ =
      DocQueryParam "capital"
                    ["true", "false"]
                    "Get the greeting message in uppercase (true) or not (false). Default is false."
                    Normal

instance ToAuthInfo (BasicAuth "login" Int) where
    toAuthInfo _ =
      DocAuthentication "Login credientials"
                        "Username and password"

type TestApi =
        Summary "Send a greeting"
           :> "hello"
           :> Capture "name" Text
           :> QueryParam "capital" Bool
           :> Get '[JSON] Greet
   :<|> Summary "Greet utilities"
           :> "greet"
           :> ReqBody '[JSON] Greet
           :> ( Get  '[JSON] Int
           :<|> BasicAuth "login" Int
             :> Post '[JSON] NoContent
              )
   :<|> Summary "List users"
           :> "auth"
           :> "listUsers"
           :> Header "authorization" Text
           :> Get '[JSON] [Text]
   :<|> Summary "Deep paths test"
           :> "dig"
           :> "down"
           :> "deep"
           :> Summary "Almost there"
           :> Capture "name" Text
           :> "more"
           :> Summary "We made it"
           :> Get '[JSON] Text


testApi :: Proxy TestApi
testApi = Proxy

server :: Application
server = serveWithContext testApi (authCheck :. EmptyContext) $
        (\t b -> pure . Greet $ "Hello, "
              <> if fromMaybe False b
                    then T.toUpper t
                    else t
        )
   :<|> (\(Greet g) -> pure (T.length g)
                  :<|> (\_ -> pure NoContent)
        )
   :<|> (\case Just "hunter2" -> pure ["john", "hunter"]
               _              -> throwError err401
        )
   :<|> (pure . T.reverse)
  where
    -- | Map of valid users and passwords
    userMap = M.fromList [("alice", "password"), ("bob", "hunter2")]
    authCheck = BasicAuthCheck $ \(BasicAuthData u p) ->
      case M.lookup u userMap of
        Nothing -> pure NoSuchUser
        Just p'
          | p == p'   -> Authorized <$> randomIO @Int
          | otherwise -> pure BadPassword

-- | Safely shutdown the server when we're done
withServer :: IO () -> IO ()
withServer action =
  bracket (forkIO $ run 8081 server)
    killThread
    (const action)

main :: IO ()
main = do
    c <- parseClientPrettyFlatWithContext
                    testApi
                    (Proxy :: Proxy ClientM)
                    (getPwd :& RNil)
                    cinfo

    withServer $ do

        manager' <- newManager defaultManagerSettings
        res      <- runClientM c (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))

        case res of
          Left (FailureResponse _ resp) -> do
            let status = responseStatusCode resp
                code   = statusCode status
                reason = BS.unpack $ statusMessage status
                body   = BSL.unpack $ responseBody resp
            hPutStrLn stderr $ "Error: " ++ show code ++ " " ++ reason
                ++ if null body then "" else " — " ++ body
            exitFailure
          Left (ConnectionError e) -> do
            hPutStrLn stderr $ "Connection error: " ++ show e
            exitFailure
          Left e -> do
            hPutStrLn stderr $ "Error: " ++ show e
            exitFailure
          Right bs -> BSL.putStrLn bs
  where
    cinfo = header "greet" <> progDesc "Greet API"
    getPwd :: ContextFor ClientM (BasicAuth "login" Int)
    getPwd = GenBasicAuthData . liftIO $ do
      putStrLn "Authentication needed for this action!"
      putStrLn "(Hint: try 'bob' and 'hunter2')"
      putStrLn "Enter username:"
      n <- BS.getLine
      putStrLn "Enter password:"
      p <- BS.getLine
      pure $ BasicAuthData n p
