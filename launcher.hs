import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Status
import System.FilePath ((</>))
import Control.Concurrent
import qualified Data.Text as T
import System.Process
import System.Environment
import System.Exit
import qualified LauncherConfig as Config

linkbotLabs :: FilePath -> IO ()
linkbotLabs directory = do
  (port, serverThread) <- forkServer directory
  browserThread <- forkIO $ startBrowser port
  yield

forkServer :: FilePath -> IO (Warp.Port, ThreadId)
forkServer dir = do
  port <- choosePort
  threadId <- forkIO $ startServer port dir
  return (port, threadId)

choosePort :: IO Warp.Port
choosePort = return Config.port

startServer :: Warp.Port -> FilePath -> IO ()
startServer port dir = Warp.run port app
  where
    app :: Application -- aka Request -> (Reponse -> IO ReponseReceived) -> IO ResponseReceived
    app req func =
      let path = foldl (</>) dir $ map T.unpack $ pathInfo req
      in
        func $ responseFile status200 [] path Nothing

startBrowser port = do
    exitCode <- rawSystem Config.browser_bin_location [url]
    if exitCode == ExitSuccess 
        then 
            return ()
        else 
            putStrLn $ "Error: Could not find browser at " ++ Config.browser_bin_location
  where url = "http://localhost:" ++ show port ++ "/index.html"

--fmap (const ()) $ rawSystem config.browser_bin_location [url]
  --where url = "http://localhost:" ++ show port ++ "/index.html"

main = do
    args <- getArgs
    linkbotLabs $ head args
