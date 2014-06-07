import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import System.FilePath ((</>))
import Control.Concurrent
import qualified Data.Text as T
import System.Process

linkbotLabs :: FilePath -> IO ()
linkbotLabs directory = do
  (port, serverThread) <- forkServer directory
  browserThread <- forkIO $ startBrowser port
  yield

forkServer :: FilePath -> IO (Port, ThreadId)
forkServer dir = do
  port <- choosePort
  threadId <- forkIO $ startServer port dir
  return (port, threadId)

choosePort :: IO Port
choosePort = return 3000

startServer :: Port -> FilePath -> IO ()
startServer port dir = run port app
  where
    app :: Application -- aka Request -> IO Response
    app req =
      let path = foldl (</>) dir $ map T.unpack $ pathInfo req
      in
        return $ responseFile status200 [] path Nothing

startBrowser port = fmap (const ()) $ rawSystem "./barobobrowser" [url]
  where url = "http://localhost:" ++ show port ++ "/index.html"
