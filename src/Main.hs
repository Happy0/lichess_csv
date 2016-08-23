module Main where
  import qualified Data.ByteString.Lazy as BL
  import Data.Aeson
  import Data.Conduit
  import qualified Data.Conduit.List as CL
  import Web.Lichess.Conduit
  import Web.Lichess.Json
  import Web.Lichess.Request

  main :: IO ()
  main = do
    userGames "happy0" =$= CL.isolate 1 =$= CL.map flattenValue $$ CL.mapM_ (BL.putStrLn . encode)
