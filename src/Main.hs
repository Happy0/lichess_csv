module Main where
  import qualified Data.ByteString.Lazy as BL
  import Data.Aeson
  import Data.Conduit
  import qualified Data.List as L
  import qualified Data.HashMap.Strict as HM
  import qualified Data.Conduit.List as CL
  import qualified Data.Text as T
  import Web.Lichess.Conduit
  import Web.Lichess.Csv
  import Web.Lichess.File
  import Web.Lichess.Json
  import Web.Lichess.Request

  main :: IO ()
  main = do
    let userGamesConduit = userGames "happy0"
    headers <- getHeadersConduit userGamesConduit

    let valuesConduit = userGamesConduit =$= jsonToCSVConduit headers

    writeHeader "test.csv" headers
    appendCSVFileConduit "test.csv" valuesConduit
