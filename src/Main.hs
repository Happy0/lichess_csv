module Main where
  import qualified Data.ByteString.Lazy as BL
  import Data.Aeson
  import Data.Conduit
  import qualified Data.List as L
  import qualified Data.HashMap.Strict as HM
  import qualified Data.Conduit.List as CL
  import qualified Data.Set as S
  import qualified Data.Text as T
  import Web.Lichess.Conduit
  import Web.Lichess.Csv
  import Web.Lichess.Json
  import Web.Lichess.Request

  main :: IO ()
  main = do
    let userGamesConduit = userGames "happy0"
    processHeaders userGamesConduit

  getKeys :: Value -> S.Set T.Text
  getKeys (Object o) = S.fromList (HM.keys o)
  getKeys _ = S.empty

  {-
    Helper function to work out all the valid headers for the CSV output
  -}
  processHeaders conduit = do
    headers <- conduit =$= CL.isolate 200 =$= CL.map flattenValue =$= CL.map getKeys $$ CL.fold S.union S.empty
    let headersList = S.toList headers
    print headersList
