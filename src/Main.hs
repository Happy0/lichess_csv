module Main where
  import qualified Data.ByteString.Lazy as BL
  import Data.Aeson
  import Data.Conduit
  import Data.Csv
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
    userHeaders <- startUserCSV
    gameHeaders <- startGamesCSV
    tournamentHeaders <- startTournamentsCSV
    tournamentStandingsHeaders <- startTournamentStandingsCSV
    tournamentPairingsHeaders <- startTournamentPairingsCSV
    return ()

  startUserCSV :: IO Header
  startUserCSV = do
    userExample <- getUser "happy0"
    let userHeaders = getHeaders userExample
    writeHeader "user.csv" userHeaders
    return userHeaders

  startGamesCSV :: IO Header
  startGamesCSV = do
    let gamesConduit = userGames "happy0"
    headers <- getHeadersConduit gamesConduit
    writeHeader "games.csv" headers
    return headers

  startTournamentsCSV :: IO Header
  startTournamentsCSV = do
    tours <- tournamentsConduit
    headers <- getHeadersConduit tours
    writeHeader "tournaments.csv" headers
    return headers

  startTournamentStandingsCSV :: IO Header
  startTournamentStandingsCSV = do
    let tournamentStandingsConduit = tournamentStandings "yUEpyTE9"
    headers <- getHeadersConduit tournamentStandingsConduit
    writeHeader "tournament_standings.csv" headers
    return headers

  startTournamentPairingsCSV :: IO Header
  startTournamentPairingsCSV = do
    let tournamentStandingsConduit = tournamentPairings "yUEpyTE9"
    headers <- getHeadersConduit tournamentStandingsConduit
    writeHeader "tournament_pairings.csv" headers
    return headers

  {- main :: IO ()
  main = do
    let userGamesConduit = userGames "happy0"
    headers <- getHeadersConduit userGamesConduit

    let valuesConduit = userGamesConduit =$= jsonToCSVConduit headers

    writeHeader "test.csv" headers
    appendCSVFileConduit "test.csv" valuesConduit -}
