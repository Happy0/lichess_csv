module Web.Lichess.Conduit (userGames,
                            tournamentPairings,
                            tournamentStandings) where

  import Data.Aeson
  import Data.Conduit
  import qualified Data.Conduit.List as L
  import Web.Lichess.Request

  userGames :: String -> Source IO Value
  userGames userName = L.unfoldM (getGamesPage userName) 1 =$= L.concat

  tournamentStandings :: String -> Source IO Value
  tournamentStandings tournamentId =
     L.unfoldM (getTournamentStandingsPage tournamentId) 1 =$= L.concat

  tournamentPairings :: String -> Source IO Value
  tournamentPairings tournamentId =
    L.unfoldM (getTournamentPairingsPage tournamentId) 1 =$= L.concat

  getGamesPage :: String -> Int -> IO (Maybe ([Value], Int))
  getGamesPage userName page = do
    gamesResult <- getUserGames userName page
    case gamesResult of
      Left jsonErr -> error jsonErr
      Right [] -> return Nothing
      Right games -> return (Just (games, succ page))

  getTournamentStandingsPage :: String -> Int -> IO (Maybe ([Value], Int))
  getTournamentStandingsPage tournamentId page = do
    standingsResult <- getTournamentStandings tournamentId page
    case standingsResult of
      Left jsonErr -> error jsonErr
      Right [] -> return Nothing
      Right standings -> return (Just (standings, succ page))

  getTournamentPairingsPage :: String -> Int -> IO (Maybe ([Value], Int))
  getTournamentPairingsPage tournamentId page = do
    pairingsResult <- getTournamentPairings tournamentId page
    case pairingsResult of
      Left jsonErr -> error jsonErr
      Right [] -> return Nothing
      Right standings -> return (Just (standings, succ page))
