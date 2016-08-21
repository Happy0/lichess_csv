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
  getGamesPage userName page = getResourcePage getUserGames userName page

  getTournamentStandingsPage :: String -> Int -> IO (Maybe ([Value], Int))
  getTournamentStandingsPage tournamentId page =
     getResourcePage getTournamentStandings tournamentId page

  getTournamentPairingsPage :: String -> Int -> IO (Maybe ([Value], Int))
  getTournamentPairingsPage tournamentId page =
     getResourcePage getTournamentPairings tournamentId page

  getResourcePage :: (String -> Int -> IO (Either String [Value])) -> String -> Int -> IO (Maybe ([Value], Int))
  getResourcePage resourceGetter resource page = do
    result <- resourceGetter resource page
    case result of
      Left jsonErr -> error jsonErr
      Right [] -> return Nothing
      Right re -> return (Just (re, succ page))
