module Web.Lichess.Conduit (
                            getHeaders,
                            userGames,
                            tournamentPairings,
                            tournamentStandings) where

  import Data.Aeson
  import Data.Conduit
  import qualified Data.Conduit.List as L
  import qualified Data.Set as S
  import qualified Data.Text as T
  import Web.Lichess.Json
  import Web.Lichess.Request

  {-
    Helper function to work out all the valid headers for a given endpoint
  -}
  getHeaders :: Monad m => Conduit () m Value -> m (S.Set T.Text)
  getHeaders conduit =
      conduit =$=
      L.isolate 500 =$=
      L.map flattenValue =$=
      L.map getKeys $$
      L.fold S.union S.empty

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

  getResourcePage :: (String -> Int -> IO (Either String [Value]))
                     -> String
                     -> Int
                     -> IO (Maybe ([Value], Int))
  getResourcePage resourceGetter resource page = do
    result <- resourceGetter resource page
    case result of
      Left jsonErr -> error jsonErr
      Right [] -> return Nothing
      Right re -> return (Just (re, succ page))
