module Web.Lichess.Conduit (getHeadersConduit,
                            userGames,
                            tournamentPairings,
                            tournamentStandings) where

  import Data.Aeson
  import Data.Conduit
  import qualified Data.Csv as C
  import qualified Data.Conduit.List as L
  import qualified Data.Set as S
  import qualified Data.Text as T
  import qualified Data.Text.Encoding as TE
  import qualified Data.Vector as V
  import Web.Lichess.Json
  import Web.Lichess.Request

  {-
    Helper function to work out all the valid headers for a given endpoint
  -}
  getHeadersConduit :: Monad m => Conduit () m Value -> m C.Header
  getHeadersConduit conduit =
    do
      headers <- conduit =$=
        L.isolate 500 =$=
        L.map flattenValue =$=
        L.map getKeys $$
        L.fold S.union S.empty

      let encodedHeaders = fmap TE.encodeUtf8 (S.toList headers)

      return ((V.fromList) encodedHeaders)

  userGames :: String -> Source IO Value
  userGames userName =
    L.unfoldM (getGamesPage userName) 1 =$= L.concat

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
