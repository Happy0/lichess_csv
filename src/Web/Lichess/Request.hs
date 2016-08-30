module Web.Lichess.Request (Page,
                            getTournament,
                            getTournaments,
                            getTournamentPairings,
                            getTournamentStandings,
                            getUser,
                            getUserGames) where

  import Control.Concurrent
  import Control.Lens
  import Control.Monad
  import Data.Aeson (encode, Value)
  import Data.Aeson.Lens (key, nth)
  import qualified Data.Aeson.Types as T
  import qualified Data.HashMap.Strict as Hm
  import qualified Data.Text as Te
  import qualified Data.Map as M
  import qualified Data.Vector as V
  import Network.Wreq
  import Web.Lichess.Json

  type Page = Int

  getTournaments :: IO (Either String [Value])
  getTournaments =
    let url = "http://en.lichess.org/api/tournament"
    in parseTournamentsResponse <$> makeRequest url Nothing
    where
      parseTournamentsResponse resp =
        let created = resp ^? key "created"
        in case created of
          (Just (T.Array a)) -> Right (fmap flattenValue (V.toList a))
          _ -> Left "Expected tournament JSON array"

  getTournament :: String -> IO Value
  getTournament tournamentId =
    let url = "http://en.lichess.org/api/tournament/" ++ tournamentId
    in flattenValue <$> makeRequest url Nothing

  getTournamentStandings :: String -> Page -> IO (Either String [Value])
  getTournamentStandings tournamentId page = do
    let url = "http://en.lichess.org/api/tournament/" ++ tournamentId
    let options = paginatorOptions page

    response <- makeRequest url (Just options)
    let standings = response ^? key "standing" . key "players"

    return $
      case standings of
        Just (T.Array a) ->
          Right (fmap (addTournamentId . flattenValue) (V.toList a))
        _ -> Left "Expected pairings array"

    where
      addTournamentId :: Value -> Value
      addTournamentId (T.Object o) =
        T.Object (Hm.insert "tournamentId" (T.String (Te.pack tournamentId)) o)
      addTournameId _ = error "i dun goofed (tournament standings)"

  getTournamentPairings :: String -> Page -> IO (Either String [Value])
  getTournamentPairings tournamentId page = do
    let url = "http://en.lichess.org/api/tournament/" ++ tournamentId
    let options = paginatorOptions page
    response <- makeRequest url (Just options)
    let pairings = response ^? key "pairings"

    return $
      case pairings of
        Just (T.Array a) -> sequence (V.toList (fmap pairingToObj a))
        _ -> Left "Expected pairings array"
    where
      pairingToObj :: Value -> Either String Value
      pairingToObj pairing =
        let whitePlayer = (pairing ^? key "u" . nth 0) :: Maybe Value
        in let blackPlayer = (pairing ^? key "u" . nth 1) :: Maybe Value

        in case (whitePlayer, blackPlayer) of
          (Just w, Just b) ->
            let whitePair = ("white_player", w)
            in let blackPair = ("black_player", b)
            in let tournamentIdPair = ("tournament_id", T.String (Te.pack tournamentId))
            in Right (T.object [whitePair, blackPair, tournamentIdPair])
          _ -> Left "Expected white and black player in pairing"

  getUser :: String -> IO Value
  getUser userName =
    let url = "http://en.lichess.org/api/user/" ++ userName
    in flattenValue <$> makeRequest url Nothing

  getUserGames :: String -> Page -> IO (Either String [Value])
  getUserGames userName page = do
    let options = paginatorOptions page & param "nb" .~ ["100"]
                    & param "with_moves" .~ ["1"] & param "with_opening" .~ ["1"]
    let url = "http://en.lichess.org/api/user/" ++ userName ++ "/games"
    r <- makeRequest url (Just options)

    let games = r ^? key "currentPageResults"

    return $
      case games of
        Just (T.Array a) -> Right (fmap flattenValue (V.toList a))
        _ -> Left "Expected games array"

  paginatorOptions :: Page -> Options
  paginatorOptions page = defaults & param "page" .~ [Te.pack (show page)]

  {-
    Makes request with a given URL and options, waits 1 second (to obey the
    lichess rate limit instructions) and then returns the results
  -}
  makeRequest :: String -> Maybe Options -> IO Value
  makeRequest url options =
    case options of
      Nothing ->
        let requestAction = get url
        in makeRequest requestAction
      (Just o) ->
        let requestAction = getWith o url
        in makeRequest requestAction

    where
      makeRequest request = do
        r <- asJSON =<< request
        threadDelay 1000000
        return (r ^. responseBody)
