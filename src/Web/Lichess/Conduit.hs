module Web.Lichess.Conduit (userGames) where

  import Data.Aeson
  import Data.Conduit
  import qualified Data.Conduit.List as L
  import Web.Lichess.Request

  userGames :: String -> Source IO Value
  userGames userName = L.unfoldM (getGamesPage userName) 1 =$= L.concat

  getGamesPage :: String -> Int -> IO (Maybe ([Value], Int))
  getGamesPage userName page = do
    gamesResult <- getUserGames userName page
    case gamesResult of
      Left err -> error err
      Right [] -> return Nothing
      Right games -> return (Just (games, succ page))
