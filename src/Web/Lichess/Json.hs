module Web.Lichess.Json (flattenValue, getKeys) where

import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Csv
import qualified Data.HashMap.Strict as Hm
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T

instance ToRecord Value where
  toRecord blah = undefined

getKeys :: Value -> S.Set T.Text
getKeys (Object o) = S.fromList (Hm.keys o)
getKeys _ = S.empty

flattenValue :: Value -> Value
flattenValue value =
  let keyValues = flatten [] value
  in object keyValues

flatten :: [T.Text] -> Value -> [Pair]
flatten parentKeys (Object o) =
  do
    (key, value) <- Hm.toList o
    pairs <- flatten (parentKeys ++ [key]) value
    return pairs

flatten keys@(parentKey : rest) value = [(concatKeys keys) A..= value]
flatten _ _ = error "I dun goofed."

concatKeys :: [T.Text] -> T.Text
concatKeys keys = T.concat (intersperse "_" keys)
