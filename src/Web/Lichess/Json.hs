module Web.Lichess.Json (flattenValue) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as Hm
import Data.List
import qualified Data.Text as T

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

flatten keys@(parentKey : rest) value = [(concatKeys keys) .= value]
flatten _ _ = error "I dun goofed."

concatKeys :: [T.Text] -> T.Text
concatKeys keys = T.concat (intersperse "_" keys)
