module Web.Lichess.Csv (jsonToCSV) where

  import qualified Data.Aeson as A
  import qualified Data.Csv as C
  import qualified Data.Text as T

  jsonToCSV :: C.Header -> A.Value -> C.Record
  jsonToCSV header jsonValue = undefined

  valueToText :: A.Value -> T.Text
  valueToText value = T.pack (show value)
