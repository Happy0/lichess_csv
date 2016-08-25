module Web.Lichess.Csv (jsonToCSV) where

  import qualified Data.Aeson as A
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Char8 as C8
  import qualified Data.Csv as C
  import qualified Data.HashMap.Strict as HM
  import qualified Data.Text as T
  import qualified Data.Text.Encoding as E

  jsonToCSV :: C.Header -> A.Value -> C.Record
  jsonToCSV header (A.Object o) =
    fmap (textValueOrEmpty . flip HM.lookup o . E.decodeUtf8) header
    where
      textValueOrEmpty :: Maybe A.Value -> B.ByteString
      textValueOrEmpty (Just v) = valueToText v
      textValueOrEmpty _ = ""


  jsonToCSV _ _ = error "Expected a JSON object to convert to CSV"


  valueToText :: A.Value -> B.ByteString
  valueToText value = C8.pack (show value)
