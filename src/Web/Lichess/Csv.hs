module Web.Lichess.Csv (jsonToCSVConduit, jsonToCSV) where

  import qualified Data.Aeson as A
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Char8 as C8
  import qualified Data.Csv as C
  import Data.Conduit
  import qualified Data.Conduit.List as CL
  import qualified Data.HashMap.Strict as HM
  import qualified Data.Text as T
  import qualified Data.Text.Encoding as E

  jsonToCSVConduit :: Monad m => C.Header -> Conduit A.Value m C.Record
  jsonToCSVConduit header = CL.map (jsonToCSV header)

  jsonToCSV :: C.Header -> A.Value -> C.Record
  jsonToCSV header (A.Object o) =
    fmap (textValueOrEmpty . flip HM.lookup o . E.decodeUtf8) header
    where
      textValueOrEmpty :: Maybe A.Value -> B.ByteString
      textValueOrEmpty (Just v) = valueToText v
      textValueOrEmpty _ = ""
  jsonToCSV _ _ = error "Expected a JSON object to convert to CSV"

  valueToText :: A.Value -> B.ByteString
  valueToText val =
    case val of
      (A.String t) -> E.encodeUtf8 t
      (A.Bool b) -> C8.pack (show b)
      (A.Number num) -> C8.pack (show num)
      _ -> ""
