module Web.Lichess.File (writeCSVFile) where

  import qualified Data.ByteString as B
  import qualified Data.Csv as C
  import qualified Data.Csv.Incremental as CI
  import Data.Conduit
  import qualified Data.Conduit.Binary as CB
  import qualified Data.Conduit.List as CL
  import qualified Data.List as L
  import qualified Data.Vector as V
  import System.IO

  writeCSVFile :: Monad m => C.Header -> FilePath -> Conduit () m C.Record -> IO ()
  writeCSVFile header filepath recordConduit =
    do
      writeHeader filepath header
      conduit =$= CL. map CL.encode $$ CB.sinkFile filepath

  writeHeader :: FilePath -> C.Header -> IO ()
  writeHeader filepath header = B.writeFile filepath (makeHeaderString header)

  makeHeaderString :: C.Header -> B.ByteString
  makeHeaderString header =
    let headerList = V.toList header
    in B.concat (L.intersperse "," headerList)
