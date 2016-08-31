module Web.Lichess.File (writeHeader,
                         appendCSVFile,
                         appendCSVFileConduit) where

  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Lazy as B
  import qualified Blaze.ByteString.Builder as BB
  import Control.Monad.Trans.Resource
  import qualified Data.Csv as C
  import qualified Data.Csv.Builder as CBuild
  import Data.Conduit
  import qualified Data.Conduit.Combinators as CC
  import qualified Data.Conduit.Binary as CB
  import qualified Data.Conduit.List as CL
  import qualified Data.List as L
  import qualified Data.Vector as V
  import System.IO

  writeHeader :: FilePath -> C.Header -> IO ()
  writeHeader filepath header =
    withFile filepath WriteMode $ \handle -> do
      let csvHeader = BB.toLazyByteString (CBuild.encodeHeader header)
      B.hPut handle csvHeader

  appendCSVFileConduit :: FilePath -> Source IO C.Record -> IO ()
  appendCSVFileConduit filepath recordConduit =
    withFile filepath AppendMode $ \handle -> do

      -- TODO: Learn how to use CB.sinkFile. Jeez, y so difficult?
      recordConduit =$=
        CL.map (BB.toByteString . CBuild.encodeRecord) $$
        CL.mapM_ (BS.hPut handle)

  appendCSVFile :: FilePath -> C.Record -> IO ()
  appendCSVFile filepath record =
    withFile filepath AppendMode $ \handle ->
      let encodedRecord = CBuild.encodeRecord record
      in BS.hPut handle (BB.toByteString encodedRecord)
