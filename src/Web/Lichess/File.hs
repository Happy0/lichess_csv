module Web.Lichess.File (writeCSVFile) where

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

  writeCSVFile :: C.Header -> FilePath -> Source IO C.Record -> IO ()
  writeCSVFile header filepath recordConduit = do
    withFile filepath WriteMode $ \handle -> do
      writeHeader filepath header handle

      -- TODO: Learn how to use CB.sinkFile. Jeez, y so difficult?
      recordConduit =$=
        CL.map (BB.toByteString . CBuild.encodeRecord) =$=
        CC.intersperse newLine $$
        CL.mapM_ (BS.hPut handle)

  writeHeader :: FilePath -> C.Header -> Handle -> IO ()
  writeHeader filepath header handle = do
    let csvHeader = BB.toLazyByteString (CBuild.encodeHeader header)
    B.hPut handle csvHeader

  newLine = "\n"
