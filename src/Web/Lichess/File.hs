module Web.Lichess.File (writeCSVFile) where

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

  writeCSVFile header filepath recordConduit = do
      writeHeader filepath header
      runResourceT $
        recordConduit =$=
        CL.map (BB.toByteString . CBuild.encodeRecord) =$=
        CC.intersperse newLine $$
        CB.sinkFile filepath
    where
      newLine = "\n"

  writeHeader :: FilePath -> C.Header -> IO ()
  writeHeader filepath header = do
    let csvHeader = BB.toLazyByteString (CBuild.encodeHeader header)
    B.writeFile filepath csvHeader
