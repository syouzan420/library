module TeruFile (fileRead, fileWrite, fileRead2) where

import System.IO(FilePath, IOMode(..), openFile, hClose, hGetContents', hSetEncoding
                , utf8, hPutStr)
import Data.Text.Encoding (decodeUtf8)
import Data.Functor ((<&>))
import Data.Text (Text) 
import Prelude hiding (readFile)
import Data.ByteString(readFile)

fileRead :: FilePath -> IO String
fileRead fileName = do
  h <- openFile fileName ReadMode
  hSetEncoding h utf8
  hGetContents' h

fileWrite :: FilePath -> String -> IO ()
fileWrite fileName str = do
  h <- openFile fileName WriteMode
  hSetEncoding h utf8
  hPutStr h str
  hClose h

fileRead2 :: FilePath -> IO Text
fileRead2 fileName = readFile fileName <&> decodeUtf8
