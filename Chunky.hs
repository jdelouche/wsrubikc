{-# LANGUAGE OverloadedStrings #-}
module PrintMyChunks where

import Prelude
import qualified Data.ByteString as BS
import Rainbow
import Rainbow.Translate
import Data.Function ((&))
import Data.Text as T

myChunks :: [Chunk]
myChunks = [ (chunk "Roses") & fore red, chunk "\n",
             (chunk "Violets") & fore white, chunk "\n" ]

myPrintedChunks :: IO ()
myPrintedChunks = mapM_ BS.putStr
                . chunksToByteStrings toByteStringsColors256
                $ myChunks

myPrintedChunks' :: IO ()
myPrintedChunks' = do
  printer <- byteStringMakerFromEnvironment
  mapM_ BS.putStr
    . chunksToByteStrings printer
    $ myChunks
main = do
       mapM_ BS.putStr $ chunksToByteStrings toByteStringsColors256 $ fmap (\x -> (chunk (T.pack ((show x)++"*******\n"))) & (fore (color256 x))) [1..255]
       --print $ BS.concat $ chunksToByteStrings toByteStringsColors256 $ fmap (\x -> (chunk (T.pack ((show x)++"*******\n"))) & (fore (color256 x))) [130]
