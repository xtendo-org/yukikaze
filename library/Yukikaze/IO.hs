module Yukikaze.IO
    ( sender
    , receiver
    ) where

import Yukikaze.Import

-- external modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB

-- local modules

import Yukikaze.Constants
import Yukikaze.Parser
import Yukikaze.Types


sender :: Chan CoreMsg -> IO ()
sender chan = do
    payload <- readChan chan
    LB.putStr (B.toLazyByteString (encode payload <> B.byteString cSep))
    sender chan


receiver :: Chan FaceMsg -> IO a
receiver chan = receiver' ""
  where
    receiver' buffer = do
        (former, latter) <- loopTillSep buffer
        writeChan chan $
            either (const FaceMsgUnknown) id $ parseOnly faceParser former
        receiver' latter
    loopTillSep buffer
        | B.null latter = do
            received <- get
            loopTillSep (buffer <> received)
        | otherwise = return
            ( B.take (B.length former + B.length cSep) buffer
            , B.drop (B.length cSep) latter
            )
      where
        (former, latter) = B.breakSubstring cSep buffer
    get :: IO ByteString
    get = B.hGetSome stdin cBufSize
