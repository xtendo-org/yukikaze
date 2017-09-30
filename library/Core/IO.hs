module Core.IO
    ( sender
    , receiver
    ) where

import Core.Import

-- external modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB


sender :: Chan LB.ByteString -> IO ()
sender chan = do
    payload <- (<> "\n") <$> readChan chan
    LB.putStr payload
    sender chan


receiver :: Chan ByteString -> IO a
receiver chan = receiver' ""
  where
    receiver' buffer = do
        (former, latter) <- loopTillRN buffer
        writeChan chan former
        receiver' latter
    loopTillRN buffer
        | B.null latter = do
            received <- get
            loopTillRN (buffer <> received)
        | otherwise = return
            ( B.take (B.length former + B.length sep) buffer
            , B.drop (B.length sep) latter
            )
      where
        (former, latter) = B.breakSubstring sep buffer
        sep = "\n"
    get :: IO ByteString
    get = B.hGet stdin 4096
