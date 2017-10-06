module Yukikaze (main) where

import Yukikaze.Import

-- external modules

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T

-- local modules

import Yukikaze.IO
import Yukikaze.Types


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    upstream <- atomically newTChan
    downstream <- atomically newTChan
    _ <- forkIO $ receiver downstream
    _ <- forkIO $ sender upstream

    mainLoop upstream downstream


mainLoop :: TChan CoreMsg -> TChan FaceMsg -> IO a
mainLoop upstream downstream = do
    faceMsg <- atomically $ readTChan downstream
    maybe (return ()) (atomically . writeTChan upstream) $ simpleReact faceMsg
    mainLoop upstream downstream


simpleReact :: FaceMsg -> Maybe CoreMsg
simpleReact = \ case
    DownMsg d -> case d of
        PRIVMSG Who{..} place msg
            | "!ping" `B.isPrefixOf` msg -> let
                target = if B.head place == '#' then place else whoNick
              in Just $ UpMsg $ UpPRIVMSG target (whoNick <> ": pong!")
            | utf8 "!핑" `B.isPrefixOf` msg -> let
                target = if B.head place == '#' then place else whoNick
              in Just $ UpMsg $ UpPRIVMSG target (whoNick <> utf8 ": 퐁!")
            | otherwise -> Nothing
        _ -> Nothing
    _ -> Nothing
  where
    utf8 :: Text -> ByteString
    utf8 = T.encodeUtf8
