module Yukikaze (main) where

import Yukikaze.Import

-- external modules

import qualified Data.ByteString.Char8 as B

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
            | "!핑" `B.isPrefixOf` msg -> let
                target = if B.head place == '#' then place else whoNick
              in Just $ UpMsg $ UpPRIVMSG target (whoNick <> ": 퐁!")
            | otherwise -> Nothing
        _ -> Nothing
    _ -> Nothing
