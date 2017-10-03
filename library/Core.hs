module Core (main) where

import Core.Import

-- external modules

import qualified Data.ByteString as B

-- local modules

import Core.IO
import Core.Types


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    upstream <- newChan
    downstream <- newChan
    _ <- forkIO $ receiver downstream
    _ <- forkIO $ sender upstream

    mainLoop upstream downstream


mainLoop :: Chan CoreMsg -> Chan FaceMsg -> IO a
mainLoop upstream downstream = do
    faceMsg <- readChan downstream
    maybe (return ()) (writeChan upstream) $ simpleReact faceMsg
    mainLoop upstream downstream


simpleReact :: FaceMsg -> Maybe CoreMsg
simpleReact = \ case
    DownMsg d -> case d of
        PRIVMSG Who{..} target msg -> if "!ping" `B.isPrefixOf` msg
            then Just $ UpMsg $ UpPRIVMSG target (whoNick <> ", pong!")
            else Nothing
        _ -> Nothing
    _ -> Nothing
