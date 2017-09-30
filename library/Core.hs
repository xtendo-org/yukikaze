module Core (main) where

import Core.Import

-- external modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB

-- local modules

import Core.IO
import Core.Parser
import Core.Types


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    upstream <- newChan
    downstream <- newChan
    _ <- forkIO $ receiver downstream
    _ <- forkIO $ sender upstream

    mainLoop upstream downstream


mainLoop :: Chan LB.ByteString -> Chan ByteString -> IO a
mainLoop upstream downstream = do
    msg <- readChan downstream
    case parseOnly faceParser msg of
        Left _ -> LB.putStr "Parse fail"
        Right parsed -> let
            write = writeChan upstream . B.toLazyByteString . encode
          in maybe (return ()) write $ simpleReact parsed
    mainLoop upstream downstream


simpleReact :: FaceMsg -> Maybe CoreMsg
simpleReact = \ case
    DownMsg d -> case d of
        PRIVMSG Who{..} target msg -> if "!ping" `B.isPrefixOf` msg
            then Just $ UpMsg $ UpPRIVMSG target (whoNick <> ", " <> msg)
            else Nothing
        _ -> Nothing
    _ -> Nothing
