module Core.Types where

import Core.Import

-- external modules

import qualified Data.ByteString.Builder as B


data FaceMsg
    = FaceMsgVersion
    | DownMsg DownMsg
    | FaceUnknown ByteString
    deriving Show

data DownMsg
    = PRIVMSG Who ByteString ByteString
    | DownUnknown ByteString
    deriving Show

data Who = Who
    { whoNick :: ByteString
    , whoUser :: ByteString
    , whoHost :: ByteString
    }
    deriving Show

class Encode d where
    encode :: d -> Builder

data CoreMsg
    = UpMsg UpMsg
    | CoreMsgVersion
    deriving Show
instance Encode CoreMsg where
    encode (UpMsg d) = "UP " <> encode d
    encode CoreMsgVersion = "VERSION"

data UpMsg
    = UpPRIVMSG ByteString ByteString
    deriving Show
instance Encode UpMsg where
    encode (UpPRIVMSG who what) = fold
        ["PRIVMSG ", B.byteString who, " :", B.byteString what]
