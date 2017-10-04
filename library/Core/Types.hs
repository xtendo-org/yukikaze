module Core.Types where

import Core.Import

-- external modules

import qualified Data.ByteString.Builder as B


data FaceMsg
    = FaceMsgUnknown
    | FaceMsgVersion
    | DownMsg DownMsg
    deriving Show

data DownMsg
    = DownUnknown ByteString
    | PRIVMSG Who ByteString ByteString
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
    | CoreMsgRestart
    deriving Show
instance Encode CoreMsg where
    encode (UpMsg d) = "NET " <> encode d
    encode CoreMsgVersion = "VERSION"
    encode CoreMsgRestart = "RESTART"

data UpMsg
    = UpPRIVMSG ByteString ByteString
    deriving Show
instance Encode UpMsg where
    encode (UpPRIVMSG who what) = fold
        ["PRIVMSG ", B.byteString who, " :", B.byteString what]
