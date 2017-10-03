module Core.Parser
    ( P.parseOnly
    , faceParser
    ) where

import Core.Import

-- external modules

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P

-- local modules

import Core.Types


faceParser :: Parser FaceMsg
faceParser = P.choice [version, faceNet]
  where
    version = P.string "VERSION" *> return FaceMsgVersion
    faceNet = P.string "NET " *> (DownMsg <$> net)


net :: Parser DownMsg
net = P.choice [privmsg, unknown]
  where
    privmsg = do
        _ <- P.char8 ':' <|> return 0
        nick <- takeTillChar '!'
        user <- takeTillChar '@'
        host <- takeTillChar ' '
        _ <- P.string "PRIVMSG "
        target <- takeTillChar ' '
        _ <- P.char8 ':' <|> return 0
        msg <- P.takeByteString
        return $ PRIVMSG (Who nick user host) target msg
    unknown = DownUnknown <$> P.takeByteString


takeTillChar :: Char -> Parser ByteString
takeTillChar c = P.takeTill (== c) <* P.char8 c
