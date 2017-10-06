module Yukikaze.Import
    ( module Module
    , Builder
    , ByteString
    , Text
    , for
    ) where

-- base

import Control.Applicative as Module
import Control.Concurrent as Module
import Control.Exception as Module
import Control.Monad as Module
import Data.Char as Module
import Data.Foldable as Module
import Data.Maybe as Module
import Data.Monoid as Module
import Data.Word as Module
import System.IO as Module

-- data types

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)


for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap
