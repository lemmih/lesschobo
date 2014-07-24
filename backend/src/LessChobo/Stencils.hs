{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module LessChobo.Stencils
    ( StencilId
    , Chinese
    , Pinyin
    , English
    , Stencil(..)
    , features
    , schedule
    ) where

import           LessChobo.Common
import           LessChobo.Features
import           LessChobo.Users

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON (..), ToJSON (..), Value,
                                       withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson           as Aeson
import           Data.Aeson.Types     (Pair)
import           Data.Chinese.CCDict  as CCDict
import           Data.SafeCopy
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text
import           Data.Time
import           Data.Typeable

data Stencil
  = Chinese        Chinese [English]
  -- | ChineseChunked [Chinese] [[English]]
  deriving ( Eq, Ord, Read, Show, Typeable )

features :: Stencil -> Set Feature
features (Chinese chinese _english) = Set.fromList
  [ MandarinWordFeature (entryChinese entry) | KnownWord entry <- tokenizer ccDict chinese ]

schedule :: Stencil -> Reader User (Maybe UTCTime)
schedule stencil@Chinese{} = fmap scheduleByReps $ mapM lookupRep (Set.toList $ features stencil)










--------------------------------------------------------------------
-- ToJSON/FromJSON instances

(.=.) :: ToJSON a => Text -> a -> Pair
a .=. b  = a Aeson..= b

typed :: String -> [Pair] -> Value
typed ty pairs = Aeson.object (("type" .=. ty) : pairs)

instance ToJSON Stencil where
  toJSON stencil =
    case stencil of
      Chinese chinese english -> typed "chinese"
        [ "chinese" .=. chinese
        , "english" .=. english ]

instance FromJSON Stencil where
  parseJSON = withObject "Stencil" $ \o -> do
    ty <- o .: "type"
    case ty of
      "chinese" -> Chinese <$> o.:"chinese" <*> o.:? "english" .!= []
      _other    -> fail $ "Unknown stencil type: " ++ ty


deriveSafeCopy 0 'base ''Stencil

