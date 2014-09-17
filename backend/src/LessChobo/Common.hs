module LessChobo.Common where

import           Data.Aeson
import           Data.Text
import           Data.UUID
import Data.SafeCopy
import Data.Serialize

instance ToJSON UUID where
    toJSON = toJSON . show

instance FromJSON UUID where
    parseJSON = withText "UUID" $ \txt ->
        case reads (unpack txt) of
            [(uuid,"")] -> return uuid
            _           -> fail "invalid uuid"

instance Serialize UUID where
    put = undefined
    get = undefined
instance SafeCopy UUID where

type UniqueId   = Int
type UnitId     = Text
type CourseId   = Text
type UserId     = Text
type StencilId  = UUID -- UniqueId
type ResponseId = UniqueId

type Chinese = Text
type Pinyin  = Text
type English = Text
type Comment = Text
