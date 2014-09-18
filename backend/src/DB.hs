{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB
    ( runDB
    , ensureUser
    , postCourse
    , postUnit
    , postStencils
    , addResponse
    , fetchStencilModels
    , postModels
    , fetchReviewStencils
    , fetchStudyStencils
    , fetchDirtyStencils
    , postFeatures
    , FeatureId
    , Model
    , UserId
    , CourseId
    ) where

import           LessChobo.Features                   (Feature (..), Rep,
                                                       repSchedule)
import           LessChobo.Responses
import           LessChobo.Stencils                   (Stencil (..))

import qualified Data.Vector as V
import           Control.Monad
import           Control.Exception
import           Data.Aeson
import           Data.Pool
import           Data.Text                            (Text)
import           Data.UUID                            (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Transaction
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

type CourseId   = Text
type UnitId     = Text
type UserId     = Text
type StencilId  = UUID
type ResponseId = UUID
type FeatureId  = UUID

type Model = Rep

instance FromField Feature where
    fromField = fromJSONField

instance ToField Feature where
    toField = toJSONField


instance ToField ResponseContent where
    toField = toJSONField


instance FromField Stencil where
    fromField = fromJSONField

instance ToField Stencil where
    toField = toJSONField



instance FromField Rep where
    fromField = fromJSONField

instance ToField Rep where
    toField = toJSONField


logErrors :: IO a -> IO a
logErrors action = action `catch` \e -> do
    putStrLn $ "Error: " ++ show (e::SomeException)
    throwIO e

runDB :: Pool Connection -> (Connection -> IO a) -> IO a
runDB pool action =
    logErrors $
    withResource pool $ \conn ->
    withTransactionSerializable conn (action conn)

ensureUser :: Connection -> UserId -> IO ()
ensureUser conn userId = do
    rets <- query conn "SELECT id FROM Users WHERE id = ?" (Only userId)
    when (null (rets :: [Only UserId])) $ do
        _ <- execute conn "INSERT INTO Users (id) VALUES (?)" (Only userId)
        return ()

postCourse :: Connection -> CourseId -> [UnitId] -> IO ()
postCourse conn courseId units = do
    void $ execute conn
        "DELETE FROM Units WHERE course_id = ?"
        (Only courseId)
    _ <- executeMany conn
        "INSERT INTO Units (id, course_id, index) VALUES (?, ?, ?)"
        [ (unitId, courseId, n::Int)
        | (unitId, n) <- zip units [0..] ]
    void $ execute conn
        "DELETE FROM Inherit WHERE receiver = ? AND receiver = ?"
        (courseId, courseId)
    void $ execute conn
        "INSERT INTO Inherit (receiver, giver) VALUES (?,?)"
        (courseId, courseId)
    return ()

postStencils :: Connection -> [Stencil] -> IO [StencilId]
postStencils conn stencils = do
    rows <- returning conn "INSERT INTO StencilsView (content) VALUES (?) \
                            \RETURNING id"
                [ Only stencil
                | stencil <- stencils ]
    return $ map fromOnly rows

postUnit :: Connection -> UnitId -> [Stencil] -> IO ()
postUnit conn unitId stencils = do
    _ <- execute conn "DELETE FROM UnitMembers WHERE unit_id = ?" (Only unitId)
    stencilIds <- postStencils conn stencils
    _ <- executeMany conn
        "INSERT INTO UnitMembers (unit_id, stencil_id, index) \
        \VALUES (?, ?, ?)"
        [ (unitId, stencilId, n::Int)
        | (stencilId, n) <- zip stencilIds [0..] ]
    return ()

querySingle :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO r
querySingle conn str q = do
    rows <- query conn str q
    case rows of
        []  -> error $ "querySingle: No results: " ++ show str
        [x] -> return x
        _   -> error $ "querySingle: Multiple results: " ++ show str

addResponse :: Connection -> Response -> IO ResponseId
addResponse conn Response{..} = do
    Only responseId <- querySingle conn
        "INSERT INTO Responses (id, stencil_id, user_id, content, at) \
        \VALUES (uuid_generate_v4(),?,?,?,?) RETURNING id"
        (responseStencil, responseUserId, responseContent, responseAt)
    return responseId

fetchStencilModels :: Connection -> UserId -> StencilId ->
    IO [(FeatureId, Feature, Maybe Model)]
fetchStencilModels conn userId stencilId =
    query conn
        "SELECT id, Features.content, \
        \  (SELECT content FROM Models WHERE user_id = ? AND \
        \     feature_id = Features.id) \
        \FROM Features, StencilFeatures \
        \WHERE StencilFeatures.stencil_id = ? AND \
        \      Features.id = StencilFeatures.feature_id"
        (userId, stencilId)

postModels :: Connection -> UserId -> [(FeatureId, Model)] -> IO ()
postModels conn userId models = do
    void $ execute conn
        "DELETE FROM Models WHERE user_id = ? AND feature_id IN ?"
        (userId, In (map fst models))
    void $ executeMany conn
        "INSERT INTO Models (user_id, feature_id, content, at)\
        \ VALUES (?, ?, ?, ?)"
        [ (userId, featureId, model, repSchedule model)
        | (featureId, model) <- models ]

fetchReviewStencils :: Connection -> UserId -> CourseId ->
    IO [(StencilId, Stencil, [(Feature, Maybe Model)])]
fetchReviewStencils conn userId courseId = do
    rows <- query conn
        "SELECT stencil_id, stencil, features, models \
        \FROM Review \
        \WHERE user_id = ? AND course_id = ? AND review_at < now() \
        \ORDER BY review_at DESC \
        \LIMIT 10"
        (userId, courseId)
    return
        [ (stencilId, stencil, V.toList (V.zip features models))
        | (stencilId, stencil, features, models) <- rows]

fetchStudyStencils :: Connection -> UserId -> CourseId -> Int ->
    IO [(StencilId, Stencil, [(Feature, Maybe Model)])]
fetchStudyStencils conn userId courseId unitIdx = do
    rows <- query conn
        "SELECT stencil_id, stencil, features, models \
        \FROM Study \
        \WHERE user_id = ? AND course_id = ? AND unitindex <= ? AND\
        \      at IS NULL \
        \ORDER BY stencilindex \
        \LIMIT 10"
        (userId, courseId, unitIdx)
    return
        [ (stencilId, stencil, V.toList (V.zip features models))
        | (stencilId, stencil, features, models) <- rows]

fetchDirtyStencils :: Connection -> IO [(StencilId, Stencil)]
fetchDirtyStencils conn =
    query conn
        "SELECT id, content \
        \FROM Stencils \
        \WHERE dirty"
        ()

postFeatures :: Connection -> StencilId -> [Feature] -> IO ()
postFeatures conn stencilId features = do
    featureIds <- returning conn
        "INSERT INTO FeaturesView (content) VALUES (?) RETURNING id"
        [ Only feature | feature <- features ]
    void $ execute conn "DELETE FROM StencilFeatures WHERE stencil_id = ?"
        (Only stencilId)
    void $ executeMany conn
        "INSERT INTO StencilFeatures (stencil_id, feature_id) VALUES (?,?)"
        [ (stencilId, featureId :: FeatureId)
        | Only featureId <- featureIds ]
    void $ execute conn
        "UPDATE Stencils SET dirty = false WHERE id = ?"
        (Only stencilId)

