{-# LANGUAGE OverloadedStrings #-}
module DB
    ( runDB
    , ensureUser
    , postCourse
    , postUnit
    , postStencils
    ) where

import LessChobo.Stencils (Stencil(..))

import Data.Aeson
import           Control.Monad
import           Data.Pool
import           Data.Text                (Text)
import           Data.UUID (UUID)
import           Database.PostgreSQL.Simple

type CourseId  = Text
type UnitId    = Text
type UserId    = Text
type StencilId = UUID



runDB :: Pool Connection -> (Connection -> IO a) -> IO a
runDB pool action =
    withResource pool $ \conn ->
    withTransaction conn (action conn)

ensureUser :: Connection -> UserId -> IO ()
ensureUser conn userId = do
    rets <- query conn "SELECT id FROM Users WHERE id = ?" (Only userId)
    when (null (rets :: [Only UserId])) $ do
        _ <- execute conn "INSERT INTO Users (id) VALUES (?)" (Only userId)
        return ()

postCourse :: Connection -> CourseId -> [UnitId] -> IO ()
postCourse conn courseId units = do
    _ <- executeMany conn
        "DELETE FROM Units WHERE id = ? AND course_id = ?"
        [ (unitId, courseId)
        | unitId <- units ]
    _ <- executeMany conn
        "INSERT INTO Units (id, course_id, index) VALUES (?, ?, ?)"
        [ (unitId, courseId, n::Int)
        | (unitId, n) <- zip units [0..] ]
    return ()

postStencils :: Connection -> [Stencil] -> IO [StencilId]
postStencils conn stencils = do
    rows <- returning conn "INSERT INTO StencilsView (content) VALUES (?) \
                            \RETURNING id"
                [ Only (encode stencil)
                | stencil <- stencils ]
    return $ map fromOnly rows

{-
CREATE TABLE Units
  ( id        text PRIMARY KEY
  , course_id text
  , index     int
  , UNIQUE (course_id, index)
  );

CREATE TABLE UnitMembers
  ( unit_id    text REFERENCES Units(id)
  , stencil_id uuid REFERENCES Stencils(id)
  , index      int
  );

-}
postUnit :: Connection -> UnitId -> [Stencil] -> IO ()
postUnit conn unitId stencils = do
    stencilIds <- postStencils conn stencils
    _ <- executeMany conn
        "INSERT INTO UnitMembers (unit_id, stencil_id, index) \
        \VALUES (?, ?, ?)"
        [ (unitId, stencilId, n::Int)
        | (stencilId, n) <- zip stencilIds [0..] ]
    return ()

