{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main ( main ) where

import           DB
import           LessChobo.Common
import           LessChobo.Responses
import           LessChobo.Stencils (permaUserId)
--import           LessChobo.State
import           Logic

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import Data.List.Split
import           Data.Acid
import           Data.Acid.Advanced
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.Set as Set
import           Data.Maybe
import           Data.Pool
import qualified Data.Text                  as T
import           Data.Time
import           Database.MongoDB
import qualified Database.PostgreSQL.Simple as PSQL
import           Debug.Trace
import           Happstack.Server           hiding (Host, Response)
import           Network.URI                (URI (..), URIAuth (..), parseURI,
                                             uriAuthority)
import           System.Environment
import           System.IO.Error

import qualified Worker

instance ToMessage Aeson.Value where
  toContentType _ = "text/json"
  toMessage       = Aeson.encode

--maxSize :: Int
--maxSize = 1024*1024*10

--MONGO_URL=mongodb://$MONGO_PORT_27017_TCP_ADDR/lesschobo
getMongoAddr :: IO (Host, Database)
getMongoAddr = do
  mongoURL <- getEnv "MONGO_URL" `catchIOError` \_ -> return ""
  return $ fromMaybe (readHostPort "127.0.0.1:3001", "meteor") $ do
    uri <- parseURI mongoURL
    auth <- uriAuthority uri
    addr <- readHostPortM (uriRegName auth ++ uriPort auth)
    return (addr, T.pack $ tail $ uriPath uri)

mkDatabasePool :: IO (Pool PSQL.Connection)
mkDatabasePool = do
  dbAddr <- getEnv "SQL_DB" `catchIOError` \_ -> return "host=localhost user=postgres"
  createPool (PSQL.connectPostgreSQL (B.pack dbAddr)) PSQL.close
    1 -- One stripe.
    (60*60) -- Keep connections open for an hour.
    5 -- Max five connections per stripe.

oneSecond :: Int
oneSecond = 10^6

main :: IO ()
main = do
  group <- Worker.new
  pool <- mkDatabasePool
  (mongoHost, database) <- getMongoAddr
  pipe <- connect mongoHost
  --global <- openChobo


  Worker.forkIO group $ forever $ do
    catch (runDB pool $ updDirtyStencils)
      (\e ->
        putStrLn $ "updDirtyStencils error: " ++ show (e::SomeException))
    threadDelay oneSecond

  Worker.forkIO group $ forever $ do
    catch (runDB pool $ updDirtySchedule)
      (\e ->
        putStrLn $ "updDirtySchedule error: " ++ show (e::SomeException))
    threadDelay oneSecond
  --ThreadGroup.forkIO group $ forever $ do
  --  updCourseStats global pipe database `catch` \e -> do
  --    putStrLn $ "Caught exception: " ++ show (e :: SomeException)
  --  threadDelay (round (1e6 :: Double))

  simpleHTTP nullConf (msum
    [ do -- decodeBody (defaultBodyPolicy "/tmp/" maxSize maxSize maxSize)
         --liftIO $ putStrLn "Body decoded"
         mzero
    , dir "users" $ path $ \userId -> do
      msum
        [ {-dir "duplicate" $ path $ \dstUserId -> do
          method POST
          () <- update' global $ DuplicateUser userId dstUserId
          ok $ toResponse ()
        , -}dir "courses" $ msum
          [ path $ \courseId -> msum
            [ dir "review" $ do
              liftIO $ putStrLn "review"
              cards <- runDB pool $ \conn -> do
                ensureUser conn userId
                drawReviewCards conn userId courseId
              ok $ toResponse $ Aeson.toJSON cards
            , path $ \unitIdx -> do
              liftIO $ putStrLn "study"
              cards <- runDB pool $ \conn -> do
                ensureUser conn userId
                drawStudyCards conn userId courseId unitIdx
              ok $ toResponse $ Aeson.toJSON cards
            ]
          ]
        ]
    , dir "responses" $ do
      method POST
      response <- jsonBody
      runDB pool $ \conn ->
        Logic.addResponse conn response
      ok $ toResponse ()
    
    , dir "perma" $ do
      method GET
      lst <- runDB pool $ fetchPermaResponses
      ok $ toResponse $ Aeson.toJSON lst
    , dir "perma" $ do
      method POST
      responses <- jsonBody
      let userIds = Set.toList $ Set.fromList (map permaUserId responses)
      runDB pool $ \conn ->
        mapM_ (ensureUser conn) userIds
      forM_ (chunksOf 1000 responses) $ \chunk ->
        runDB pool $ \conn -> postPermaResponses conn chunk
      runDB pool $ deleteDuplicateResponses
      ok $ toResponse ()
    
    , dir "courses" $ path $ \courseId -> do
      method PUT
      unitList <- jsonBody
      runDB pool $ \conn ->
        postCourse conn courseId unitList
      ok $ toResponse ()

    , dir "units" $ path $ \unitId -> do
      liftIO $ putStrLn "units"
      method PUT
      stencils <- jsonBody
      runDB pool $ \conn ->
        postUnit conn unitId (sanitizeStencils stencils)
      ok $ toResponse ()

    , dir "recalc" $ do
      liftIO $ forkIO $ runDB pool $ recalcAllBrains
      ok $ toResponse ()
    ]) `finally` Worker.killAll group
  where
    jsonBody :: Aeson.FromJSON a => ServerPart a
    jsonBody = do
      liftIO $ putStrLn "jsonBody"
      rq <- askRq
      mbBS <- fmap unBody <$> takeRequestBody rq
      case mbBS of
        Nothing -> mzero
        Just bs -> do
          -- liftIO $ L.putStrLn bs
          case Aeson.decode bs of
            Nothing    -> liftIO (putStrLn "failed to parse") >> mzero
            Just value -> return value





--openChobo :: IO (AcidState Global)
--openChobo = openLocalState emptyGlobal



{-
CourseStats
{ userId:   id
, courseId: id
, review:   boolean
, seen:     int
, mastered: int
, total:    int
}
CourseStatsTimes
{ userId:    id
, courseId:  id
, updatedAt: date
, expiresAt: date
}
-}
--updCourseStats :: AcidState Global -> Pipe -> Database -> IO ()
--updCourseStats global pipe database = do
--  -- fetch expired or old userId/courseId pairs
--  now <- liftIO getCurrentTime
--  let expires = addUTCTime 80000 now
--  access pipe slaveOk database $ do
--    cursor <- find (select ["expiresAt" =: ["$lt" =: now]] "CourseStatsTimes")
--    fix $ \loop -> do
--      objs <- nextBatch cursor
--      unless (null objs) $ do
--        forM_ objs $ \obj -> do
--          liftIO $ putStrLn $ "Got object: " ++ show obj
--          let userId = at "userId" obj
--              courseId = at "courseId" obj
--          doc <- liftIO $ mkUserStats global now userId courseId
--          liftIO $ putStrLn $ "Made doc: " ++ show doc
--          upsert
--            (select [ "userId" =: userId
--                    , "courseId" =: courseId] "CourseStats")
--            doc
--          upsert
--            (select [ "userId" =: userId
--                    , "courseId" =: courseId] "CourseStatsTimes")
--            [ "userId" =: userId
--            , "courseId" =: courseId
--            , "updatedAt" =: now
--            , "expiresAt" =: expires ]
--        loop
--    closeCursor cursor
--    return ()
--  -- recalculate review/seen/mastered for each one of them.
--  return ()

{-
{ userId:   id
, courseId: id
, review:   boolean
, seen:     int
, mastered: int
, total:    int
}
-}
--mkUserStats :: AcidState Global -> UTCTime -> UserId -> CourseId -> IO Document
--mkUserStats global now userId courseId = do
--  (review, seen, mastered, total) <-
--    query global $ GenCourseStats now userId courseId
--  return
--    [ "userId"   =: userId
--    , "courseId" =: courseId
--    , "review"   =: review
--    , "seen"     =: seen
--    , "mastered" =: mastered
--    , "total"    =: total ]
