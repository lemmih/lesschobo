{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main ( main ) where

import           LessChobo.Common
import           LessChobo.Responses
import           LessChobo.State

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Data.Acid
import           Data.Acid.Advanced
import qualified Data.Aeson          as Aeson
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Time
import           Database.MongoDB
import           Debug.Trace
import           Happstack.Server    hiding (Host, Response)
import           Network.URI         (URI (..), URIAuth (..), parseURI,
                                      uriAuthority)
import           System.Environment
import           System.IO.Error

instance ToMessage Aeson.Value where
  toContentType _ = "text/json"
  toMessage       = Aeson.encode

instance FromData Response where
  fromData = do
    pairs <- lookPairs
    trace ("pairs: " ++ show pairs) $ return undefined

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

main :: IO ()
main = do
  (mongoHost, database) <- getMongoAddr
  pipe <- connect mongoHost
  global <- openChobo

  forkIO $ forever $ do
    updCourseStats global pipe database `catch` \e -> do
      putStrLn $ "Caught exception: " ++ show (e :: SomeException)
    threadDelay (round (1e6 :: Double))

  simpleHTTP nullConf (msum
    [ do -- decodeBody (defaultBodyPolicy "/tmp/" maxSize maxSize maxSize)
         --liftIO $ putStrLn "Body decoded"
         mzero
    , dir "users" $ path $ \userId -> do
      update' global $ EnsureUser userId
      msum
        [ dir "courses" $ msum
          [ path $ \courseId -> msum
            [ dir "review" $ do
              liftIO $ putStrLn "review"
              now <- liftIO getCurrentTime
              cards <- query' global $ DrawRepetitionCards now userId courseId
              ok $ toResponse $ Aeson.toJSON cards
            , path $ \unitIdx -> do
              liftIO $ putStrLn "study"
              now <- liftIO getCurrentTime
              cards <- query' global $ DrawCards now userId courseId unitIdx
              ok $ toResponse $ Aeson.toJSON cards
            ]
          ]
        ]
    , dir "responses" $ do
      method POST
      response <- jsonBody
      () <- update' global $ AddResponse response
      ok $ toResponse ()
    , dir "responses" $ do
      method GET
      lst <- query' global ExportPermaResponses
      ok $ toResponse $ Aeson.toJSON lst

    , dir "courses" $ path $ \courseId -> do
      method PUT
      unitList <- jsonBody
      () <- update' global $ PutCourse courseId unitList
      ok $ toResponse ()

    , dir "units" $ path $ \unitId -> do
      liftIO $ putStrLn "units"
      method PUT
      stencils <- jsonBody
      () <- update' global $ PutUnit unitId stencils
      ok $ toResponse ()
    ]) `finally` closeAcidState global
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





openChobo :: IO (AcidState Global)
openChobo = openLocalState emptyGlobal



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
updCourseStats :: AcidState Global -> Pipe -> Database -> IO ()
updCourseStats global pipe database = do
  -- fetch expired or old userId/courseId pairs
  now <- liftIO getCurrentTime
  let expires = addUTCTime 80000 now
  access pipe slaveOk database $ do
    cursor <- find (select ["expiresAt" =: ["$lt" =: now]] "CourseStatsTimes")
    fix $ \loop -> do
      objs <- nextBatch cursor
      unless (null objs) $ do
        forM_ objs $ \obj -> do
          liftIO $ putStrLn $ "Got object: " ++ show obj
          let userId = at "userId" obj
              courseId = at "courseId" obj
          doc <- liftIO $ mkUserStats global now userId courseId
          liftIO $ putStrLn $ "Made doc: " ++ show doc
          upsert
            (select [ "userId" =: userId
                    , "courseId" =: courseId] "CourseStats")
            doc
          upsert
            (select [ "userId" =: userId
                    , "courseId" =: courseId] "CourseStatsTimes")
            [ "userId" =: userId
            , "courseId" =: courseId
            , "updatedAt" =: now
            , "expiresAt" =: expires ]
        loop
    closeCursor cursor
    return ()
  -- recalculate review/seen/mastered for each one of them.
  return ()

{-
{ userId:   id
, courseId: id
, review:   boolean
, seen:     int
, mastered: int
, total:    int
}
-}
mkUserStats :: AcidState Global -> UTCTime -> UserId -> CourseId -> IO Document
mkUserStats global now userId courseId = do
  (review, seen, mastered, total) <-
    query global $ GenCourseStats now userId courseId
  return
    [ "userId"   =: userId
    , "courseId" =: courseId
    , "review"   =: review
    , "seen"     =: seen
    , "mastered" =: mastered
    , "total"    =: total ]
