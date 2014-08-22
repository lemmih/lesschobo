{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

--import           LessChobo.Tools
import           LessChobo.State
import           LessChobo.Responses

import           Control.Monad
import           Control.Monad.Trans
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Aeson
import           Data.Time
import           Happstack.Server hiding (Response)
import Debug.Trace
import Control.Exception
import Control.Applicative

instance ToMessage Value where
  toContentType _ = "text/json"
  toMessage value = encode value

instance FromData Response where
  fromData = do
    pairs <- lookPairs
    trace ("pairs: " ++ show pairs) $ return undefined

maxSize :: Int
maxSize = 1024*1024*10

main :: IO ()
main = do
  global <- openChobo
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
              ok $ toResponse $ toJSON cards
            , path $ \unitIdx -> do
              liftIO $ putStrLn "study"
              now <- liftIO getCurrentTime
              cards <- query' global $ DrawCards now userId courseId unitIdx
              ok $ toResponse $ toJSON cards
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
      ok $ toResponse $ toJSON lst

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
    jsonBody :: FromJSON a => ServerPart a
    jsonBody = do
      liftIO $ putStrLn "jsonBody"
      rq <- askRq
      mbBS <- fmap unBody <$> takeRequestBody rq
      case mbBS of
        Nothing -> mzero
        Just bs -> do
          -- liftIO $ L.putStrLn bs
          case decode bs of
            Nothing    -> liftIO (putStrLn "failed to parse") >> mzero
            Just value -> return value





openChobo :: IO (AcidState Global)
openChobo = openLocalState emptyGlobal

