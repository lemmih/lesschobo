{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
module Main where

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
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Concurrent
import Control.Exception
import Control.Applicative

instance ToMessage Value where
  toContentType _ = "text/json"
  toMessage value = encode value

instance FromData Response where
  fromData = do
    pairs <- lookPairs
    trace ("pairs: " ++ show pairs) $ return undefined

{-
Endpoints:
GET  /users/:userId/units
GET  /users/:userId/units/:unitId/cards
POST /responses
-}
main :: IO ()
main = do
  global <- openChobo
  simpleHTTP nullConf (msum
    [ do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
         mzero
    , dir "favicon.iso" $ notFound (toResponse ())
    --, dir "units" $ path $ \unitId -> msum
    --  [ dir "stencils" $ do
    --    method GET
    --    trailingSlash
    --    stencils <- query' global $ UnitStencils unitId
    --    ok $ toResponse $ toJSON stencils
    --  ]
    , dir "users" $ path $ \userId -> msum
      [ dir "units" $ msum
          [ path $ \unitId -> msum
              [ dir "cards" $ do
                now <- liftIO getCurrentTime
                cards <- query' global $ DrawCards now userId unitId
                ok $ toResponse $ toJSON cards
              , dir "stencils" $ do
                method GET
                trailingSlash
                annStencils <- query' global (ListAnnotatedStencils userId unitId)
                ok $ toResponse $ toJSON
                  [ object [ "id"       .= stencilId
                           , "order"    .= (n::Int)
                           , "stencil"  .= stencil
                           , "schedule" .= schedule ]
                  | (stencilId, stencil, schedule) <- annStencils
                  | n <- [1..] ]
              ]
          , do
              method GET
              trailingSlash
              units <- query' global ListUnits
              ok $ toResponse $ toJSON $
                [ object [ "id" .= unitId, "unit" .= unit ] | (unitId, unit) <- units ]
          ]
      ]
    , dir "responses" $ do
      method POST
      response <- jsonBody
      () <- update' global $ AddResponse response
      ok $ toResponse ()
    ]) `finally` closeAcidState global
  where
    jsonBody = do
      rq <- askRq
      bs <- liftIO $ unBody <$> readMVar (rqBody rq)
      liftIO $ L.putStrLn bs
      case decode bs of
        Nothing    -> liftIO (putStrLn "failed to parse") >> mzero
        Just value -> return value





openChobo :: IO (AcidState Global)
openChobo = openLocalState emptyGlobal

