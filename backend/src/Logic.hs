{-# LANGUAGE OverloadedStrings #-}
module Logic where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Chinese.CCDict
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Time
import           Database.PostgreSQL.Simple

import           DB
import           LessChobo.Cards
import           LessChobo.Features         (Feature (..), applyResponse,
                                             bumpRep, repSchedule, defaultRep)
import           LessChobo.Responses
import           LessChobo.Stencils

type Instantiate a = State (Map Feature Model) a

getModel :: Feature -> Instantiate Model
getModel feat = gets (Map.findWithDefault (defaultRep feat) feat)

setModel :: Feature -> Model -> Instantiate ()
setModel feat model = modify $ Map.insert feat model

runInstantiate :: Instantiate a -> [(Feature,Model)] -> a
runInstantiate action st = evalState action (Map.fromList st)

instantiateStencils :: UTCTime -> UserId ->
    [(StencilId, Stencil)] -> Instantiate [Card]
instantiateStencils now userId lst = catMaybes <$> mapM worker lst
  where
    worker (stencilId, stencil) = do
      mbCardContent <- instantiateContent stencil now
      return $ do
        cardContent <- mbCardContent
        return Card{ cardStencil = stencilId, cardContent = cardContent }

dropSeparator :: [Token] -> [Token]
dropSeparator = filter (not . isTokenSeparator)
  where
    isTokenSeparator (UnknownWord "|") = True
    isTokenSeparator _ = False

instantiateContent :: Stencil -> UTCTime -> Instantiate (Maybe CardContent)
instantiateContent (Chinese chinese english _comment) now = do
    let rows = zip (T.lines chinese) (T.lines (T.unlines english) ++
                                        repeat T.empty)
    sentences <- forM rows $ \(line, hint) -> do
      blocks <- forM (dropSeparator $ tokenizer' ccDict line) $ \token ->
        case token of
          KnownWord entry -> do
            let feat = MandarinWordFeature (entryChinese entry)
            rep <- getModel feat
            setModel feat (bumpRep now rep)
            let gapped = maybe True (<now) (repSchedule rep)
            let definitions = nub $
                  zipWith MandarinDefinition (entryPinyin entry) (entryDefinition entry)
            return $ MandarinWord (entryChinese entry) definitions gapped
          UnknownWord txt -> return $ EscapedBlock txt
      return $ MandarinGapSentence blocks hint
    let isValid (MandarinGapSentence blocks _) = not $ null [ () | MandarinWord _ _ True <- blocks ]
    if any isValid sentences
      then return $ Just $ ChineseCard sentences
      else return Nothing







drawReviewCards :: Connection -> UserId -> CourseId -> IO [Card]
drawReviewCards conn userId courseId = do
    tasks <- fetchReviewStencils conn userId courseId
    now <- getCurrentTime
    let (stencils, brains) = unzip
            [ ((stencilId, stencil), models)
            | (stencilId, stencil, models) <- tasks ]
    return $ runInstantiate (instantiateStencils now userId stencils)
        (concat brains)

drawStudyCards :: Connection -> UserId -> CourseId -> Int -> IO [Card]
drawStudyCards conn userId courseId unitIdx = do
    tasks <- fetchStudyStencils conn userId courseId unitIdx
    now <- getCurrentTime
    let (stencils, brains) = unzip
            [ ((stencilId, stencil), models)
            | (stencilId, stencil, models) <- tasks ]
    return $ runInstantiate (instantiateStencils now userId stencils)
        (concat brains)





addResponse :: Connection -> Response -> IO ()
addResponse conn response = do
    DB.addResponse conn response
    brains <- DB.fetchStencilModels conn
        (responseUserId response)
        (responseStencil response)
    let newBrains = updateBrain response brains
    DB.postModels conn (responseUserId response) newBrains

updateBrain :: Response -> [(FeatureId, Feature, Model)] -> [(FeatureId, Model)]
updateBrain response = mapMaybe worker
  where
    worker (featureId, feature, model) =
        let model' = applyResponse response feature model in
        if model' == model
            then Nothing
            else Just (featureId, model')
