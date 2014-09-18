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
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Time
import           Database.PostgreSQL.Simple

import           DB
import           LessChobo.Cards
import           LessChobo.Features         (Feature (..), applyResponse,
                                             bumpRep, defaultRep, repSchedule)
import           LessChobo.Responses
import           LessChobo.Stencils

type Instantiate a = State (Map Feature Model) a

getModel :: Feature -> Instantiate Model
getModel feat = gets (Map.findWithDefault (defaultRep feat) feat)

setModel :: Feature -> Model -> Instantiate ()
setModel feat model = modify $ Map.insert feat model

runInstantiate :: Instantiate a -> [(Feature,Maybe Model)] -> a
runInstantiate action st = evalState action m
  where
    m = Map.fromList [ (feat, model) | (feat, Just model) <- st ]

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





limitComplexity :: [Card] -> [Card]
limitComplexity = worker 0
  where
    worker c [] = []
    worker c (card:cards)
      | c >= maxComplexity = []
      | otherwise          = card : worker (c+complexity card) cards
    maxComplexity = 10

drawReviewCards :: Connection -> UserId -> CourseId -> IO [Card]
drawReviewCards conn userId courseId = do
    tasks <- fetchReviewStencils conn userId courseId
    now <- getCurrentTime
    let (stencils, brains) = unzip
            [ ((stencilId, stencil), models)
            | (stencilId, stencil, models) <- tasks ]
    return $ limitComplexity $
      runInstantiate (instantiateStencils now userId stencils)
        (concat brains)

drawStudyCards :: Connection -> UserId -> CourseId -> Int -> IO [Card]
drawStudyCards conn userId courseId unitIdx = do
    review <- fetchReviewStencils conn userId courseId
    study <- fetchStudyStencils conn userId courseId unitIdx
    now <- getCurrentTime
    let (stencils, brains) = unzip
            [ ((stencilId, stencil), models)
            | (stencilId, stencil, models) <- review ++ study ]
    return $ limitComplexity $
      runInstantiate (instantiateStencils now userId stencils)
        (concat brains)





addResponse :: Connection -> Response -> IO ()
addResponse conn response = do
    DB.addResponse conn response
    brains <- DB.fetchStencilModels conn
        (responseUserId response)
        (responseStencil response)
    let newBrains = updateBrain response brains
    DB.postModels conn (responseUserId response) newBrains

updateBrain :: Response -> [(FeatureId, Feature, Maybe Model)]
  -> [(FeatureId, Model)]
updateBrain response = mapMaybe worker
  where
    worker (featureId, feature, mbModel) =
        let model = fromMaybe (defaultRep feature) mbModel
            newModel = applyResponse response feature model in
        if newModel == model
            then Nothing
            else Just (featureId, newModel)




updDirtyStencils :: Connection -> IO ()
updDirtyStencils conn = do
  stencils <- fetchDirtyStencils conn
  forM_ stencils $ \(stencilId, stencil) ->
    postFeatures conn stencilId (Set.toList $ features stencil)




