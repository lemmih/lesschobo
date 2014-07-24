{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LessChobo.State where

import           LessChobo.Cards
import           LessChobo.Common
import           LessChobo.Features
import           LessChobo.Responses
import           LessChobo.ResponseStore
import           LessChobo.Stencils
import           LessChobo.StencilStore
import           LessChobo.Unique
import           LessChobo.Users
import           LessChobo.UserStore
import           LessChobo.Utilities

import           Control.Lens            (makeLenses, use, view, (%=), (.=))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Aeson              (ToJSON (..), object)
import           Data.Char
import           Data.Chinese.CCDict     as CCDict
import           Data.List               (foldl', nub, sortBy)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Ord
import           Data.SafeCopy
import qualified Data.Set                as Set
import qualified Data.Text               as T
import           Data.Time
import           Data.Typeable


-- World?
data Global = Global
  { _globalUnits       :: UnitStore
  , _globalStencils    :: StencilStore
  , _globalUsers       :: UserStore
  , _globalResponses   :: ResponseStore
  , _globalUniqueStore :: UniqueStore
  } deriving ( Typeable )

emptyGlobal :: Global
emptyGlobal = Global
  { _globalUnits     = emptyUnitStore
  , _globalStencils  = emptyStencilStore
  , _globalUsers     = emptyUserStore
  , _globalResponses = emptyResponseStore
  , _globalUniqueStore = emptyUniqueStore
  }



{-
888     888
888     888
888     888
888     888 .d8888b   .d88b.  888d888
888     888 88K      d8P  Y8b 888P"
888     888 "Y8888b. 88888888 888
Y88b. .d88P      X88 Y8b.     888
 "Y88888P"   88888P'  "Y8888  888
-}







{-
888     888          d8b 888
888     888          Y8P 888
888     888              888
888     888 88888b.  888 888888
888     888 888 "88b 888 888
888     888 888  888 888 888
Y88b. .d88P 888  888 888 Y88b.
 "Y88888P"  888  888 888  "Y888
-}


data UnitStore = UnitStore
  { unitStoreById :: Map UnitId Unit }

emptyUnitStore :: UnitStore
emptyUnitStore = UnitStore { unitStoreById = Map.empty }

insertUnitStore :: UnitId -> Unit -> UnitStore -> UnitStore
insertUnitStore unitId unit store = store
  { unitStoreById = Map.insert unitId unit (unitStoreById store) }

lookupUnitStore :: UnitId -> UnitStore -> Maybe Unit
lookupUnitStore unitId = Map.lookup unitId . unitStoreById

deleteUnitStore :: UnitId -> UnitStore -> UnitStore
deleteUnitStore unitId store = store
  { unitStoreById = Map.delete unitId (unitStoreById store) }

listUnitStore :: UnitStore -> [(UnitId, Unit)]
listUnitStore = Map.toList . unitStoreById



data Unit = Unit
  { unitTitle    :: Title
  , unitCategory :: Category
  , unitSections :: [Section] -- Unused for now, 2013-12-09
  , unitStencils :: [StencilId]
  } deriving ( Typeable )

instance ToJSON Unit where
  toJSON Unit{..} = object
    [ "title" .=. unitTitle
    , "slug"  .=. mkSlug unitTitle
    , "category" .=. unitCategory ]

mkSlug :: Title -> String
mkSlug = mapMaybe replace . map toLower
  where
    replace c | isSpace c = Just '_'
    replace c | isPunctuation c = Nothing
    replace c = Just c

type Category = [String]



type Title = String
type Passive = String

type SectionId = UniqueId
data Section = Section Title Passive [StencilId]




















deriveSafeCopy 0 'base ''Global
instance SafeCopy ResponseStore where
  version = 0
  putCopy = contain . safePut . responseStoreToList
  getCopy = contain $ fmap responseStoreFromList safeGet
deriveSafeCopy 0 'base ''UserStore
deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''Rep
deriveSafeCopy 0 'base ''RecallCurve
deriveSafeCopy 0 'base ''UnitStore
deriveSafeCopy 0 'base ''Unit
deriveSafeCopy 0 'base ''Section
deriveSafeCopy 0 'base ''UniqueStore
deriveSafeCopy 0 'base ''Feature
deriveSafeCopy 0 'base ''Response

deriveSafeCopy 1 'extension ''ResponseContent
deriveSafeCopy 0 'base ''Card
deriveSafeCopy 0 'base ''CardContent
deriveSafeCopy 0 'base ''MandarinGapSentence
deriveSafeCopy 0 'base ''MandarinBlock
deriveSafeCopy 0 'base ''MandarinDefinition

makeLenses ''Global



instantiateStencils :: UTCTime -> User -> [StencilId] -> Query Global [Card]
instantiateStencils now = worker []
  where
    worker acc _user [] = return $ reverse acc
    worker acc user (stencilId:stencilIds) = do
      stencil <- requireStencil stencilId
      case runState (instantiateContent stencil now) user of
        (Nothing, user') -> worker acc user' stencilIds
        (Just content, user') -> do
          let card = Card{ cardStencil = stencilId, cardContent = content }
          worker (card:acc) user' stencilIds

instantiateContent :: Stencil -> UTCTime -> State User (Maybe CardContent)
instantiateContent (Chinese chinese english) now = do
    let rows = zip (T.lines chinese) (T.lines (T.unlines english) ++ repeat T.empty)
    sentences <- forM rows $ \(line, hint) -> do
      blocks <- forM (tokenizer ccDict line) $ \token ->
        case token of
          KnownWord entry -> do
            let feat = MandarinWordFeature (entryChinese entry)
            rep <- gets (runReader (lookupRep feat))
            let gapped = maybe True (<now) (repSchedule rep)
            setUserRep feat (bumpRep now rep)
            let definitions = nub $
                  zipWith MandarinDefinition (entryPinyin entry) (entryDefinition entry)
            return $ MandarinWord (entryChinese entry) definitions gapped
          UnknownWord txt -> return $ EscapedBlock txt
      return $ MandarinGapSentence blocks hint
    let isValid (MandarinGapSentence blocks _) = not $ null [ () | MandarinWord _ _ True <- blocks ]
    if any isValid sentences
      then return $ Just $ ChineseCard sentences
      else return Nothing











addStencil :: Stencil -> Update Global StencilId
addStencil stencil = do
  store <- use globalStencils
  case lookupStencilId stencil store of
    Just stencilId -> return stencilId
    Nothing -> do
      stencilId <- newUnique
      globalStencils %= insertStencilStore stencilId stencil
      return stencilId

addResponse :: Response -> Update Global ()
addResponse response = do

  responseId <- newUnique
  globalResponses %= insertResponseStore responseId response
  stencilStore <- liftQuery $ view globalStencils

  -- Update the models touched by the response.
  u0      <- liftQuery $ requireUser (responseUserId response)
  originalStencil <- liftQuery $ requireStencil (responseStencil response)

  let worker user feat =
        let rep  = runReader (lookupRep feat) user
            rep' = applyResponse response feat rep
        in execState (setUserRep feat rep') user

  let feats = Set.toList $ features originalStencil
      u1 = foldl' worker u0 feats
      dirtyFeatures =
          [ feat | feat <- feats
          , runReader (lookupRep feat) u0 /= runReader (lookupRep feat) u1 ]
      dirtyStencils = Map.toList $ Map.unions
          [ lookupStencilByFeature feature stencilStore
          | feature <- dirtyFeatures ]
      fixSchdule user (stencilId, stencil) =
        setStencilSchedule stencilId (runReader (schedule stencil) user) user
      u2 = foldl' fixSchdule u1 dirtyStencils

  globalUsers %= insertUserStore (responseUserId response) u2

  return ()

clearResponses :: UserId -> Update Global ()
clearResponses userId = do
  globalResponses %= deleteUserResponseStore userId
  globalUsers %= clearUserStore userId

listResponses :: UserId -> Query Global [Response]
listResponses userId = do
  store <- view globalResponses
  let responses = Set.toList $ lookupUserResponses userId store
  return $ sortBy (comparing responseAt) responses

viewModel :: UserId -> Query Global [(Feature, Rep)]
viewModel userId = do
  user <- requireUser userId
  let ordFn (_feat, rep) = repSchedule rep
  return $ sortBy (comparing ordFn) $ Map.toList (userModel user)

resetAllUserModels :: Update Global ()
resetAllUserModels = do
  store <- use globalUsers
  mapM_ resetUserModel (idsUserStore store)

resetUserModel :: UserId -> Update Global ()
resetUserModel userId = do
  store <- use globalResponses
  u0    <- liftQuery $ requireUser userId
  let cleanUser = u0{userModel = Map.empty, userSchedule = Map.empty, userSchedule' = Map.empty}
  let responses = sortBy (comparing responseAt) $ Set.toList $ lookupUserResponses userId store
  let apply response user feat =
        let rep  = runReader (lookupRep feat) user
            rep' = applyResponse response feat rep
        in execState (setUserRep feat rep') user
  let worker user response = do
        stencil <- liftQuery $ requireStencil (responseStencil response)
        return $ foldl' (apply response) user (Set.toList $ features stencil)
  u1 <- foldM worker cleanUser responses
  globalUsers %= insertUserStore userId u1

drawRepetitionCards :: UTCTime -> UserId -> UnitId -> Query Global [Card]
drawRepetitionCards now userId unitId = do
  user <- requireUser userId
  unit <- requireUnit unitId
  let filterSet = Set.fromList (unitStencils unit)
  -- TODO: shuffle set elements.
  let (before,_) = Map.split now (userSchedule user)
      stencilIds = concatMap (Set.toList . Set.intersection filterSet) (reverse $ Map.elems before)
  instantiateStencils now user stencilIds

drawNovelCards :: UTCTime -> UserId -> UnitId -> Query Global [Card]
drawNovelCards now userId unitId = do
  user <- requireUser userId
  unit <- requireUnit unitId
  let stencilIds = unitStencils unit
  instantiateStencils now user stencilIds

drawCards :: UTCTime -> UserId -> UnitId -> Query Global [Card]
drawCards now userId unitId = do
  repCards <- drawRepetitionCards now userId unitId
  newCards <- drawNovelCards now userId unitId
  let repIds = map cardStencil repCards
  return (take 10 $ repCards ++ (filter ((`notElem` repIds) . cardStencil) newCards))

listAnnotatedStencils :: UserId -> UnitId -> Query Global [(StencilId, Stencil, Maybe UTCTime)]
listAnnotatedStencils userId unitId = do
  user <- requireUser userId
  unit <- requireUnit unitId
  forM (unitStencils unit) $ \stencilId -> do
    stencil <- requireStencil stencilId
    return (stencilId, stencil, Map.lookup stencilId (userSchedule' user))

updateStencilSchedules :: UserId -> UnitId -> Update Global ()
updateStencilSchedules userId unitId = do
  user <- liftQuery $ requireUser userId
  unit <- liftQuery $ requireUnit unitId
  let stencilIds = unitStencils unit
  stencils <- liftQuery $ mapM requireStencil stencilIds
  let worker u (stencil, stencilId) =
        let newSchedule = runReader (schedule stencil) u
        in setStencilSchedule stencilId newSchedule u
      user' = foldl' worker user (zip stencils stencilIds)
  globalUsers %= insertUserStore userId user'

probeStats :: UserId -> UnitId -> Query Global (Map String Int)
probeStats = undefined

newUnique :: Update Global UniqueId
newUnique = do
  store <- use globalUniqueStore
  let (uniq, store') = getNextUnique store
  globalUniqueStore .= store'
  return uniq

requireStencil :: StencilId -> Query Global Stencil
requireStencil stencilId = do
  store <- view globalStencils
  case lookupStencil stencilId store of
    Nothing      -> error $ "Missing stencil: " ++ show stencilId
    Just stencil -> return stencil

requireUser :: UserId -> Query Global User
requireUser userId = do
  store <- view globalUsers
  case lookupUserStore userId store of
    Nothing   -> error $ "User not found: " ++ show userId
    Just user -> return user

requireUnit :: UnitId -> Query Global Unit
requireUnit unitId = do
  store <- view globalUnits
  case lookupUnitStore unitId store of
    Nothing   -> error $ "Unit not found: " ++ show unitId
    Just unit -> return unit

addUser :: Alias -> Update Global UserId
addUser alias = do
  userId <- newUnique
  globalUsers %= insertUserStore userId emptyUser{ userAlias = alias }
  return userId

addSimpleUnit :: Title -> [Stencil] -> Update Global UnitId
addSimpleUnit title stencils = do
  stencilIds <- mapM addStencil stencils
  unitId <- newUnique
  let unit = Unit{ unitTitle = title, unitCategory = []
                 , unitSections = [], unitStencils = stencilIds }
  globalUnits %= insertUnitStore unitId unit
  return unitId

updSimpleUnit :: UnitId -> Title -> [Stencil] -> Update Global ()
updSimpleUnit unitId title stencils = do
  stencilIds <- mapM addStencil stencils
  let unit = Unit{ unitTitle = title, unitCategory = []
                 , unitSections = [], unitStencils = stencilIds }
  globalUnits %= insertUnitStore unitId unit
  return ()

deleteUnit :: UnitId -> Update Global ()
deleteUnit unitId =
  globalUnits %= deleteUnitStore unitId

listUnits :: Query Global [(UnitId, Unit)]
listUnits = do
  store <- view globalUnits
  return $ listUnitStore store

-- FIXME: Removed subscribed units from user data type.
deleteAllUnits :: Update Global ()
deleteAllUnits = globalUnits .= emptyUnitStore

listUsers :: Query Global [(UserId, User)]
listUsers = do
  store <- view globalUsers
  return $ listUserStore store

makeAcidic ''Global [ 'addStencil
                    , 'addResponse
                    , 'clearResponses
                    , 'listResponses
                    , 'viewModel
                    , 'resetUserModel
                    , 'resetAllUserModels
                    , 'drawNovelCards
                    , 'drawCards
                    , 'listAnnotatedStencils
                    , 'updateStencilSchedules
                    , 'addUser
                    , 'addSimpleUnit
                    , 'updSimpleUnit
                    , 'deleteUnit
                    , 'listUnits
                    , 'deleteAllUnits
                    , 'listUsers
                    ]


