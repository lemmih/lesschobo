{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module LessChobo.Features where

import           LessChobo.Common
import           LessChobo.Responses

import           Data.Maybe
import           Data.Time
import           Data.Typeable

type FeatureId = UniqueId

-- Eigenvector? Even more esoteric. Brainbug? Seedling? Dimension?
data Feature
  = MandarinWordFeature Chinese
  deriving ( Eq, Ord, Read, Show, Typeable )

defaultRep :: Feature -> Rep
defaultRep MandarinWordFeature{} = MandarinWordRep Nothing

data Rep
  = MandarinWordRep (Maybe RecallCurve)
  deriving ( Read, Show, Typeable, Eq )

repSchedule :: Rep -> Maybe UTCTime
repSchedule (MandarinWordRep mbCurve) = fmap (recallWhen 0.8) mbCurve

scheduleByReps :: [Rep] -> Maybe UTCTime
scheduleByReps reps =
  case mapM repSchedule reps of
    -- Unless all reps have been scheduled, do not give a final schedule.
    Nothing  -> Nothing
    -- If there are no reps then there's nothing to schedule.
    Just []  -> Nothing
    -- The final schedule is the earliest of the rep schedules.
    Just lst -> Just $ minimum lst

bumpRep :: UTCTime -> Rep -> Rep
bumpRep now rep =
  case rep of
    MandarinWordRep Nothing ->
      MandarinWordRep $ Just (RecallCurve now 100)
    MandarinWordRep (Just curve) ->
      MandarinWordRep $ Just curve{rcSuccessfulRecall = now}



applyResponse :: Response -> Feature -> Rep -> Rep
applyResponse Response{..} feature rep =
  case (responseContent, feature, rep) of
    (MandarinTextAnswer shownAnswer key answer, MandarinWordFeature chinese, MandarinWordRep mbCurve)
      | key == chinese ->
      let curve = fromMaybe (RecallCurve responseAt 100) mbCurve in
      MandarinWordRep $ Just $
      if answer == chinese
        then if shownAnswer
                then (markSuccess responseAt .
                      bumpStability (recip factor)) curve
                else if recallLikelihood responseAt curve > spuriousCutoff && isJust mbCurve
                  then markSuccess responseAt curve -- Spurious response
                  else (markSuccess responseAt .
                        bumpStability factor .
                        setEffectiveStability responseAt) curve
        else bumpStability guessPenalty curve
    _ -> rep
    where
      -- Reward for correct answers and punishment for giving up.
      factor         = 5
      -- Ignore responses given when the recall probability is higher than this.
      -- This happens when the above factor is increased.
      spuriousCutoff = 0.5
      -- Penalty for giving an incorrect answer.
      guessPenalty   = 0.9










data RecallCurve = RecallCurve
  { rcSuccessfulRecall :: UTCTime
  , rcStability        :: Integer
  } deriving ( Read, Show, Typeable, Eq )

markSuccess :: UTCTime -> RecallCurve -> RecallCurve
markSuccess now curve = curve{rcSuccessfulRecall = now}

bumpStability :: Rational -> RecallCurve -> RecallCurve
bumpStability factor curve =
  curve{rcStability = max 60 (round (fromIntegral (rcStability curve) * factor))}

setEffectiveStability :: UTCTime -> RecallCurve -> RecallCurve
setEffectiveStability now curve =
    curve{rcStability = max (rcStability curve) effectiveStability}
  where
    effectiveStability = round (diffUTCTime now (rcSuccessfulRecall curve))

-- R = e^(-t/S)
-- t = S(log(1/R))
-- Calculate when the predicted recall will reach 'likelihood' %.
-- Invariant: recallLikelihood (recallWhen likelihood curve) curve = likelihood
-- Invariant: recallWhen (recallLikelihood ts curve) curve = ts
recallWhen :: Double -> RecallCurve -> UTCTime
recallWhen likelihood (RecallCurve ts stability) =
  addUTCTime (realToFrac (fromIntegral stability * log (recip likelihood))) ts

-- R = e^(-t/S)
recallLikelihood :: UTCTime -> RecallCurve -> Double
recallLikelihood future (RecallCurve ts stability) =
    exp (negate (t/fromIntegral stability))
  where
    t = realToFrac $ diffUTCTime future ts