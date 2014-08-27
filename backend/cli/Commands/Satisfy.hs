module Commands.Satisfy
  ( cmdSatisfy ) where

import           Data.Chinese.CCDict          as CCDict
import           Data.Chinese.Frequency
import           Data.Either
import           Data.List
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import Data.Text (Text)
import qualified Data.Text.IO                 as T
import qualified Data.Yaml                    as Yaml
import           LessChobo.SimulatedAnnealing
import           LessChobo.Stencils
import           System.FilePath
import           System.IO
import           System.IO.Unsafe
import           System.Random.MWC
import           Text.Printf

import           Commands.Load                (readStencilDB)

cmdSatisfy :: Bool -> [FilePath] -> FilePath -> IO ()
cmdSatisfy _verbose basePaths path =
  mkStoryStencils basePaths path



--sortAllStencils :: Int -> IO [Stencil]
--sortAllStencils iterations = do
--    putStr "Loading ccdict... "
--    _ <- evaluate ccDict
--    putStrLn "Done!"
--    stencils <- readStencilDB
--    let byCost = map fst $ sortStencilsByCost stencils
--    putStrLn $ "Stencils: " ++ show (length stencils)
--    putStrLn $ "Unsorted: " ++ show (stencilPathCost stencils)
--    putStrLn $ "By cost:  " ++ show (stencilPathCost byCost)
--    let loop 0 saStencils = return saStencils
--        loop n saStencils = do
--          seed <- withSystemRandom $ asGenIO save
--          let sorted = sortStencilsWithSA seed saStencils
--          putStrLn $ "By SA:    " ++ show (stencilPathCost sorted)
--          loop (n-1) sorted
--    loop iterations byCost

-- Create Chinese stencils covering the concepts in a short story or poem.
-- The stencils are written in YAML to {input-file}.yaml
mkStoryStencils :: [FilePath] -> FilePath -> IO ()
mkStoryStencils assumedFiles inputFile = do
    putStrLn "Loading Chinese stencils..."
    stencils <- readStencilDB
    text <- T.readFile inputFile
    assumedTexts <- mapM T.readFile assumedFiles
    let assumedWords = Set.fromList
          [ entry
          | KnownWord entry <- tokenizer ccDict (T.concat assumedTexts)]
        textWords = nub
          [ entry
          | KnownWord entry <- tokenizer ccDict text
          , entry `Set.notMember` assumedWords ]
        textWordsN = length textWords

        cover = nub $ satisfyWithStencils textWords stencils
        totalWords = length cover
        coveredWords = length $ rights cover
        coveredWordsP = (coveredWords*100) `div` totalWords
        missingWords = length $ lefts cover
        
        
        stencilWords = nub
          [ entry | Chinese chinese _english <- rights cover
          , KnownWord entry <- tokenizer ccDict chinese ]
        superfluousWords = length (stencilWords \\ textWords)
        superfluousWordsP =
          ((superfluousWords)*100) `div` textWordsN
    printf "Word coverage:     %3d%% (missing %d)\n" coveredWordsP missingWords
    printf "Superfluous words: %3d%% (%d)\n" superfluousWordsP superfluousWords
    putStrLn "Sorting stencils..."
    let sorted = map fst $ sortStencilsByCost $ rights cover
    Yaml.encodeFile outputFile sorted
    putStrLn $ "Stencils written to: " ++ outputFile
  where
    outputFile = inputFile <.> "yaml"

--testSatisfy :: IO ()
--testSatisfy = do
--    stencils <- concat <$> mapM loadStencils paths
--    text <- T.readFile "../data/cn_story.pigs_picnic.txt"
--    --text <- T.readFile "../data/cn_story.doctor_monkey.txt"
--    --text <- T.readFile "../data/cn_story.foolish_affair.txt"
--    --text <- T.readFile "../data/cn_story.umbrella_flowers.txt"
--    let cover = satisfyWithStencils text stencils
--    print $ length $ nub $ lefts $ satisfyWithStencils text stencils
--    print $ length $ nub $ rights $ satisfyWithStencils text stencils
--    mapM_ T.putStrLn $ nub [ word | Left word <- satisfyWithStencils text stencils ]
--    -- mapM_ T.putStrLn $ nub [ chinese | Right (Chinese chinese _) <- cover ]
--    seed <- withSystemRandom $ asGenIO save
--    --let sorted = sortStencilsWithSA seed (nub $ rights cover)
--    -- mapM_ T.putStrLn $ nub [ chinese | Chinese chinese _ <- sorted ]
--    return ()
--  where
--    paths =
--      ["../data/cn_stencils.yaml"
--      ,"../data/cn_stencils.3800.yaml"
--      ,"../data/cn_stencils.20000.yaml"
--      ,"../data/cn_stencils.chinesepod.basic.yaml"
--      ,"../data/cn_stencils.rocket.yaml"
--      ,"../data/cn_stencils.livelingua.yaml" ]

satisfyWithStencils :: [Entry] -> [Stencil] -> [Either Chinese Stencil]
satisfyWithStencils entries stencils =
    [ case Map.lookup entry keyMap of
        Nothing       -> Left (entryChinese entry)
        Just covers -> Right (selectCheapest $ Set.toList covers)
    | entry <- entries ]
  where
    seen = Set.fromList (map entryChinese entries)
    -- selectCheapest = fst . head . sortStencilsByCost
    selectCheapest = fst . head . sortStencilsByFrequency seen
    keyMap = Map.fromListWith Set.union
      [ (entry, Set.singleton stencil)
      | stencil@(Chinese chinese _english) <- stencils
      , KnownWord entry <- tokenizer ccDict chinese ]






wordCost :: Double
wordCost      = 1

characterDiscount :: Char -> Double
characterDiscount _ = 0.1
--characterDiscount char =
--  case Map.lookup (T.singleton char) subtlex of
--    Nothing    -> 0
--    Just{}     -> 0.1 --  + recip (fromIntegral $ subtlexWCount entry)

phraseCost :: Set Char -> Set Entry -> [Token] -> Double
phraseCost knownCharacters knownWords tokens = sum
  [ wordCost -
    T.foldl' worker 0 (entryChinese entry)
  | KnownWord entry <- tokens, entry `Set.notMember` knownWords ]
  where
    worker n character
      | character `Set.member` knownCharacters
      = n + characterDiscount character
      | otherwise
      = n

phraseFreqCost :: Set Text -> [Token] -> Double
phraseFreqCost seen tokens =
  foldr max 0 $
  map (recip . wordFrequency) $
  nub [ entryChinese entry
      | KnownWord entry <- tokens
      , entryChinese entry `Set.notMember` seen ]

{-# NOINLINE traceSingle #-}
traceSingle :: String -> a -> a
traceSingle msg val = unsafePerformIO $ do
  putStr (msg ++ "                \r")
  hFlush stdout
  return val

sortStencilsByCost :: [Stencil] -> [(Stencil, Double)]
sortStencilsByCost stencils =
    worker (0::Int) Set.empty Set.empty Set.empty initCostMap
  where
    allStencilKeys =
      [ (stencil, tokenizer ccDict chinese)
      | stencil@(Chinese chinese _) <- stencils ]
    initCostMap = Map.fromList
      [ (key, phraseCost Set.empty Set.empty token)
      | key@(_stencil, token) <- allStencilKeys ]

    revMap = Map.fromListWith Set.union
      [ (key, Set.singleton item)
      | item@(_stencil, tokens) <- allStencilKeys
      , key <- tokenKeys tokens ]

    tokenKeys tokens = nub $ concat
      [ [chinese] ++ T.chunksOf 1 chinese
      | KnownWord entry <- tokens
      , let chinese = entryChinese entry ]

    worker n knownTokens knownCharacters knownWords costMap =
      case sortBy (comparing snd) (Map.toList costMap) of
        [] -> []
        (((stencil, tokens),score):_) ->
          let newCharacters    =
                  [ char | KnownWord entry <- tokens
                  , char <- T.unpack (entryChinese entry) ]
              knownCharacters' = knownCharacters `Set.union` Set.fromList newCharacters
              newWords         = [ entry | KnownWord entry <- tokens ]
              knownWords'      = knownWords `Set.union` Set.fromList newWords
              knownTokens' = Set.union knownTokens (Set.fromList (tokenKeys tokens))
              dirtyItems = Set.unions
                [ Map.findWithDefault Set.empty key revMap
                | key <- tokenKeys tokens
                , key `Set.notMember` knownTokens ]
              costMap' = Map.delete (stencil, tokens) costMap
              newCosts = Map.fromList
                [ (item, phraseCost knownCharacters' knownWords' thisTokens)
                | item@(_stencil, thisTokens) <- Set.toList dirtyItems
                , item `Map.member` costMap'
                ]
              newCostMap = newCosts `Map.union` costMap'
          in traceSingle ("Score: " ++ show (n, Set.size dirtyItems, Map.size costMap)) $
            (stencil, score) : worker (n+1) knownTokens' knownCharacters' knownWords' newCostMap

sortStencilsByFrequency :: Set Text -> [Stencil] -> [(Stencil, Double)]
sortStencilsByFrequency seen0 stencils =
  worker seen0
    [ (stencil, tokenizer ccDict chinese)
    | stencil@(Chinese chinese _) <- stencils ]
  where
    worker seen lst =
      case sortBy (comparing snd)
              [ ((stencil, tokens), phraseFreqCost seen tokens)
              | (stencil, tokens) <- lst ] of
        [] -> []
        (((stencil, tokens),score):xs) ->
          let seen' = Set.union seen $
                Set.fromList [ entryChinese entry | KnownWord entry <- tokens ]
          in (stencil, score) : worker seen' (map fst xs)

--sortStencilsWithSA :: Seed -> [Stencil] -> [Stencil]
--sortStencilsWithSA seed stencils =
--    [ stencils !! n | n <- testSA' seed (map stencilToFact stencils) ]

--stencilPathCost :: [Stencil] -> Int
--stencilPathCost = testPathCost . map stencilToFact

--stencilToFact :: Stencil -> [Int]
--stencilToFact (Chinese chinese _) =
--  mapMaybe (\w -> fmap subtlexIndex (Map.lookup w subtlex)) $ concat
--      [ entryChinese word : map T.singleton (T.unpack $ entryChinese word)
--      | KnownWord word <- tokenizer ccDict chinese ]

--frequencyTally :: Int -> [Stencil] -> [(T.Text, Int)]
--frequencyTally nWords stencils =
--    [ (w, Map.findWithDefault 0 w stencilMap)
--    | w <- freqWords
--    , fmap entryChinese (CCDict.lookup w ccDict) == Just w ]
--  where
--    stencilMap = Map.fromListWith (+)
--      [ (entryChinese entry, 1)
--      | Chinese chinese _ <- stencils, KnownWord entry <- nub $ tokenizer ccDict chinese ]
--    freqWords =
--        map subtlexWord $
--        take nWords $
--        sortBy (comparing (negate . subtlexWCount)) $ Map.elems subtlex



wordFrequency :: T.Text -> Double
wordFrequency txt =
  case Map.lookup txt subtlex of
    Just entry -> subtlexWMillion entry
    Nothing -> foldr min 0 $ catMaybes
      [ fmap subtlexWMillion (Map.lookup chunk subtlex)
      | chunk <- T.chunksOf 1 txt ]
