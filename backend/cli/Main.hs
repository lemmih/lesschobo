module Main ( main ) where

import           LessChobo.Common
import           LessChobo.Stencils

import           Control.Applicative
import           Control.Exception
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
import qualified Data.Text.IO                 as T
import           Data.Yaml                    as Yaml
import           LessChobo.SimulatedAnnealing
import           System.FilePath
import           System.IO
import           System.IO.Unsafe
import           System.Random.MWC
import           Text.Printf

import           Commands

main :: IO ()
main =
  runCommands
  --args <- getArgs
  --mapM_ mkStoryStencils args
  ----stencils <- sortAllStencils 0
  --return ()


