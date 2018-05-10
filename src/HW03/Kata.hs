{-# OPTIONS -fwarn-tabs -fno-warn-type-defaults -fdefer-type-errors #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Kata where

import Data.Char as Char
import Data.List as List
import Data.Maybe as Maybe
import Text.Read  (readMaybe)
import Test.HUnit

data Weather = Weather {
  day :: Int, maxT :: Int, minT :: Int 
  } deriving (Eq, Show)

-- Access this information from tabulated strings
-- Note that there are more columns in the file, but we ignore the extras
-- and only work with the ones that we care about
instance FromRecord Weather where
  parseRecord (("DY",dy):("MAX",mx):("MIN",mn):_) = Just Weather <**> dy <**> mx <**> mn
  parseRecord _            = Nothing

-- compare two weather records based on the difference between their
-- minimum and maximum temperatures
cmpDiffTemp :: Weather -> Weather -> Ordering
cmpDiffTemp w1 w2 = compare (diff w1) (diff w2) where
    diff w = maxT w - minT w

weatherProgram :: IO ()
weatherProgram = do
   bytes <- readFile "jan.dat"
   let wdata :: [Weather]
       wdata = decode AlignRight bytes           -- parse the weather data
   let ans = day (minimumBy cmpDiffTemp wdata)   -- compute a stat
   putStrLn $ "The day with minimum temp change was " ++ show ans

-----------------------------------------------------------------

data Alignment  = AlignLeft | AlignRight

testSplitHeader = TestList
  [ splitHeader AlignRight "  Dy MxT   MnT   AvT   HDDay" ~?=
      [("Dy",4),("MxT",4),("MnT",6),("AvT",6),("HDDay",8)],
    splitHeader AlignLeft "       Team            P     W    L" ~?=
      [("",7),("Team",16),("P",6),("W",5),("L",1)]
   ]

-- | Use column headers to determine offsets for each row in a table
splitHeader :: Alignment -> String -> [(String,Int)]
splitHeader = undefined

splitRow :: [(String,Int)] -> String -> [(String,String)]
splitRow = undefined

testSplitRow = splitRow [("",7),("Team",16),("P",6),("L",5)]
                        "    1. Arsenal         38    26   " ~?=
               [("","1."),("Team","Arsenal"),("P","38"),("L","26")]

class FromRecord a where
  -- Convert a list of string elements into row
  parseRecord :: [(String,String)] -> Maybe a

-- | Decode a data file into a list of data rows
-- any rows in the data file that are unparseable are ignored
decode :: forall a. FromRecord a => Alignment -> String -> [a]
decode alignment str = Maybe.mapMaybe parseRecord (tabulate str) where
 tabulate :: String -> [[(String,String)]]
 tabulate str =
   case lines str of
    []      -> []
    hd:rows -> map (splitRow header) rows where
         header = splitHeader alignment hd

class FromField a where
  parseField :: String -> Maybe a

instance FromField Int where
  parseField = readMaybe

infixl 4 <**>
(<**>) :: FromField a => Maybe (a -> b) -> String -> Maybe b
Just f  <**> str = f `fmap` parseField str
Nothing <**> str = Nothing

instance FromField String where
  parseField = Just

data Event = Fog | IntenseFog | Thunder | Ice | Hail
           | FreezingRain | Duststorm | Smoke | BlowingSnow | Tornado
  deriving (Eq, Ord, Enum, Show)

instance FromField Event where
   
   parseField _   = Nothing

testParseEvent :: Test
testParseEvent = TestList [ parseField "1" ~?= Just Fog,
                            parseField "X" ~?= Just Tornado,
                            parseField "12" ~?= (Nothing :: Maybe Event) ]

newtype Events = Events { getEvents :: [Event] }

instance FromField Events where
   parseField str = fmap Events (sequence (map (parseField . (:[])) str))

-----------------------------------------------------------------



mostUnseasonable :: String -> Int
mostUnseasonable = undefined



numPrecip :: String -> Int
numPrecip = undefined



foggyDays :: String -> [Int]
foggyDays = undefined

-----------------------------------------------------------------

soccer :: String -> String
soccer = undefined
 

soccerProgram :: IO String
soccerProgram = do
   bytes <- readFile "soccer.dat"
   return (soccer bytes)



