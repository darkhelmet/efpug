{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.IntMap as M
import Data.List
import Data.Maybe
import System.Random

-- data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq, Show, Ord)

-- Bitchin' type safety!!
-- HOLY SHIT COMPILE TIME!!!!..............ladies???
newtype Chips = Chips Int deriving (Eq, Ord, Num)

instance Show Chips where
  show (Chips c) = "$" ++ show c

data HorsePosition = Scratched Chips | Distance Int deriving (Show)

data Horse = Horse { position :: HorsePosition
                   , finish :: Int
                   } deriving (Show)

data Player = Player { name :: String
                     , chips :: Chips
                     } deriving (Show)

player :: String -> Player
player n = Player { name = n, chips = 100 }

credit :: Chips -> Player -> Player
credit c p = p { chips = chips p + c }

debit :: Player -> Chips -> (Player, Chips)
debit p c | chips p >= c = (p { chips = chips p - c }, c)
          | otherwise = (p { chips = 0 }, chips p)

newtype Players = Players (M.IntMap Player, Int) deriving Show

fromList :: [Player] -> Players
fromList ps = Players (M.fromList $ zip [0..] ps, 0)

toList :: Players -> [Player]
toList (Players (m, _)) = map snd $ M.toList m

currentPlayer :: Players -> Player
currentPlayer (Players (m,i)) = m M.! i

advancePlayer :: Players -> Players
advancePlayer (Players (m,i)) = Players (m, (i+1) `mod` M.size m)

modifyAll :: (Player -> Player) -> Players -> Players
modifyAll tx (Players (m,i)) = Players (M.map tx m, i)

modifyCurrent :: (Player -> (Player, a)) -> Players -> (Players, a)
modifyCurrent tx players@(Players (m,i)) = (updatedPlayers, stuff)
                where updatedPlayers = Players (M.insert i updatedPlayer m, i)
                      (updatedPlayer, stuff) = tx $ currentPlayer players

advanceHorse :: Horse -> Horse
advanceHorse h = case position h of
                    Scratched _ -> error "can't advance scratched horsed"
                    Distance d  -> h { position = Distance (d + 1) }

scratchHorse :: Chips -> Horse -> Horse
scratchHorse cost horse = horse{position = Scratched cost}

finished :: Horse -> Bool
finished h = case position h of
                    Scratched _ -> False
                    Distance d  -> d == finish h

data RoundState = RoundState { horses :: M.IntMap Horse
                             , pot :: Chips
                             , players :: Players } deriving (Show)

padString :: Int -> String -> String
padString width str
    | length str < width = replicate (width - length str) ' ' ++ str
    | otherwise = str

showLane :: Int -> Int -> String
showLane 0 len = replicate len '-'
showLane pos len = replicate (pos - 1) '-' ++ "@" ++ replicate (len - pos) '-'

showHorse :: (Int, Horse) -> String
showHorse (number, Horse pos fin) = "Horse " ++ padString 2 (show number) ++ ": " ++ suffix
                                  where suffix = case pos of
                                                    Scratched c -> replicate fin 'x' ++ " " ++ show c
                                                    Distance  d -> showLane d fin

showPlayer :: Player -> String
showPlayer p = name p ++ ": " ++ show (chips p)

showState :: RoundState -> String
showState rs = showHorses rs ++ "\n-----\n" ++ showPlayers (players rs) ++ "\n" ++ showPot rs

showHorses :: RoundState -> String
showHorses = concat . intersperse "\n" . map showHorse . M.toList . horses

showPlayers :: Players -> String
showPlayers = intercalate "\n" . map showPlayer . toList

showPot :: RoundState -> String
showPot rs = "Pot: " ++ show (pot rs)

makeHorse :: Int -> Horse
makeHorse finishSpot = Horse{position = Distance 0, finish = finishSpot}

makeRound :: Players -> RoundState
makeRound ps = RoundState { horses = M.fromList pairs,
                            pot = 0,
                            players = ps }
            where pairs = zip lanes $ map makeHorse lengths
                  lanes = [2..12]
                  lengths = [3,4,5,6,7,8,7,6,5,4,3]

isScratched :: Horse -> Bool
isScratched horse = case position horse of
                        Scratched _ -> True
                        Distance  _ -> False

rollHitScratch :: Int -> RoundState -> Bool
rollHitScratch number rs = isScratched $ horses rs M.! number

countScratched :: RoundState -> Int
countScratched rs = length $ filter isScratched $ M.elems $ horses rs

allScratched :: RoundState -> Bool
allScratched rs = 4 == (countScratched rs)

scratchValue :: RoundState -> Chips
scratchValue rs = 5 * (1 + fromIntegral (countScratched rs))

playTurn :: Int -> RoundState -> RoundState
playTurn roll rs
    | rollHitScratch roll rs =
        rs { players = players', pot = paid + pot rs }
    | allScratched rs = rs{horses = M.adjust advanceHorse roll (horses rs)}
    | otherwise = rs{horses = M.adjust (scratchHorse (scratchValue rs)) roll (horses rs)}
  where charge = flip debit (scratchValue rs)
        (players', paid) = modifyCurrent charge (players rs)

nextTurn :: RoundState -> RoundState
nextTurn rs = rs { players = advancePlayer (players rs) }

winner :: RoundState -> Maybe (Int, Horse)
winner = listToMaybe . filter (finished . snd) . M.toList . horses

makeDiceRolls :: Int -> [Int]
makeDiceRolls seed = map (\r -> (r `mod` 6) + 1) $ randoms (mkStdGen seed) :: [Int]

makePlayerRolls :: [Int] -> [Int]
makePlayerRolls (first:second:rest) = (first + second):makePlayerRolls rest

playRound :: Players -> [Int] -> [RoundState]
playRound ps rolls = losers ++ [first]
                 where states = playRound' ps rolls
                       (losers,first:_) = span (isNothing . winner) states

playRound' :: Players -> [Int] -> [RoundState]
playRound' ps = takeTurns (makeRound ps)
          where takeTurns rs (roll:rolls) =
                  rs : takeTurns (nextTurn $ playTurn roll rs) rolls
                takeTurns rs [] = [rs]

prettyRound :: [Int] -> IO ()
prettyRound = putStrLn . intercalate "\n=====\n" . map showState . playRound ps
  where ps = fromList $ map player $ ["Daniel", "Justin", "Benny", "Dale", "Kevin"]

main = prettyRound $ makePlayerRolls $ makeDiceRolls 2

{-
1. Get deck of cards, 2 dice
2. Remove Aces, Kings, Jacks from deck
3. Start with $100 for each player
4. Each round:
   1. shuffle and deal deck
   2. starting left of dealer, roll both dice 4 times, "scratching" 4 horses. they go back to $5, $10, $15, $20 spots
   3. If you have cards of any horses that were scratched, pay that amount into the pot (times number of cards you have)
   4. starting left of dealer, roll dice, either move horse forward one spot, or pay into the pot if the horse is scratched
   5. when a horse wins, divide up pot. 4 cards win, each card gets 25% of the pot. divide pot evenly, leaving remainder in the pot for the next round.
-}