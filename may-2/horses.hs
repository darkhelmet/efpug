import qualified Data.IntMap as M
import Data.List
import Data.Maybe
import System.Random

-- data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq, Show, Ord)

data HorsePosition = Scratched Int | Distance Int deriving (Show)

data Horse = Horse { position :: HorsePosition
                   , finish :: Int
                   } deriving (Show)

advanceHorse :: Horse -> Horse
advanceHorse h = case position h of
                    Scratched _ -> error "can't advance scratched horsed"
                    Distance d  -> h { position = Distance (d + 1) }

scratchHorse :: Int -> Horse -> Horse
scratchHorse cost horse = horse{position = Scratched cost}

finished :: Horse -> Bool
finished h = case position h of
                    Scratched _ -> False
                    Distance d  -> d == finish h

data RoundState = RoundState { horses :: M.IntMap Horse
                             } deriving (Show)

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

showState :: RoundState -> String
showState = concat . intersperse "\n" . map showHorse . M.toList . horses

makeHorse :: Int -> Horse
makeHorse finishSpot = Horse{position = Distance 0, finish = finishSpot}

makeRound :: RoundState
makeRound = RoundState { horses = M.fromList pairs }
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

scratchValue :: RoundState -> Int
scratchValue rs = 5 * (1 + countScratched rs)

playTurn :: Int -> RoundState -> RoundState
playTurn roll rs
    | rollHitScratch roll rs = rs
    | allScratched rs = rs{horses = M.adjust advanceHorse roll (horses rs)}
    | otherwise = rs{horses = M.adjust (scratchHorse (scratchValue rs)) roll (horses rs)}

winner :: RoundState -> Maybe (Int, Horse)
winner = listToMaybe . filter (finished . snd) . M.toList . horses

playRound' :: [Int] -> [RoundState]
playRound' = takeTurns makeRound
          where takeTurns rs (roll:rolls) = rs : takeTurns (playTurn roll rs) rolls
                takeTurns rs [] = [rs]

makeDiceRolls :: Int -> [Int]
makeDiceRolls seed = map (\r -> (r `mod` 6) + 1) $ randoms (mkStdGen seed) :: [Int]

makePlayerRolls :: [Int] -> [Int]
makePlayerRolls (first:second:rest) = (first + second):makePlayerRolls rest

playRound :: [Int] -> [RoundState]
playRound rolls = losers ++ [first]
                 where states = playRound' rolls
                       (losers,first:_) = span (isNothing . winner) states

-- prettyRound $ makePlayerRolls $ makeDiceRolls 2
prettyRound :: [Int] -> IO ()
prettyRound = putStrLn . concat . intersperse "\n=====\n" . map showState . playRound

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