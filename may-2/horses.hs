import qualified Data.IntMap as M
import Data.Maybe

-- data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq, Show, Ord)

data HorsePosition = Scratched Int | Distance Int deriving (Show)

data Horse = Horse { position :: HorsePosition
                   , finish :: Int
                   } deriving (Show)

advanceHorse :: Horse -> Horse
advanceHorse h = case position h of
                    Scratched _ -> error "can't advance scratched horsed"
                    Distance d  -> h { position = Distance (d + 1) }

finished :: Horse -> Bool
finished h = case position h of
                    Scratched _ -> False
                    Distance d  -> d == finish h

data RoundState = RoundState { horses :: M.IntMap Horse
                             } deriving (Show)

makeHorse :: Int -> Horse
makeHorse finishSpot = Horse{position = Distance 0, finish = finishSpot}

makeRound :: RoundState
makeRound = RoundState { horses = M.fromList pairs }
            where pairs = zip lanes $ map makeHorse lengths
                  lanes = [2..12]
                  lengths = [3,4,5,6,7,8,7,6,5,4,3]

playTurn :: Int -> RoundState -> RoundState
playTurn number rs = rs{horses = M.adjust advanceHorse number (horses rs)}

winner :: RoundState -> Maybe (Int, Horse)
winner = listToMaybe . filter (finished . snd) . M.toList . horses

playRound :: [Int] -> [RoundState]
playRound = takeTurns makeRound
          where takeTurns rs (roll:rolls) = rs : takeTurns (playTurn roll rs) rolls
                takeTurns rs [] = [rs]

-- playRound should return a list of RoundStates up to and including the winning state

-- (head . snd) $ span (isNothing . winner) $ playRound [2,2,2]


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