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

newtype Card = Card Int deriving (Enum, Eq, Num, Show)

deck :: [Card]
deck = concat $ replicate 4 [2..12]

data HorsePosition = Scratched Chips | Distance Int deriving (Show)

data Horse = Horse { position :: HorsePosition
                   , finish :: Int
                   } deriving (Show)

data Player = Player { name :: String
                     , chips :: Chips
                     , hand :: [Card]
                     } deriving (Show)

player :: String -> Player
player n = Player { name = n, chips = 100, hand = [] }

credit :: Chips -> Player -> Player
credit c p = p { chips = chips p + c }

debit :: Player -> Chips -> (Player, Chips)
debit p c | chips p >= c = (p { chips = chips p - c }, c)
          | otherwise = (p { chips = 0 }, chips p)

newtype Players = Players [Player] deriving Show

fromList :: [Player] -> Players
fromList = Players

toList :: Players -> [Player]
toList (Players l) = l

currentPlayer :: Players -> Player
currentPlayer = head . toList

advancePlayer :: Players -> Players
advancePlayer (Players (p:ps)) = Players (ps ++ [p])

modifyAll :: (Player -> Player) -> Players -> Players
modifyAll f = fromList . map f . toList

modifyCurrent' :: (Player -> (Player, a)) -> Players -> (Players, a)
modifyCurrent' tx (Players (p:ps)) = (fromList (updatedPlayer:ps), stuff)
    where (updatedPlayer, stuff) = tx p
          
modifyCurrent :: (Player -> Player) -> Players -> Players
modifyCurrent tx (Players (p:ps)) = fromList (tx p:ps)

dropPlayer :: Players -> Players
dropPlayer (Players (p:ps)) = fromList ps

countPlayers :: Players -> Int
countPlayers (Players ps) = length ps

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
                             , previousRoll :: Int
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

showRoll :: RoundState -> String
showRoll rs = name player ++ " rolled a " ++ show roll ++ "!"
    where player = currentPlayer $ players rs
          roll = previousRoll rs

showState :: RoundState -> String
showState rs = showRoll rs ++ "\n" ++ showHorses rs ++ "\n-----\n" ++ showPlayers (players rs) ++ "\n" ++ showPot rs

showHorses :: RoundState -> String
showHorses = concat . intersperse "\n" . map showHorse . M.toList . horses

showPlayers :: Players -> String
showPlayers = intercalate "\n" . map showPlayer . toList

showPot :: RoundState -> String
showPot rs = "Pot: " ++ show (pot rs)

summarize :: RoundState -> String
summarize rs = intercalate "\n" . map (showPlayerHand rs) . (toList . players) $ rs
               
showPlayerHand rs player = name player ++ " has " ++ show howMany ++ suffix
  where howMany = length . filter (==fromIntegral lastRoll) . hand $ player
        lastRoll = previousRoll rs
        suffix = if (howMany > 0) then " winnah!" else "... :("

makeHorse :: Int -> Horse
makeHorse finishSpot = Horse{position = Distance 0, finish = finishSpot}

makeRound :: Players -> RoundState
makeRound ps = RoundState { horses = M.fromList pairs,
                            pot = 0,
                            players = ps,
                            previousRoll = 0 }
            where pairs = zip lanes $ map makeHorse lengths
                  lanes = [2..12]
                  lengths = [3,4,5,6,7,8,7,6,5,4,3]

isScratched :: Horse -> Bool
isScratched horse = case position horse of
                        Scratched _ -> True
                        Distance  _ -> False

countScratched :: RoundState -> Int
countScratched rs = length $ filter isScratched $ M.elems $ horses rs

allScratched :: RoundState -> Bool
allScratched rs = 4 == (countScratched rs)

nextScratchValue :: RoundState -> Chips
nextScratchValue rs = 5 * (1 + fromIntegral (countScratched rs))

scratchValue :: Int -> RoundState -> Maybe Chips
scratchValue roll rs = value pos
    where horse = (horses rs) M.! roll
          pos = position horse
          value (Scratched n) = Just n
          value (Distance _) = Nothing

playTurn :: Int -> RoundState -> RoundState
playTurn roll rs
    | isJust sc = rs { players = players', pot = paid + pot rs, previousRoll = roll }
    | allScratched rs = rs{horses = M.adjust advanceHorse roll (horses rs), previousRoll = roll }
    | otherwise = rs{horses = M.adjust (scratchHorse (nextScratchValue rs)) roll (horses rs), previousRoll = roll }
  where charge = flip debit $ fromJust sc
        sc = scratchValue roll rs
        (players', paid) = modifyCurrent' charge (players rs)

nextTurn :: RoundState -> RoundState
nextTurn rs
    | currentPlayerIsBroke = rs { players = dropPlayer ps  }
    | otherwise = rs { players = advancePlayer ps }
    where ps = players rs
          currentPlayerIsBroke = 0 == chips cp
          cp = currentPlayer ps

winner :: RoundState -> Maybe (Int, Horse)
winner = listToMaybe . filter (finished . snd) . M.toList . horses

makeDiceRolls :: Int -> [Int]
makeDiceRolls seed = map (\r -> (r `mod` 6) + 1) $ randoms (mkStdGen seed) :: [Int]

makePlayerRolls :: [Int] -> [Int]
makePlayerRolls (first:second:rest) = (first + second):makePlayerRolls rest

gameOver :: RoundState -> Bool
gameOver rs = onePlayerLeft || horseFinished
    where onePlayerLeft = 1 == (length $ toList $ players rs)
          horseFinished = isJust . winner $ rs

watchHorses :: Players -> [Int] -> [RoundState]
watchHorses ps rolls = losers ++ [first]
    where states = playTurns ps rolls
          (losers,first:_) = span (not . gameOver) states

playTurns :: Players -> [Int] -> [RoundState]
playTurns ps = takeTurns (makeRound ps)
          where takeTurns rs (roll:rolls) =
                  rs : takeTurns (nextTurn $ playTurn roll rs) rolls
                takeTurns rs [] = [rs]

everyNth :: Int -> [a] -> [a]
everyNth n (x:xs) = x:everyNth n (drop (n-1) xs)
everyNth n [] = []

deal :: [Card] -> Int -> [[Card]]
deal cards n = zipWith giveCards [0..n-1] (replicate n cards)
  where giveCards ix cards = everyNth n (drop ix cards)
        
handOutCards :: [Card] -> Players -> Players
handOutCards cards players = foldl' handCardsAndAdvance players hands
  where handCardsAndAdvance players hand = advancePlayer . modifyCurrent (giveHand hand) $ players
        giveHand hand player = player { hand = hand }
        hands = deal cards (countPlayers players)
        
playGame :: Int -> [RoundState]
playGame = watchHorses ps . makePlayerRolls . makeDiceRolls
  where ps = handOutCards deck $ fromList $ map player $ ["Daniel", "Justin", "Benny", "Dale", "Kevin"]

prettify :: [RoundState] -> IO ()
prettify rs = putStrLn $ showEachState rs ++ "\n\n" ++ summarize (last rs)
  where showEachState = intercalate "\n=====\n" . map showState

main = prettify . playGame $ 2

{-

- Remove players between rounds
- Cards: Make deck (2-12 only), shuffle, deal, pay when scratching
- Divide pot at end of round
- Win/finish game when 1 player left

-}

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