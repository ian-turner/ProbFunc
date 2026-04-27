module BlackJack where

import DistMonad


data Card = Ace | Two | Three | Four | Five           -- | Defining cards
          | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
          deriving (Show, Eq)                     
data Hand = H Int Bool deriving (Show, Eq)            -- | Hand of player: count, hard/soft flag (ace or not)
data Action = Hit | Stay deriving (Show, Eq)          -- | Hit (draw another card) or stay (no more cards)
data Outcome = Win | Lose | Push deriving (Show, Eq)  -- | Outcome of hand - either win or lose
data GameState = G Hand Hand deriving (Show, Eq)      -- | Game state object: holds player hand and dealer hand
type Strategy = (GameState -> Action)                 -- | Choice of action based on player hand and dealer card


-- | Functions for game actions - drawing cards, hit/stay, etc.
drawCard :: Dist Card
drawCard = uniform [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]

cardValue :: Card -> Int
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10

emptyHand :: Hand
emptyHand = (H 0 False)

emptyGame :: GameState
emptyGame = G emptyHand emptyHand

playerCount :: GameState -> Int
playerCount (G (H c _) _) = c

dealerCount :: GameState -> Int
dealerCount (G _ (H c _)) = c

addToHand :: Card -> Hand -> Hand
addToHand Ace (H c s) =
  if (s) then (H (c+1) True)
  else let nc = c+11 in
    if (nc > 21) then (H (c+1) False)
    else (H nc True)
addToHand x (H c s) = H (c+(cardValue x)) s

createHand :: [Card] -> Hand
createHand xs = foldl (\x y -> addToHand y x) emptyHand xs

hitHand :: Hand -> Dist Hand
hitHand h = do
  nc <- drawCard
  return $ addToHand nc h

playerHit :: GameState -> Dist GameState
playerHit (G pl dl) = do
  pl' <- hitHand pl
  return $ G pl' dl

dealerHit :: GameState -> Dist GameState
dealerHit (G pl dl) = do
  dl' <- hitHand dl
  return $ G pl dl'

initGame :: Dist GameState
initGame = do
  let gs = emptyGame
  gs     <- playerHit gs
  gs     <- playerHit gs
  gs     <- dealerHit gs
  return gs

runPlayer :: GameState -> Strategy -> Dist GameState
runPlayer gs strat = let (act, plCnt) = (strat gs, playerCount gs) in do
  if (plCnt >= 21 || act == Stay) then
    return gs
  else
    (playerHit gs) >>= (\gs' -> runPlayer gs' strat)

runDealer :: GameState -> Dist GameState
runDealer gs =
  let dc = dealerCount gs in
  if (dc < 17) then do
    gs' <- dealerHit gs
    runDealer gs'
  else
    return gs

runStrategy :: GameState -> Strategy -> Dist Outcome
runStrategy gs strat = do
  gs'  <- runPlayer gs strat
  gs'' <- runDealer gs'
  let pc = playerCount gs''
  let dc = dealerCount gs''
  if (pc > 21 || (dc <= 21 && dc > pc)) then
    return Lose
  else if (dc > 21 || pc > dc) then
    return Win
  else
    return Push

expVal :: Dist Outcome -> Float
expVal (D d) = sum [p * (val x) | (x,p) <- d]
  where
    val Win = 1.0
    val Push = 0.5
    val Lose = 0.0
