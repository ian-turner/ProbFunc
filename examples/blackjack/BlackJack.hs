module BlackJack where

import DistMonad


data Card = Ace | Two | Three | Four | Five       -- | Defining cards
          | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
          deriving (Show, Eq)                     
data Hand = H Int Bool deriving (Show, Eq)        -- | Hand of player: count, hard/soft flag (ace or not)
data Action = Hit | Stay deriving (Show, Eq)      -- | Hit (draw another card) or stay (no more cards)
data Outcome = Win | Lose deriving (Show, Eq)     -- | Outcome of hand - either win or lose
type Strategy = (Hand -> Card -> Action)          -- | Choice of action based on player hand and dealer card

-- | Functions for game actions - drawing cards, hit/stay, etc.
drawCard :: Dist Card
drawCard = uniform [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]
