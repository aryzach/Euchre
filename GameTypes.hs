module GameTypes where

import qualified Data.Set as S 
import qualified Data.Map as M 

{-
State Teams Game
Game Hand Score
Trick LeadPlayer Turn Winner CardsPlayed
Hand Dealer Trump FaceUp Trick Score 
-}

data State  = S Players Game deriving (Show)
data Game   = G Hand GameScore deriving (Show)
--hand needs to have players with cards
data Hand   = StartOfHand Dealer FaceUp | H Dealer Trump TrumpCaller Trick HandScore deriving (Show)
data Trick  = StartOfTrick | T LeadPlayer CurrentPlayer Winner CardsPlayed deriving (Show)
--players should not have cards here, because then in updateHandScore, players w/ same name but diff cards are unique
data Player = P ID Name [Card] deriving (Show, Eq,Ord)
data FourOf a = F a a a a deriving (Show) 
data Card   = C Suit Value deriving (Show, Eq, Read, Ord)
data Suit   = Hrt | Dia | Spd | Clb deriving (Show, Eq, Read, Enum, Bounded) 
data Value  = Nine | Ten | J | Q | K | A deriving (Show, Eq, Read, Ord, Enum, Bounded) 

instance Ord Suit where
 compare _ _ = EQ

instance Functor FourOf where
 fmap f (F a b c d) = F (f a) (f b) (f c) (f d)  

instance Applicative FourOf where
 pure a = F a a a a
 (F f g h i) <*> (F a b c d) = F (f a) (g b) (h c) (i d) 


type CardsPlayed = M.Map Player Card 
type Players = FourOf Player 
type Trump   = Suit
type TrumpCaller = Player
type FaceUp  = Maybe Card
type Team    = S.Set Player  
type Teams   = S.Set Team 
type Score   = M.Map Team Int 
type Dealer  = Player
type HandScore = Score
type GameScore = Score
type LeadPlayer = Player
type CurrentPlayer = Player 
type Winner = Maybe Player 
type Name = String
type ID = Int
