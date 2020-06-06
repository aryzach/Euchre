module Euchre where

import Shuffle
import Text.Read
import qualified Data.Map as M

--constructors, instance declarations, type synonyms
data Value = Nine | Ten | J | Q | K | A deriving (Show, Eq, Read, Ord, Enum, Bounded)
data Suit = Heart | Diamond | Spade | Club deriving (Show, Eq, Read, Enum, Bounded)
data Card = C Suit Value deriving (Show, Eq, Read, Ord)
data State = S (FourOf Player) CurrentPlayer Turn CurrentTrick Trump HandScore GameScore deriving (Show)
data Player = P String Team [Card] deriving (Show, Eq, Ord)
data FourOf a = F a a a a deriving (Show)
data HandScore = HS Int Int deriving (Show)
data GameScore = GS Int Int deriving (Show)

type Trump = Card
type Team = Int
type CurrentPlayer = Player
type CurrentTrick   = [(Player,Card)]
type Turn = Int -- in normal Trick, 4 turns happen. Control for ending the Trick

instance Ord Suit where
 compare _ _ = EQ

instance Functor FourOf where
 fmap f (F a b c d) = F (f a) (f b) (f c) (f d)  

instance Applicative FourOf where
 pure a = F a a a a
 (F f g h i) <*> (F a b c d) = F (f a) (g b) (h c) (i d) 

--game constants
allCards :: [Card]
allCards = [C s v | s <- [Heart .. Club], v <- [Nine .. A]]

playerOrder :: M.Map Player Player
playerOrder = M.fromList [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

startState :: State
startState = S allPlayers p2 0 [] card1 (HS 0 0) (GS 0 0) 

--test constants
p1,p2,p3,p4 :: Player
p1 = P "me"   1 [] 
p2 = P "you"  1 [] 
p3 = P "ok"   2 [C Spade Ten,C Club Q]
p4 = P "cool" 2 [C Heart K]
allPlayers :: FourOf Player
allPlayers = F p1 p2 p3 p4 
card1,card2,card3,card4,card5,card6,card7 :: Card
card1 = C Heart A
card2 = C Spade J
card3 = C Spade A
card4 = C Club  J
card5 = C Diamond J
card6 = C Club Nine
card7 = C Club Q

--IO, procedural logic
main :: IO ()
main = trickloop startState 

trickloop :: State -> IO ()
trickloop s = 
 do startTrickState <- dealCards s
    if (currentTurn s) < 4 
    then 
     do newState <- singleTurn startTrickState  
        trickloop newState 
    else
     do 
        putStrLn "trick over"
        return ()
   
singleTurn :: State -> IO State
singleTurn s = 
 do putStrLn $ "select card from your cards" ++ (show . getCards $ (currentPlayer s))
    stringInput <- getLine
    let readInput = readMaybe stringInput :: Maybe Card
    case readInput of
     Nothing -> do putStrLn "invalid"
                   return s
     Just c  -> if c `elem` (getCards $ currentPlayer s)
                 then
                  do
                   let newState = handleValidCardPlay c s
                   return newState
                 else
                  do putStrLn "you don't have that card"
                     return s

--data State = S Player Player Player Player CurrentPlayer Turn CurrentTrick HandScore GameScore
dealCards :: State -> IO State
dealCards (S players cp _ _ _ hs gs) = 
 do cards <- shuffle allCards
    let (d1,rest1) = getFive cards
        (d2,rest2) = getFive rest1
        (d3,rest3) = getFive rest2
        (d4,rest4) = getFive rest3
        trump      = getOne  rest4 
        dealtPlayers = (fmap dealPlayer (F d1 d2 d3 d4)) <*> players 
    return (S dealtPlayers (firstPlayer dealtPlayers) 0 [] trump hs gs)
   where getFive:: [Card] -> ([Card],[Card])
         getFive (a:b:c:d:e:cs) = ([a,b,c,d,e],cs)
         getFive _              = error "not enough cards to deal"
         getOne :: [Card] -> Card
         getOne (c:cs) = c
         getOne _      = error "trump card not available"
{-
 - can't do this until I have trump, can't get trump til I deal cards and handle that process, call this function on 'finishTrick'
scoreTrick :: State -> IO State 
scoreTrick (S p1 p2 p3 p4 _ _ allCards hs gs) =
 let (winName, winTeam) = sh' allCards

 where winTup :: [(Player,Card)] -> (Player,Card)
       winTup = foldr 
       sh :: [(Player,Card)] -> (String,Team)
       sh ac = (

finishTrick = scoreTrick, update score, update beginning player based on winning player 
-}

--pure logic
handleValidCardPlay :: Card -> State -> State
handleValidCardPlay c (S (F p1 p2 p3 p4) cp t ch trump hs gs) =  
 if      cp == p1 then S (F (removeCard c p1) p2 p3 p4) (playerOrder M.! cp) (t+1) ((p1,c) : ch) trump hs gs
 else if cp == p2 then S (F p1 (removeCard c p2) p3 p4) (playerOrder M.! cp) (t+1) ((p2,c) : ch) trump hs gs
 else if cp == p3 then S (F p1 p2 (removeCard c p3) p4) (playerOrder M.! cp) (t+1) ((p3,c) : ch) trump hs gs 
 else if cp == p4 then S (F p1 p2 p3 (removeCard c p4)) (playerOrder M.! cp) (t+1) ((p4,c) : ch) trump hs gs
 else error "current player not one of the four players"

dealPlayer :: [Card] -> Player -> Player
dealPlayer cs (P n t _) = P n t cs 

firstPlayer :: FourOf Player -> Player
firstPlayer (F p _ _ _) = p

removeCard :: Card -> Player -> Player
removeCard c (P t n cs) = P t n (remove c cs)
 where remove :: Card -> [Card] -> [Card]
       remove _ [] = []
       remove c (x:xs) = if x == c then xs else remove c xs

ordering :: Suit -> Suit -> Card -> Card -> Ordering
ordering trumpSuit leadSuit c1 c2 = 
 let s1 = getSuit c1
     s2 = getSuit c2
     v1 = getVal c1
     v2 = getVal c2
 in 
  if isTrump trumpSuit c1 && isTrump trumpSuit c2 then trumpOrdering trumpSuit c1 c1 
  else if isTrump trumpSuit c1 then GT
  else if isTrump trumpSuit c2 then LT
  else leadSuitOrdering leadSuit c1 c2 

leadSuitOrdering :: Suit -> Card -> Card -> Ordering
leadSuitOrdering = undefined

trumpOrdering :: Suit -> Card -> Card -> Ordering
trumpOrdering trump (C s1 v1) (C s2 v2) = 
 if v1 == v2 then 
  if s1 == trump then GT
  else LT
 else if v1 == J then GT
 else if v2 == J then LT
 else compare v1 v2

isTrump :: Suit -> Card -> Bool
isTrump trump (C s v) = 
 trump == s ||  
 case trump of
  Heart   -> (s == Diamond && v == J)
  Diamond -> (s == Heart   && v == J)
  Spade   -> (s == Club    && v == J)
  Club    -> (s == Spade   && v == J)

--getters
getCards :: Player -> [Card]
getCards (P _ _ cs) = cs 

currentPlayer :: State -> Player
currentPlayer (S _ cp _ _ _ _ _) = cp

currentTurn :: State -> Turn 
currentTurn (S _ _ t _ _ _ _) = t

getSuit :: Card -> Suit
getSuit (C s _) = s

getVal :: Card -> Value
getVal (C _ v) = v

getName :: Player -> String
getName (P n _ _) = n

getTeam :: Player -> Team
getTeam (P _ t _) = t


