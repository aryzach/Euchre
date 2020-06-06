module Euchre where

import Shuffle
import Text.Read
import qualified Data.Map as M
import qualified Data.Set as S
import GameTypes

--game constants
allCards :: [Card]
allCards = [C s v | s <- [Hrt .. Clb], v <- [Nine .. A]]

playerOrder :: M.Map Player Player
playerOrder = M.fromList [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

startState :: State
startState = S allPlayers startGame

startGame :: Game
--need to start game w/ random dealer and card
startGame = G (StartOfHand p1 (Just card1)) (M.fromList [(S.fromList [p1,p2],0),(S.fromList [p3,p4],0)])

--test constants
p1,p2,p3,p4 :: Player
p1 = P 1 "me"   [C Hrt Ten] 
p2 = P 2 "you"  [C Clb Ten] 
p3 = P 3 "ok"   [C Spd Ten,C Clb Q]
p4 = P 4 "cool" [C Hrt K]
allPlayers :: Players
allPlayers = F p1 p2 p3 p4 
card1,card2,card3,card4,card5,card6,card7 :: Card
card1 = C Hrt A
card2 = C Spd J
card3 = C Spd A
card4 = C Clb  J
card5 = C Dia J
card6 = C Clb Nine
card7 = C Clb Q

--IO, procedural logic
-- initialize game w/ players names, teams, and starting dealer
main :: IO ()
main = runGame startState 

runGame :: State -> IO ()
runGame s = if gameWinCondition s then 
 declareWinner s
 else playHand s 

playHand :: State -> IO () 
playHand s = 
 if handWinCondition s then declareHandOver s >>= runGame 
 else 
  dealCards (changeDealerS s) >>=
  decideTrump >>= 
  playTrick >>
  print "got here"
--  updateHandFromTrick (and also update players' cards,and dealer,and handscore) >>=
--  playHand

gameWinCondition :: State -> Bool
gameWinCondition s = any (>= 10) $ M.elems $ getGameScoreS s
 
handWinCondition :: State -> Bool
-- only cases (3 - 1 or 5 - 0 or 4 - 1)
handWinCondition (S _ (G h _)) = 
 case h of
  (H _ _ _ _ hs) -> 
   let scores = M.elems hs
   in ((sum scores) == 5) || ((any (==3) scores) && any (>=1) scores) 
  _ -> False

decideTrump :: State -> IO State 
decideTrump s =
 let dealer = getDealerS s
 in putStr "faced up card is " >>
 print (getFaceUpS s) >> 
 askAll (nextP dealer) (getHandS s) False >>= 
 \h -> let newState = updateStateH s h
 in return newState 
 where askAll :: Player -> Hand -> Bool -> IO Hand 
       --T/F indicates whether face up card, or choose any suit
       askAll p (StartOfHand d f) False = 
        if p == d 
        then promptPlayer p "'order up' or 'pass'" >>= 
             \str -> let (p',h) = handleSetTrump p d f str
                     in askAll p' h True 
        else promptPlayer p "'order up' or 'pass'" >>= 
             \str -> let (p',h) = handleSetTrump p d f str
                     in askAll p' h False 
       askAll p (StartOfHand d f) True  = 
        if p == d
        then promptPlayer p "screw the dealer, pick a suit" >>= 
             \str -> let (p',h) = handleSetTrump p d Nothing str
                     in askAll p' h True 
        else promptPlayer p "call suit or 'pass'" >>= 
             \str -> let (p',h) = handleSetTrump p d Nothing str
                     in askAll p' h True 
       askAll _ h _ = return h
 
handleSetTrump :: Player -> Player -> FaceUp -> String -> (Player,Hand)
handleSetTrump p d (Just (C s v)) str = 
 if str == "order up" then (p, (H d s p StartOfTrick M.empty))  
 else ((nextP p), (StartOfHand d (Just (C s v)))) 
handleSetTrump p d Nothing  str =
 let maybeSuit = readMaybe str :: Maybe Suit 
 in 
  case maybeSuit of
   (Just s) -> (p, (H d s p StartOfTrick M.empty))
   Nothing  -> if p == d then (p, (StartOfHand d Nothing)) 
               else (nextP p, (StartOfHand d Nothing)) 
     
       
updateStateH :: State -> Hand -> State 
updateStateH (S p (G _ gs)) h = S p (G h gs)  

promptPlayer :: Player -> String -> IO String
promptPlayer (P id n c) str = 
 putStr (n ++ ", your cards: ") >> print c >>
 putStr ("enter " ++ str) >>
 getLine >>= return 

changeDealerS :: State -> State
--change dealers only in not in 'StartOfHand'
changeDealerS (S p (G h gs)) = (S p (G (changeDealerH h) gs)) 
 where changeDealerH (H d t tc tr hs) = (H (nextP d) t tc tr hs) 
       changeDealerH (StartOfHand d fu) = StartOfHand d fu
 
declareHandOver :: State -> IO State
--reset to StartOfHand?
declareHandOver s = undefined

declareWinner :: State -> IO ()
declareWinner = undefined

playTrick :: State -> IO Trick 
playTrick s = 
 trickLoop (setupTrick s) >>=
 decideTrickWinner >>=
 return

setupTrick :: State -> Trick 
setupTrick s = 
 let p = nextP (getDealerS s)
 in (T p p Nothing M.empty)

decideTrickWinner :: Trick -> IO Trick
decideTrickWinner t = undefined

trickLoop :: Trick -> IO Trick 
trickLoop t = 
 if (getLeadPlayerT t) == (nextP (getCurrentPlayerT t))
 then return t 
 else
  putStrLn "played so far: " >>
  print (getCardsPlayedT t) >> 
  promptPlayer (getCurrentPlayerT t) "play a card" >>= 
  \str -> 
   case (readMaybe str :: Maybe Card) of
    Nothing  -> putStrLn "enter valid card" >> trickLoop t 
    (Just c) -> if (playerHasCard (getCurrentPlayerT t) c)
                then
                 let oneTurnTrick = updateCardsPlayed t c 
                 in trickLoop (advanceTrickPlayer oneTurnTrick) 
                else 
                 putStrLn "you don't have that card" >> trickLoop t

updateCardsPlayed :: Trick -> Card -> Trick
updateCardsPlayed (T lp cp w crds) c = (T lp cp w (M.insert cp c crds))  

advanceTrickPlayer :: Trick -> Trick
advanceTrickPlayer (T lp cp w crds) = (T lp (nextP cp) w crds)

playerHasCard :: Player -> Card -> Bool
playerHasCard (P _ _ cs) c = c `elem` cs

{-
trickLoop :: State -> IO State
trickLoop s =  
 do startTrickState <- dealCards s
    if (currentTurn s) < 4 
    then 
     do newState <- singleTurn startTrickState  
        trickloop newState 
    else
     do 
        putStrLn "trick over"
        return ()
-} 
{-
singlePlayerTurn :: State -> IO State
singlePlayerTurn s = 
 do putStrLn $ "select card from your cards" ++ (show . getCards $ (currentPlayerS s))
    stringInput <- getLine
    let readInput = readMaybe stringInput :: Maybe Card
    case readInput of
     Nothing -> do putStrLn "invalid"
                   return s
     Just c  -> if c `elem` (getCards $ currentPlayerS s)
                 then
                  do
                   let newState = handleValidCardPlay c s
                   return newState
                 else
                  do putStrLn "you don't have that card"
                     return s
-}
--data State = S Player Player Player Player CurrentPlayer Turn CurrentTrick HandScore GameScore
dealCards :: State -> IO State
dealCards (S players game) = 
 do cards <- shuffle allCards
    let (d1,rest1) = getFive cards
        (d2,rest2) = getFive rest1
        (d3,rest3) = getFive rest2
        (d4,rest4) = getFive rest3
        trump      = getOne  rest4 
        dealtPlayers = (fmap dealPlayer (F d1 d2 d3 d4)) <*> players 
    return (S dealtPlayers game)
    --maybe need to update game for first player and scores?
    --return (S dealtPlayers (firstPlayer dealtPlayers) 0 [] trump hs gs)
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
{-
handleValidCardPlay :: Card -> State -> State
handleValidCardPlay c (S (F p1 p2 p3 p4) cp t ch trump hs gs) =  
 if      cp == p1 then S (F (removeCard c p1) p2 p3 p4) (playerOrder M.! cp) (t+1) ((p1,c) : ch) trump hs gs
 else if cp == p2 then S (F p1 (removeCard c p2) p3 p4) (playerOrder M.! cp) (t+1) ((p2,c) : ch) trump hs gs
 else if cp == p3 then S (F p1 p2 (removeCard c p3) p4) (playerOrder M.! cp) (t+1) ((p3,c) : ch) trump hs gs 
 else if cp == p4 then S (F p1 p2 p3 (removeCard c p4)) (playerOrder M.! cp) (t+1) ((p4,c) : ch) trump hs gs
 else error "current player not one of the four players"
-}
dealPlayer :: [Card] -> Player -> Player
dealPlayer cs (P id n _) = P id n cs 

firstPlayer :: FourOf Player -> Player
firstPlayer (F p _ _ _) = p

removeCard :: Card -> Player -> Player
removeCard c (P id n cs) = P id n (remove c cs)
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
  Hrt   -> (s == Dia && v == J)
  Dia -> (s == Hrt   && v == J)
  Spd   -> (s == Clb    && v == J)
  Clb    -> (s == Spd   && v == J)

--getters
getCards :: Player -> [Card]
getCards (P _ _ cs) = cs 

currentPlayerT :: Trick -> Player
currentPlayerT (T _ cp _ _) = cp 

getSuit :: Card -> Suit
getSuit (C s _) = s

getVal :: Card -> Value
getVal (C _ v) = v

getName :: Player -> String
getName (P _ n _) = n

getLeadSuit :: Trick -> Suit
getLeadSuit (T lp _ _ cp) = 
 case (M.lookup lp cp) of
  Nothing      -> error "lead player hasn't played yet"
  Just (C s v) -> s

nextPlayer :: State -> Player
nextPlayer s = playerOrder M.! currentPlayerS s  

currentPlayerS :: State -> Player
currentPlayerS s = currentPlayerT $ getTrickS s

getTrickS :: State -> Trick
getTrickS s = getTrickH $ getHandG $ getGameS s

getTrickH :: Hand -> Trick
getTrickH (H _ _ _ t _) = t
getTrickH _ = error "no trick yet"

getGameS :: State -> Game
getGameS (S _ g) = g

getGameScoreS :: State -> GameScore
getGameScoreS s = getGameScoreG $ getGameS s

getGameScoreG :: Game -> GameScore
getGameScoreG (G _ g) = g

getHandG :: Game -> Hand
getHandG (G h _) = h

getHandS :: State -> Hand
getHandS s = getHandG $ getGameS s

getFaceUpS :: State -> Card
getFaceUpS s = getFaceUpH $ getHandG $ getGameS s

getFaceUpH :: Hand -> Card
getFaceUpH (StartOfHand _ (Just fu)) = fu
getFaceUpH _ = error "no face up card yet"

getDealerS :: State -> Player
getDealerS s = getDealerH $ getHandS s

getDealerH :: Hand -> Player
getDealerH (H d _ _ _ _) = d
getDealerH (StartOfHand d _) = d 

getHandScoreS :: State -> HandScore
getHandScoreS s = getHandScoreH $ getHandS s

getHandScoreH :: Hand -> HandScore
getHandScoreH (H _ _ _ _ hs) = hs
getHandScoreH _ = error "no hand yet"

nextP :: Player -> Player
nextP p = playerOrder M.! p

getCardsPlayedT :: Trick -> CardsPlayed
getCardsPlayedT (T _ _ _ cp) = cp

getCurrentPlayerT:: Trick -> Player 
getCurrentPlayerT (T _ cp _ _) = cp

getLeadPlayerT:: Trick -> Player 
getLeadPlayerT (T lp _ _ _) = lp


