module Euchre where

import Shuffle
import Text.Read
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
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
 dealCards (changeDealerS s) >>=
 decideTrump >>= 
 (\s -> playTricks $ initializeHandScore s) 

playTricks :: State -> IO () 
playTricks s = 
 if handWinCondition s then runGame $ declareHandOver (getHandS s) s
 else
  playTrick s >>= playTricks


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
 askAll (nextP (getPlayersS s) dealer) (getHandS s) False >>= 
 \h -> let newState = updateStateH s h
 in return newState 
 where askAll :: Player -> Hand -> Bool -> IO Hand 
       --T/F indicates whether face up card, or choose any suit
       askAll p (StartOfHand d f) False = 
        if p == d 
        then promptPlayer p "'order up' or 'pass'" >>= 
             \str -> let (p',h) = handleSetTrump (getPlayersS s) p d f str
                     in askAll p' h True 
        else promptPlayer p "'order up' or 'pass'" >>= 
             \str -> let (p',h) = handleSetTrump (getPlayersS s) p d f str
                     in askAll p' h False 
       askAll p (StartOfHand d f) True  = 
        if p == d
        then promptPlayer p "screw the dealer, pick a suit" >>= 
             \str -> let (p',h) = handleSetTrump (getPlayersS s) p d Nothing str
                     in askAll p' h True 
        else promptPlayer p "call suit or 'pass'" >>= 
             \str -> let (p',h) = handleSetTrump (getPlayersS s) p d Nothing str
                     in askAll p' h True 
       askAll _ h _ = return h
 
handleSetTrump :: Players -> Player -> Player -> FaceUp -> String -> (Player,Hand)
handleSetTrump ps p d (Just (C s v)) str = 
 if str == "order up" then (p, (H d s p StartOfTrick M.empty))  
 else ((nextP ps p), (StartOfHand d (Just (C s v)))) 
handleSetTrump ps p d Nothing  str =
 let maybeSuit = readMaybe str :: Maybe Suit 
 in 
  case maybeSuit of
   (Just s) -> (p, (H d s p StartOfTrick M.empty))
   Nothing  -> if p == d then (p, (StartOfHand d Nothing)) 
               else (nextP ps p, (StartOfHand d Nothing)) 
     
initializeHandScore :: State -> State       
initializeHandScore (S ps (G h gs)) = (S ps (G (helper h gs) gs))
 where helper :: Hand -> GameScore -> Hand
       helper (H d t tc tr _) gs = (H d t tc tr (M.map (\_ -> 0) gs))  

updateStateH :: State -> Hand -> State 
updateStateH (S p (G _ gs)) h = S p (G h gs)  

promptPlayer :: Player -> String -> IO String
promptPlayer (P id n c) str = 
 putStr (n ++ ", your cards: ") >> print c >>
 putStr ("enter " ++ str) >>
 getLine >>= return 

changeDealerS :: State -> State
--change dealers only in not in 'StartOfHand'
--also think I need to get a random card from shuffle for 'fu'
changeDealerS (S p (G h gs)) = (S p (G (changeDealerH h) gs)) 
 where changeDealerH (H d t tc tr hs) = (H (nextP p d) t tc tr hs) 
       changeDealerH (StartOfHand d fu) = StartOfHand d fu
 
declareHandOver :: Hand -> State -> State
--reset to StartOfHand,update GameScore (using TrumpCaller and HandScore), increment dealer
declareHandOver (H d _ tc _ hs) (S p (G _ gs)) = 
 let (team,points) = calculateHandPoints tc hs 
     gs' = updateGameScore team points gs
 in (S p (G (StartOfHand (nextP p d) Nothing) gs'))

updateGameScore :: Team -> Int -> GameScore -> GameScore
updateGameScore t p gs = M.adjust (+p) t gs 

calculateHandPoints :: Player -> HandScore -> (Team,Int)
calculateHandPoints tc hs = 
 let 
  maxScore = maximum $ M.elems hs
  winner = let max = M.keys $ M.filter (== maxScore) hs
           in if length max == 0 then error "calcHandPts" else head max
  eucher = S.notMember tc winner 
 in if maxScore == 5 && eucher then (winner,4)
 else if maxScore == 5 then (winner,2)
 else (winner,1)

mapMax :: Ord b => M.Map a b -> a
mapMax xs = if length max == 0 then error "mapMax" else head max
    where max = M.keys $ M.filter (== m) xs
          m = maximum $ M.elems xs

declareWinner :: State -> IO ()
declareWinner (S _ (G _ gs)) = 
 print $ mapMax gs 

playTrick :: State -> IO State 
playTrick s = 
 trickLoop (getPlayersS s) (setupTrick s) >>=
 decideTrickWinner (getTrumpS s) >>= 
 \t -> return (updateStateT s t)

updateStateT :: State -> Trick -> State
--update players' cards,and dealer,and handScore,set trick to StartOfTrick
updateStateT (S p (G h gs)) t = 
 let h' = updateHandFromTrick p h t
     p' = updatePlayersFromTrick p t
 in (S p' (G h' gs))

updatePlayersFromTrick :: Players -> Trick -> Players
updatePlayersFromTrick ps (T _ _ _ cp) = 
 fmap (\(P id n cs) -> 
  (P id n (filter (\c -> 
   (cp M.! (P id n cs)) /= c) cs))) ps 

updateHandFromTrick :: Players -> Hand -> Trick -> Hand
updateHandFromTrick ps (H d t tc tr hs) (T _ _ (Just w) _) = 
 (H (nextP ps d) t tc StartOfTrick (updateHandScore w hs)) 
 
updateHandScore :: Player -> HandScore -> HandScore 
updateHandScore w hs = 
 let team = if length flt == 0 then error (show hs) else head flt
      where flt = filter (\s -> S.member (getIdP w) (S.map getIdP s)) $ M.keys hs
 in M.adjust (+1) team hs 

setupTrick :: State -> Trick 
setupTrick s = 
 let p = nextP (getPlayersS s) (getDealerS s)
 in (T p p Nothing M.empty)

decideTrickWinner :: Suit -> Trick -> IO Trick
decideTrickWinner trump (T lp cp _ crds) = 
 let p = fst $ L.maximumBy (trickOrdering trump (getSuit (crds M.! lp))) (M.toList crds)
 in putStrLn ("trick winner: " ++ (getName p)) >> 
    return (T lp cp (Just p) crds)

trickLoop :: Players -> Trick -> IO Trick 
trickLoop ps t = 
 putStrLn "played so far: " >>
 print (getCardsPlayedT t) >> 
 promptPlayer (getCurrentPlayerT t) "play a card" >>= 
 \str -> 
  case (readMaybe str :: Maybe Card) of
   Nothing  -> putStrLn "enter valid card" >> trickLoop ps t 
   (Just c) -> if (playerHasCard (getCurrentPlayerT t) c)
               then
                let t' = updateCardsPlayed t c 
                in if (getLeadPlayerT t') == (nextP ps (getCurrentPlayerT t'))
                   then return t' 
                   else trickLoop ps (advanceTrickPlayer ps t') 
               else 
                putStrLn "you don't have that card" >> trickLoop ps t

updateCardsPlayed :: Trick -> Card -> Trick
updateCardsPlayed (T lp cp w crds) c = (T lp cp w (M.insert cp c crds))  

advanceTrickPlayer :: Players -> Trick -> Trick
advanceTrickPlayer ps (T lp cp w crds) = (T lp (nextP ps cp) w crds)

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
dealCards (S players (G (StartOfHand d _) gs)) = 
 do cards <- shuffle allCards
    let (d1,rest1) = getFive cards
        (d2,rest2) = getFive rest1
        (d3,rest3) = getFive rest2
        (d4,rest4) = getFive rest3
        faceUp      = getOne  rest4 
        dealtPlayers = (fmap dealPlayer (F d1 d2 d3 d4)) <*> players 
    return (S dealtPlayers (G (StartOfHand d (Just faceUp)) gs))
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

trickOrdering :: Suit -> Suit -> (Player,Card) -> (Player,Card) -> Ordering
trickOrdering trumpSuit leadSuit (p1,c1) (p2,c2) = 
 let s1 = getSuit c1
     s2 = getSuit c2
     v1 = getVal c1
     v2 = getVal c2
 in 
  if isTrump trumpSuit c1 && isTrump trumpSuit c2 then trumpOrdering trumpSuit c1 c2 
  else if isTrump trumpSuit c1 then GT
  else if isTrump trumpSuit c2 then LT
  else leadSuitOrdering leadSuit c1 c2 

leadSuitOrdering :: Suit -> Card -> Card -> Ordering
leadSuitOrdering leadSuit (C s1 v1) (C s2 v2) = 
 if s1 == s2 then compare v1 v2
 else if s1 == leadSuit then GT
 else if s2 == leadSuit then LT
 else compare v1 v2

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

nextP :: Players -> Player -> Player
nextP (F p1 p2 p3 p4) p = 
 if p == p1 then p2
 else if p == p2 then p3
 else if p == p3 then p4
 else p1

getCardsPlayedT :: Trick -> CardsPlayed
getCardsPlayedT (T _ _ _ cp) = cp

getCurrentPlayerT:: Trick -> Player 
getCurrentPlayerT (T _ cp _ _) = cp

getLeadPlayerT:: Trick -> Player 
getLeadPlayerT (T lp _ _ _) = lp

getTrumpS :: State -> Suit
getTrumpS s = getTrumpH $ getHandS s

getTrumpH :: Hand -> Suit
getTrumpH (H _ t _ _ _) = t
getTrumpH _ = error "trump not decided yet"

getPlayersS :: State -> Players
getPlayersS (S ps _) = ps

getIdP :: Player -> ID
getIdP (P id _ _) = id
