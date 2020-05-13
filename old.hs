import Text.Read

data Card = A | B | C deriving (Show, Eq, Read)

data Player = P String [(Maybe Card)] deriving (Show)

type Rounds = Int
type CurPlayer = Int

data State = S Player Player CurPlayer Rounds

p1 :: Player
p1 = P "me" [Nothing,(Just A),(Just B)]
p2 :: Player
p2 = P "you" [(Just A),(Just B),(Just B)]

startState :: State
startState = S p1 p2 0 0

main :: IO ()
main = handloop startState 

hand :: State -> IO ()
hand (S p1 p2 cp r) =
    do if r > 4
       then do putStrLn "game over" 
               return ()
       else do
               finishHandState <- handloop (S p1 p2 cp r)
               let finalState = incState finishHandState
               hand finalState
        

handloop :: State -> IO State
handloop s = if allPlayedCard s then 
                do putStrLn "finished hand"
                   return s 
             else
                  do print $ showCards s
                     putStrLn "select card"
                     cardString <- getLine
                     let cardSelect = readMaybe cardString :: Maybe Card
                     resultState <- handleCardSelect cardSelect s
                     handloop resultState


handleCardSelect :: Maybe Card -> State -> IO State
handleCardSelect Nothing  s = do putStrLn "invalid entry"
                                 return s
handleCardSelect (Just c) s = do newState <- handleValidCard c s 
                                 return newState 

handleValidCard :: Card -> State -> IO State
handleValidCard c (S p1 p2 cp r) = case cp of
 0 -> do putStrLn $ getName p1 ++ " played " ++ show c
         return (S (update c p1) p2 (cp + 1) r)
 1 -> do putStrLn $ getName p2 ++ " played " ++ show c
         return (S p1 (update c p2) (cp + 1) r)
 where update :: Card -> Player -> Player
       update c (P n cs) = P n (remove c cs)
       getName :: Player -> String
       getName (P name _) = name
       remove :: Card -> [(Maybe Card)] -> [(Maybe Card)]
       remove c = map (\x -> if x == (Just c) then Nothing else x) 


allPlayedCard :: State -> Bool
allPlayedCard (S _ _ cp _) = cp > 1

showCards :: State -> [String]
showCards (S p1 p2 cp _) = case cp of
 0 -> showPlayerCards p1
 1 -> showPlayerCards p2
 where showPlayerCards (P _ cs) = map show cs

incState :: State -> State
incState (S p1 p2 cs r) = (S p1 p2 cs (r+1))





















