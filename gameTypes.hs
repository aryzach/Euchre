import qualified Data.Set as S

State Teams Game

Game Hand Score

Trick LeadPlayer Turn Winner

Hand Dealer Trump FaceUp Trick Score 


data State  = S Players Game
data Game   = G Hand Score
data Hand   = H Dealer Trump FaceUp Trick TrickScore
data Trick  = T LeadPlayer Turn Winner
data Player = P Name Team [Card]
data Card   = C Suit Value
data Suit   = Hrt | Dia | Spd | Clb
data Value  = A | K | Q | J | Ten | Nine 

type Players = [Player]
type Trump   = Suit
type FaceUp  = Card
type Team  = (setOfPlayer, Int)  
type Teams = setOfTeam 

