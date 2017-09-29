module Assy1 where 
  
  -- Each domino has a left and right integer value
  type Domino = (Int, Int)
  
  -- Hand contains a list of Dominos
  type Hand = ([Domino])
  
  -- Board contains a list of dominos in the order they are lined up
  type Board = [Domino]
  
  -- Either the left or right most domino. 
  type End = (Left | Right)
  
  -- The maximum number of pips on each side of the domino
  int pips = 6
  
  
  
  
  domino :: Domino  = [(a, b)| a <- [0..pips], b <- [a..pips]]
  
  -- goesP (True if a domino can be played)
  -- type definition
  goesP :: Board -> Hand -> Bool 
  
  -- function definition
  goesP board hand = 
    if elem (fst head hand) (fst head board) then true
	else if elem (snd head hand) (fst head board) then true
	else if elem (fst head hand) (snd head board) then true
    else if elem (snd head hand) (snd board last) then true
    else goesP board (tail hand)
  
  
  
  
  -- knockingP (True if no Domino can be played)
  -- type definition
  knockingP :: Hand -> Board -> Bool
  
  -- function definition
  knockingP hand board =
    
  
  
  
  
  -- playedP (True if a domino has already been played)
  -- type definition
  playedP :: Domino -> Board -> Bool
  
  -- function definition
  playedP domino board = (elem domino board)
  
  
  
  -- possP (Returns all the dominos that can be played as Tuple of lists)
  -- type definition
  possP :: Hand -> Board -> ([Domino], [Domino])
  
  -- function definition
  possP hand board = 
  
  
  
  
  -- playDom (Adds a domino to one end of the board)
  playDom :: Domino -> Board -> End -> Board
  
  -- function definition
  playDom domino board end = 
    board++
  
  
  -- scoreBoard (Returns the 5s and 3s score of the current board)
  -- type definition
  scoreBoard :: Board -> Int
  
  -- function definition
  scoreBoard board = 
    if (((end1 + end2)mod 3 == 0) || ((end1 + end2)mod 5 == 0)) then (end1 + end2)
	else 0

	
	
  
  -- scoreN (Returns the dominos that can score)
  scoreN :: Board -> Int -> [Domino]
  
  