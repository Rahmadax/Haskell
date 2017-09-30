
module Assy1 where 
  
  -- Each domino has a left and right integer value
  type Domino = (Int, Int)
  
  -- Hand contains a list of dominoes
  type Hand = [Domino]
  
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  
  
  -- The set of all dominoes
  type Set = [Domino]
  
  -- Either the left or right most domino. 
  --type End = (left, right)
  
  -- The maximum number of pips on each side of the domino

  pips :: Int
  pips = 6
  
  set :: Set
  set  = [(a, b)| a <- [0..pips], b <- [a..pips]]
  
  board :: Board
  board = []
  
{-
  -- goesP (True if a domino can be played)
  -- type definition
  goesP :: Board -> Hand -> End -> Bool 
  
  -- function definition
  goesP board hand end = 
	if (end = left) then
      if elem (fst (head hand)) (fst (head board)) then True
	  else if elem (snd (head hand)) (fst (head board)) then True
	  else False
	else 
	  if elem (fst (head hand)) (snd (last board)) then True
      else if elem (snd (head hand)) (snd (last board)) then True
      else False
  

  -- knockingP (True if no Domino can be played)
  -- type definition
  knockingP :: Hand -> Board -> Bool
  
  -- function definition
  knockingP hand board = 
    if (goesP board hand left = False || goesP board hand right = False) then True
    else False
  
  
  -- playedP (True if a domino has already been played)  // Done!
  -- type definition
  playedP :: Domino -> Board -> Bool
  
  -- function definition
  playedP domino board = 
    if elem domino board then True
    else False
  
  
  
  -- possP (Returns all the dominos that can be played as Tuple of lists)
  -- type definition
  possP :: Hand -> Board -> ([Domino], [Domino])
  
  -- function definition
  possP hand board = 
  if (playedP (head hand) board = True) then 
    possP (tail hand) board
  else if (goesP board hand left = True)
    
  else if (goesP board hand right = True)
    
  else possP (tail hand) board
    
  -}
  
  -- playDom (Adds a domino to one end of the board) Done?
  playDom :: Domino -> Board -> End -> Board
  
  -- function definition
  playDom domino board end = 
    if ((end = left)
	  if ((snd domino) /= (fst (head board)) then 
	    swap domino
		board = domino ++ board
	  else 
	    board = domino ++ board
	else
	  if ((fst domino) /= (snd (head board))
        swap domino
		board ++ domino
      else 
        board ++ domino
  
  
  
  total board :: Board -> Int
  
  total = (fst (head board) + snd (last board))
  
  
  
  -- scoreBoard (Returns the 5s and 3s score of the current board) -- fix scoring
  -- type definition
  scoreBoard :: Board -> Int
  
  -- function definition
  scoreBoard board = 
    if ( (total board) mod 3 == 0) || (total board)mod 5 == 0) then (total board)
	else 0

	
	{-
  
  -- scoreN (Returns the dominos that can score)
  scoreN :: Board -> Int -> [Domino]
  
-}