 -- module for testing individual functions
module Test where 
  
  -- Each domino has a left and right integer value
  type Domino = (Int, Int)
  
  -- Hand contains a list of dominoes
  type Hand = [Domino]
  
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  
  -- Either the left or right most domino. 
  data End = L | R deriving (Eq, Ord)
  
  
  -- The maximum number of pips on each side of the domino
  pips :: Int
  pips = 6
  
  hand :: Hand
  hand  = [(a, b)| a <- [0..pips], b <- [a..pips]]
 

  -- swapDom (Swaps the order of a Domino tuple) // Done!
  -- type definition
  swapDom :: (a,b) -> (b,a)
  -- function definition
  swapDom (a,b) = (b,a)
  
  -- goesP (True if a domino can be played at given end) Done!
  -- type definition
  goesP :: Board -> Domino -> End -> Bool
  -- function definition
  goesP (hb:tb) domino end 
    | null (hb:tb) = True
	| ((fst domino == 0) || (snd domino == 0)) = True
    | ((end == L) && ((fb == fst domino) || (fb == snd domino))) = True
    | ((end == R) && ((lb == fst domino) || (lb == snd domino))) = True
    | otherwise = False
    where fb = fst hb         -- First domino on the board
          lb = snd (last tb)  -- Last domino on the board
 
 
 
  -- playDom plays the current domino on the board and flips the tuple if needed. Done?
  -- type definition
  playDom :: Board -> Domino -> End -> Maybe Board
  -- function definition
  playDom board domino end 
    | ((checkSide L) || (checkSide R)) = Just (if (end == L)
	                                           then leftPlay board domino 
											    else rightPlay board domino
												)
    | otherwise = Nothing
	where checkSide = goesP board domino
     -- playDom Aux functions
	 
  leftPlay :: Board -> Domino -> Board
  leftPlay board domino
    | (fst fb == fst domino) = (swapDom domino):board
    | (fst fb == snd domino) = domino:board
	where fb = (head board)
	
  rightPlay :: Board -> Domino -> Board
  rightPlay board domino
    |snd lb == fst domino = board ++ [(swapDom domino)]
    |snd lb == snd domino = board ++ [domino]
	where lb = (last board)