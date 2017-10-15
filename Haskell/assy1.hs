-- module for testing individual functions
module Assy1 where 
  
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
	| ((fst domino == 0) || ((snd domino = 0)) = True
    | ((end == L) && ((fb == fst domino) || (fb == snd domino))) = True
    | ((end == R) && ((lb == fst domino) || (lb == snd domino))) = True
    | otherwise = False
    where fb = fst hb         -- First domino on the board
          lb = snd (last tb)  -- Last domino on the board


  -- playedP (True if a domino has already been played)  // Done!
  -- type definition
  playedP :: Board -> Domino -> Bool
  -- function definition
  playedP board domino = elem domino board

  


  -- knockingP (True if no Domino can be played) // Done!
  -- type definition
  knockingP :: Board -> Hand -> Bool
  -- function definition
  knockingP board (h:t)
    | null h = True
    | null board = False
    | (not (checkSide L) && not(checkSide R)) = knockingP board t
    | otherwise = True
	where checkSide = goesP board h 
 
  {-
  -- playDom plays the current domino on the board and flips the tuple if needed. Done?
  -- type definition
  playDom :: Domino -> Board -> End -> Maybe Board
  -- function definition
  playDom domino board end 
    | null board = domino:board
    | checkSide L = just leftPlay board domino 
    | checkSide R = just rightPlay board domino
    | otherwise = Nothing
	where checkSide = goesP board domino
     -- playDom Aux functions
  leftPlay :: Board -> Domino -> Board
  leftPlay board domino
    | (f == fst domino) = (swapDom domino):board
    | (f == snd domino) = domino:board
	where f = fst (head board)
  rightPlay :: Board -> Domino -> Board
  rightPlay board domino
    | l == fst domino = board ++ [(swapDom domino)]
    | l == snd domino = board ++ [domino]
	where l = snd (last board)	
	-}
	
	
 
	
	{-

    
  possPlays :: Board -> Hand -> ([Domino],[Domino]) 
  possPlays board (h:t)
	| goesP board domino L = (h:(fst possiblePlays)) + possPlays board t
	| goesP board domino R = ((snd possiblePlays)++(h)) + possPlays board t
	| otherwise = possPlays board t
	where possiblePlays = ([],[])

-}	