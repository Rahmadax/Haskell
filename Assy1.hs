-- module for testing individual functions
module Assy1 where 
  
  -- Each domino has a left and right integer value
  type Domino = (Int, Int)
  -- Hand contains a list of dominoes
  type Hand = [Domino]
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  -- The complete set of dominoes (For later in the assignment)
  type Set = [Domino]
  -- Either the left or right of the board
  data End = L | R deriving (Eq, Ord)
  
  -- The maximum number of pips on each side of the domino
  pips :: Int
  pips = 6
  
  set :: Set
  set  = [(a, b)| a <- [0..pips], b <- [a..pips]]
 

  -- swapDom (Swaps the order of a Domino tuple) // Done!
  -- type definition
  swapDom :: (a,b) -> (b,a)
  -- function definition
  swapDom (a,b) = (b,a)
  
 
  -- goesP (True if a domino can be played at given end) Done!
  -- type definition
  goesP :: Board -> Domino -> End -> Bool
  -- function definition
  goesP [] _ _ = True
  goesP (hb:tb) domino end 
	| ((fst domino == 0) || (snd domino = 0)) = True
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
    | (fst domino == 0) = ((swapDom domino):board)
    | (fb == 0) = domino:board 
    | (fb == fst domino) || (fst domino == 0)) = (swapDom domino):board
    | (fb == snd domino) || (fst domino == 0)) = domino:board
	where fb = fst (head board)
    
  rightPlay :: Board -> Domino -> Board
  rightPlay board domino
    | lb == fst domino = board ++ [(swapDom domino)]
    | lb == snd domino = board ++ [domino]
	where lb = snd (last board)
	
	
	

	
  -- type definition
  scoreBoard :: Board -> Int
  -- function definition
  scoreBoard (h:t)
    | null h = 0
    | (((total `mod` 3) == 0 ) || ((total `mod` 5) == 0 )) = getScore total scores
    | otherwise = 0	
	where 
	  scores = [(3,1),(5,1),(6,2),(9,3),(10,2),(12,4),(15,8),(18,6),(20,4)]
	  total = getTotal board

  -- scoreBoard Aux function
  -- type definition
  getTotal :: board -> Int
  -- function definition
  getTotal (h:t)
    | ((fh == sh) && (ft == st)) = (fh*2 + st*2)
    | (fh == sh) = (fh*2 + st)
    | (ft == st) = (fh + st*2)
    | otherwise = (fh + st)
	where fh = fst h
	      sh = snd h
          ft = fst (last t)
          st = snd (last t)


  -- type definition
  getScore :: Int -> [(Int, Int)] -> Int
  --function definition
  getScore total (h:t) 
    | (fst h == total) = snd h
    | otherwise = getScore total t 
	
	

    
  possPlays :: Board -> Hand -> ([Domino],[Domino]) 
  possPlays board (h:t)
	| goesP board domino L = (h:(fst possiblePlays)) + possPlays board t
	| goesP board domino R = ((snd possiblePlays)++(h)) + possPlays board t
	| otherwise = possPlays board t
-}
