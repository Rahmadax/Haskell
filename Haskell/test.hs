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
  goesP [] _ _ = True
  goesP (hb:tb) domino end 
    | ((fst domino == 0) || (snd domino == 0)) = True
    | ((fb == 0) || (lb == 0)) = True
    | ((end == L) && ((fb == fst domino) || (fb == snd domino))) = True
    | ((end == R) && ((lb == fst domino) || (lb == snd domino))) = True
    | otherwise = False
    where fb = fst hb         -- First domino on the board's left pips
          lb = snd (last tb)  -- Last domino on the board's right pips
 
 
 
  -- playDom plays the current domino on the board and flips the tuple if needed. Done?
  -- type definition
  playDom :: Board -> Domino -> End -> Maybe Board
  -- function definition
  playDom board domino end
    | not(checkSide end) = Nothing
    | otherwise = Just (placeDom board domino end)
    where checkSide = goesP board domino
  
  
  placeDom :: Board -> Domino -> End -> Board
  placeDom [] domino _ = [domino]
  placeDom board domino L
    | (fst fb == 0) = domino:board
    | ((fst fb == fst domino) || (fst domino == 0)) = (swapDom domino):board
    | ((fst fb == snd domino) || (snd domino == 0)) = domino:board
    where fb = (head board)
  placeDom board domino R
    |( snd lb == 0) = board ++ [domino]
    |((snd lb == fst domino) || (snd domino == 0)) = board ++ [(swapDom domino)]
    |((snd lb == snd domino) || (fst domino == 0)) = board ++ [domino]
    where lb = (last board)