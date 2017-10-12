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
  goesP board domino end 
    | null board = True
    | (end == L) = goesPLeftA board domino
    | (end == R) = goesPRightA board domino

  -- goesP Aux functions
  goesPLeftA :: Board -> Domino -> Bool
  goesPLeftA (hb:tb) domino
    | ((fst hb == fst domino) || (fst hb == snd domino)) = True
    | otherwise = False

  goesPRightA :: Board -> Domino -> Bool
  goesPRightA board domino
    | ((lb == fst domino) || (lb == snd domino)) = True
    | otherwise = False
    where lb = snd (last board)
