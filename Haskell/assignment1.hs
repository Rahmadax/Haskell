----------------------------------------------
-- Assignment 1 - Dominoes (Part 1)
-- Written by Oliver Rahman
-- 
----------------------------------------------
module Assignment1 where 
  
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
 
 
 
  ---------------------------------------
  -- swapDom function
  -- Swaps the order of a Domino tuple
  ---------------------------------------
  -- type definition
  swapDom :: (a,b) -> (b,a)
  -- function definition
  swapDom (a,b) = (b,a)
  
  
 
  --------------------------------------------------
  -- goesP function 
  -- True if a domino can be played at a given end.
  --------------------------------------------------
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



  ------------------------------------
  -- playedP function 
  -- True if a domino has been played
  ------------------------------------
  -- type definition
  playedP :: Board -> Domino -> Bool
  -- function definition
  playedP board domino = elem domino board

  
  
  -----------------------------------
  -- knockingP function
  -- True if no domino can be played
  -----------------------------------
  -- type definition
  knockingP :: Board -> Hand -> Bool
  -- function definition
  knockingP board (h:t)
    | null h = True
    | null board = False
    | (not (checkSide L) && not(checkSide R)) = knockingP board t
    | otherwise = True
    where checkSide = goesP board h 
 
 
 
  -----------------------------------------
  -- scoreBoard function and Aux
  -- Returns an integer, the board's score
  -----------------------------------------
  -- type definition  
  scoreBoard :: Board -> Int
  -- function definition
  scoreBoard (h:t)
    | null h = 0
    | (((total `mod` 3) == 0 ) || ((total `mod` 5) == 0 )) = getScore total scores
    | otherwise = 0
    where 
      scores = [(0,0),(3,1),(5,1),(6,2),(9,3),(10,2),(12,4),(15,8),(18,6),(20,4)]
      total = getTotal (h:t)

  -- scoreBoard Aux function, takes a board, returns value of its ends.
  -- type definition
  getTotal :: Board -> Int
  -- function definition
  getTotal (h:t)
    | null h = 0
    | null t = (fst h + snd h)
    | ((fst h == snd h) && (fst lb == snd lb)) = (((fst h) + (snd lb))*2)
    | (fst h == snd h) = ((fst h)*2 + (snd lb))
    | (fst lb == snd lb) = ((fst lb)*2 + (fst h))
    | otherwise = 0
    where lb = last t

  -- getScore, takes the score table and the total score of the board's ends
  -- returns the actual score
  -- type definition
  getScore :: Int -> [(Int, Int)] -> Int
  --function definition
  getScore total (h:t) 
    | (fst h == total) = snd h
    | otherwise = if null t
                    then 0
				      else getScore total t 