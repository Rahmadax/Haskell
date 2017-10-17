-------------------------------------------------------
-- Assignment 1 - Dominoes (Part 1)
-- Written by Oliver Rahman
-- Written and fully tested functions go in this file.
-------------------------------------------------------
module Assignment1 where 
  ----------------------------
  -- Creating Datatypes
  ----------------------------
  -- Each domino has a left and right integer value
  type Domino = (Int, Int)
  -- Hand contains a list of dominoes
  type Hand = [Domino]
  -- Set contains a list of all dominoes (For later assignments)
  type Set = [Domino]
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  -- Either the left or right most domino. 
  data End = L | R deriving (Eq)
  
  ----------------------
  -- Init
  ----------------------
  -- The maximum number of pips on each side of the domino
  pips :: Int
  pips = 6
  -- Inits the complete set of dominoes
  set :: Set
  set  = [(a, b)| a <- [0..pips], b <- [a..pips]]
 
 
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
  goesP [] _ _ = True
  goesP board (a,b) L = (fd == a) || (fd == b)
    where fd = fst (head board) -- first domino on the board (left pips)
  goesP board (a,b) R = (ld == a) || (ld == b)
    where ld = snd (last board) -- last domino on the board (right pips)


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
  knockingP [] _ = False
  knockingP _ [] = True
  knockingP board (h:t) 
    | not (checkSide L) && not(checkSide R) = knockingP board t
    | otherwise = False
    where checkSide = goesP board h 
 
 
  -------------------------------------------------------------------------   
  -- playDom function and Aux placeDom
  -- Returns the board after a given domino has been played at a given end 
  -- Returns nothing if impossible
  ------------------------------------------------------------------------- 
  -- type definition
  playDom :: Board -> Domino -> End -> Maybe Board
  -- function definition
  playDom board domino end
    | not(goesP board domino end) = Nothing
    | otherwise = Just (placeDom board domino end)
  
  -- adds the given domino to the given end. Returns updated board
  -- type definition
  placeDom :: Board -> Domino -> End -> Board
  -- function definition
  placeDom [] domino _ = [domino]
  placeDom board domino L
    | (fst fb == fst domino) = (swapDom domino):board
    | (fst fb == snd domino) = domino:board
    where fb = (head board)
  placeDom board domino R
    |(snd lb == fst domino) = board ++ [domino]
    |(snd lb == snd domino) = board ++ [swapDom domino]
    where lb = (last board)
 
 
  ---------------------------------------------------------
  -- scoreBoard function and Aux getTotal and getScore
  -- Takes a board, returns an integer - the board's score
  ---------------------------------------------------------
  -- type definition  
  scoreBoard :: Board -> Int
  -- function definition
  scoreBoard [] = 0
  scoreBoard (h:t)
    | (((total `mod` 3) == 0 ) || ((total `mod` 5) == 0 )) = getScore total scores
    | otherwise = 0
   where 
   scores = [(0,0),(3,1),(5,1),(6,2),(9,3),(10,2),(12,4),(15,8),(18,6),(20,4)]
   total = getTotal (h:t)

  -- scoreBoard takes a board, returns the sum of its ends.
  -- type definition
  getTotal :: Board -> Int
  -- function definition
  getTotal (h:t)
    | null t                                   = fst h + snd h            -- only 1 domino on the board
    | ((fst h == snd h) && (fst lb == snd lb)) = ((fst h) + (snd lb))*2   -- a pair on each end
    | (fst h == snd h)                         = (fst h)*2 + (snd lb)     -- a pair on the left end
    | (fst lb == snd lb)                       = (fst h) + (snd lb)*2     -- a pair on the right end
    | otherwise                                = fst h + snd lb           -- no pairs
    where lb = last t

  -- getScore, takes the score table and the total score of the board's ends
  -- returns the actual score
  -- type definition
  getScore :: Int -> [(Int, Int)] -> Int
  --function definition
  getScore _ [] = 0
  getScore total (h:t) 
    | (fst h == total) = snd h
    | otherwise = getScore total t 
	