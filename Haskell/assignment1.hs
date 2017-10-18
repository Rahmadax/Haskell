--------------------------------------------------------------------------
-- Assignment 1 - Dominoes (Part 1)
-- Written by Oliver Rahman
-- Written and fully tested functions go in this file.
--------------------------------------------------------------------------
module Assignment1 where 
  ------------------------------------------------------------------------
  -- Datatypes
  -- Each domino has a left and right integer value
  type Domino = (Int, Int)
  -- Hand contains a list of dominoes
  type Hand = [Domino]
  type Set = [Domino]
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  -- Either the left or right most domino. 
  data End = L | R deriving (Eq)
  
  ------------------------------------------------------------------------
  -- Initialisation
  set :: Set 
  set = [(a, b)| a <- [0..pips], b <- [a..pips]]
  -- The maximum number of pips on each side of the domino
  pips :: Int
  pips = 6

  ------------------------------------------------------------------------
  -- swapDom function
  -- Swaps the order of a Domino tuple
  -- type definition
  swapDom :: (a,b) -> (b,a)
  -- function definition
  swapDom (a,b) = (b,a)
  
  ------------------------------------------------------------------------
  -- goesP function 
  -- True if a domino can be played at a given end.
  -- type definition
  goesP :: Board -> Domino -> End -> Bool
  -- function definition
  goesP [] _ _ = True
  goesP board (a,b) L = (leftPips == a) || (leftPips == b)
    where leftPips = fst (head board) -- first domino's left pips
  goesP board (a,b) R = (rightPips == a) || (rightPips == b)
    where rightPips = snd (last board) -- last domino's right pips

  ------------------------------------------------------------------------
  -- playedP function 
  -- True if a domino has been played
  -- type definition
  playedP :: Board -> Domino -> Bool
  -- function definition
  playedP board domino = elem domino board
  
  ------------------------------------------------------------------------
  -- knockingP function
  -- True if no domino can be played
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
 
  ------------------------------------------------------------------------
  -- scoreBoard function and Aux getTotal and getScore
  -- Takes a board, returns an integer - the board's score
  -- type definition  
  scoreBoard :: Board -> Int
  -- function definition
  scoreBoard [] = 0
  scoreBoard board
    | ((total `mod` 3) == 0 || (total `mod` 5) == 0) = getScore total scores
    | otherwise = 0
   where 
   scores = [(3,1),(5,1),(6,2),(9,3),(10,2),(12,4),(15,8),(18,6),(20,4)]
   total = getTotal board

  -- type definition
  getTotal :: Board -> Int
  -- function definition
  getTotal (hb:tb)
    -- only 1 domino on the board
    | null tb = fst hb + snd hb        
    -- a pair on each end	
    | ((fst hb == snd hb) && (fst lb == snd lb)) = ((fst hb)+(snd lb))*2
    -- a pair on the left end
    | (fst hb == snd hb) = (fst hb)*2 + (snd lb)    
    -- a pair on the right end	
    | (fst lb == snd lb) = (fst hb) + (snd lb)*2 
    -- no pairs	
    | otherwise = fst hb + snd lb           
    where lb = last tb
 
  -- type definition
  getScore :: Int -> [(Int, Int)] -> Int
  --function definition
  getScore _ [] = 0
  getScore total (hs:ts) 
    | (fst hs == total) = snd hs
    | otherwise = getScore total ts

  ------------------------------------------------------------------------
  -- possPlays function
  -- Takes a Board and Hand, returns all scoring dominoes
  -- type definition
  possPlays :: Board -> Hand -> ([Domino], [Domino])
  -- function definition
  possPlays [] hand = (hand, hand)
  possPlays board hand = 
    (filter (\domino -> goesP board domino L) hand, -- Left end
    filter (\domino -> goesP board domino R) hand)  -- Right end

  ------------------------------------------------------------------------
  -- scoreN function
  -- Takes a Board and an integer n, returns all dominoes that score n
  
  scoreN :: Board -> Int -> ([Domino],[Domino])
  
  scoreN board target = s
    (filter (\domino -> goesP board domino L) hand, -- Left end
    filter (\domino -> goesP board domino R) hand)  -- Right end  
  
  goesP && not(playedP ) && (scoreBoard == target)
  
  
  
  
  
  
  
  
  