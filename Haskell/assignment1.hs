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
  -- Set contains a list of dominoes
  type Set = [Domino]
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  -- Either the left or right most domino. 
  data End = L | R deriving (Eq)
  
  ------------------------------------------------------------------------
  -- A list of all dominoes
  set :: Set 
  set = [(a, b)| a <- [0..pips], b <- [a..pips]]
  -- The maximum number of pips on each side of the domino
  pips :: Int
  pips = 6

  ------------------------------------------------------------------------
  -- Utility functions
  -- swapDom function
  -- Swaps the order of a Domino tuple
  -- type definition
  swapDom :: (a,b) -> (b,a)
  -- function definition
  swapDom (a,b) = (b,a)
  
  resMaybe :: (Maybe a) -> a
  resMaybe (Just x) = x
 
  isJust :: (Maybe a) -> Bool
  isJust (Just _) = True
  isJust Nothing = False
  
  ------------------------------------------------------------------------
  -- goesP function 
  -- True if a domino can be played at a given end. 
  -- Also checks if a domino has already been played
  -- type definition
  goesP :: Board -> Domino -> End -> Bool
  -- function definition
  goesP [] _ _ = True
  goesP board (a,b) L = (leftP == a || leftP == b) && not(playedP board (a,b))
    where leftP = fst (head board) -- first domino's left pips
  goesP board (a,b) R = (rightP == a || rightP == b) && not(playedP board (a,b))
    where rightP = snd (last board) -- last domino's right pips
 
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
  knockingP _ [] = True
  knockingP board (hh:th) 
    | (goesP board hh L) || (goesP board hh R) = False
    | otherwise = knockingP board th
 
  -------------------------------------------------------------------------   
  -- playDom function
  -- Returns the board after a given domino has been played at a given end 
  -- Returns nothing if impossible
  -- type definition
  playDom :: Board -> Domino -> End -> Maybe Board
  -- function definition
  playDom board domino end
    | not(goesP board domino end) = Nothing
    | null board = Just [domino]
    | (end == L) = Just (if leftP == fst domino 
                          then (swapDom domino):board
                            else domino:board)

    | (end == R) = Just (if rightP == fst domino 
                          then board ++ [domino]
                            else  board ++ [swapDom domino])
    where leftP = fst (head board)
          rightP = snd (last board)
 
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
  scoreN board target = scoreNA board target set ([],[])
  
  scoreNA :: Board -> Int -> Set -> ([Domino],[Domino]) -> ([Domino],[Domino])
  scoreNA _ _ [] scoringDoms = scoringDoms
  scoreNA board target (sh:st) scoringDoms 
   | (hypoPlay L == Nothing) && (hypoPlay R == Nothing) = scoreNA board target st ((fst scoringDoms), (snd scoringDoms))
  
   | (isJust( hypoPlay L )) && (isJust( hypoPlay R ))= 
     if (scoreBoard (resMaybe( hypoPlay L )) == target && scoreBoard (resMaybe( hypoPlay R )) == target) then 
       scoreNA board target st (sh:(fst scoringDoms), sh:(snd scoringDoms))
	     else scoreNA board target st ((fst scoringDoms), (snd scoringDoms))

   | (isJust( hypoPlay L )) = 
     if ((scoreBoard (resMaybe (hypoPlay L))) == target) then
       scoreNA board target st (sh:(fst scoringDoms), (snd scoringDoms))
	     else scoreNA board target st ((fst scoringDoms), (snd scoringDoms))
  
   | (isJust( hypoPlay R )) = 
     if ((scoreBoard (resMaybe (hypoPlay R) )) == target) then 
       scoreNA board target st ((fst scoringDoms), sh:(snd scoringDoms))
	     else scoreNA board target st ((fst scoringDoms), (snd scoringDoms))
  
   | otherwise = scoreNA board target st ((fst scoringDoms), (snd scoringDoms))
   where hypoPlay = playDom board sh 