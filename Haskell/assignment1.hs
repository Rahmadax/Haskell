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
  -- Board contains a list of dominoes in the order they are lined up
  type Board = [Domino]
  -- Either the left or right most domino. 
  data End = L | R deriving (Eq, Show)
  
  ------------------------------------------------------------------------
  -- Initialisation
  -- A list of all dominoes (used for scoresN and testing)
  set :: Hand 
  set = [(a, b)| a <- [0..pips], b <- [a..pips]]
  -- The maximum number of pips on each side of the domino
  pips :: Int
  pips = 6

  ------------------------------------------------------------------------
  -- Utility functions
  -- Swaps the order of a tuple
  swapDom :: (a,b) -> (b,a)
  swapDom (a,b) = (b,a)
  -- Turns a Just x into its original type
  resMaybe :: (Maybe a) -> a
  resMaybe (Just x) = x
  -- Checks if a maybe function returns a Just or Nothing
  isJust :: (Maybe a) -> Bool
  isJust (Just _) = True
  isJust Nothing = False
  -- gets ends of the board
  getEnds :: Board->(Domino,Domino)
  getEnds board = (head board,last board)
  
  ------------------------------------------------------------------------
  -- goesP function 
  -- True if a domino can be played at a given end. 
  -- Also checks if a domino has already been played
  goesP :: Board -> Domino -> End -> Bool
  goesP [] _ _ = True
  goesP board (a,b) L = (leftP == a || leftP == b) && not(playedP board (a,b))
    where leftP = fst (head board) -- first domino's left pips
  goesP board (a,b) R = (rightP == a || rightP == b) && not(playedP board (a,b))
    where rightP = snd (last board) -- last domino's right pips
 
  ------------------------------------------------------------------------
  -- playedP function 
  -- True if a domino has been played
  playedP :: Board -> Domino -> Bool
  playedP board domino = (elem domino board) || (elem (swapDom domino) board)
  
  ------------------------------------------------------------------------
  -- knockingP function
  -- True if no domino can be played
  knockingP :: Board -> Hand -> Bool
  knockingP _ [] = True
  knockingP board (hh:th) 
    | (goesP board hh L) || (goesP board hh R) = False
    | otherwise = knockingP board th
 
  -------------------------------------------------------------------------   
  -- playDom function
  -- Returns the board after a given domino has been played at a given end 
  -- Returns nothing if impossible
  playDom :: Board -> Domino -> End -> Maybe Board
  playDom board domino end
    | not(goesP board domino end) = Nothing
    | null board = Just [domino]
    | (end == L) = Just (if leftP == fst domino then (swapDom domino):board
                          else domino:board)
    | (end == R) = Just (if rightP == fst domino then board ++ [domino]
                          else  board ++ [swapDom domino])
    where leftP = fst (head board)
          rightP = snd (last board)
 
  ------------------------------------------------------------------------
  -- scoreBoard function and Aux getTotal and getScore
  -- Takes a board, returns an integer - the board's score
  scoreBoard :: Board -> Int
  scoreBoard [] = 0
  scoreBoard board
    | ((total `mod` 3) == 0 || (total `mod` 5) == 0) = getScore total scores
    | otherwise = 0
   where scores = [(3,1),(5,1),(6,2),(9,3),(10,2),(12,4),(15,8),(18,6),(20,4)]
         total = getTotal board

  -- Calculates the total score of the board
  getTotal :: Board -> Int
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
 
 -- Calculates the points using the 'scores' list
  getScore :: Int -> [(Int, Int)] -> Int
  getScore _ [] = 0
  getScore total (hs:ts) 
    | (fst hs == total) = snd hs
    | otherwise = getScore total ts

  ------------------------------------------------------------------------
  -- possPlays function
  -- Takes a Board and Hand, returns all dominoes that can be played
  possPlays :: Board -> Hand -> ([Domino], [Domino])
  possPlays [] hand = (hand, hand)
  possPlays board hand = 
    (filter (\domino -> goesP board domino L) hand, -- Left end
    filter (\domino -> goesP board domino R) hand)  -- Right efile:///C:/Users/Ollie/AppData/Local/Temp/Assignment1.hs.hsnd

  ------------------------------------------------------------------------
  -- scoreN function
  -- Takes a Board and an integer n, returns all dominoes that score n
  scoreN :: Board -> Int -> ([Domino],[Domino])
  scoreN board target = scoreNA board target set ([],[])
  
  scoreNA :: Board -> Int -> Hand -> ([Domino],[Domino]) -> ([Domino],[Domino])
  scoreNA _ _ [] scoringDoms = scoringDoms
  scoreNA board target (sh:st) scoringDoms 
  -- A domino that cannot be played
    | (hypoPlay L == Nothing) && (hypoPlay R == Nothing) = 
      reRun ((fst scoringDoms), (snd scoringDoms))
  
   -- A domino that scores on both ends
   | (isJust(hypoPlay L)) && (isJust(hypoPlay R))= 
     if (scoreBoard (resMaybe(hypoPlay L)) == target &&
         scoreBoard (resMaybe(hypoPlay R)) == target) 
      then reRun (sh:(fst scoringDoms), sh:(snd scoringDoms))
         else reRun ((fst scoringDoms), (snd scoringDoms))

   -- A domino that only scores on the left
   | (isJust( hypoPlay L )) = 
     if (scoreBoard (resMaybe (hypoPlay L))) == target 
       then reRun (sh:(fst scoringDoms), (snd scoringDoms))
         else reRun ((fst scoringDoms), (snd scoringDoms))

   -- A domino that only scores on the left
   | (isJust(hypoPlay R)) = 
     if (scoreBoard (resMaybe (hypoPlay R))) == target 
       then reRun ((fst scoringDoms), sh:(snd scoringDoms))
         else reRun ((fst scoringDoms), (snd scoringDoms))
  
  -- A domino that doesn't score but can be played
   | otherwise = reRun ((fst scoringDoms), (snd scoringDoms))
   where hypoPlay = playDom board sh -- Hypothetical Test boards
         reRun = scoreNA board target st -- Short hand to save space