--------------------------------------------------------------------------
-- Assignment 2 - Dominoes (Part 2)
-- Part 1 and Mergesort below
-- Written by Oliver Rahman
--------------------------------------------------------------------------
module Assignment2 where 
  import System.Random
  import Data.List
  
   {- VARIABLE NAMES
   b - general name for a board
   h - general name for a hand
   h1 / h2 - name for either player 1 or 2's hand.
   s1 / s2 - name for either player 1 or 2's score
   d - general name for a domino
   e - general name for an end
   rs - random set of dominos
   raNs - list of random numbers
   seed - Seed for the RNG
   maxDs - Total number of Dominoes in the game
   dsPH - Number of Domino per hand in 2 player game
   x / y - temporary variable names
   dp - general name for a domsPlayer functions
   dp1 / dp2 - name for either player 1 or 2's domsPlayer function
   nBoard - the newly generated board after a domino has been played
   nScore - the new score after the new boared score has been added to the old score.
   pDsLeft / pDsRight - possible plays on the left / right
   psTarget / psTail - the head or tail of the possible score list 
   scoreNds - Dominos that score some given N value
   -}
   
  type Player = (DomsPlayer, Hand, Int) 
  type DomsPlayer = Hand -> Board -> (Domino,End)
  type Domino = (Int, Int)
  type Hand = [Domino]
  type Board = [Domino]
  data End = L | R deriving (Eq, Show)
  ------------------------------------------------------------------------
  maxDs :: Int  -- Total number of Dominoes in the game
  maxDs = 28
  dsPH :: Int -- Number of Domino per hand in 2 player game
  dsPH = 9
  
  -- Plays the first possible domino (Left check first, then Right)
  spPlayer :: Player
  spPlayer = (simplePlayer, [], 0)
  simplePlayer :: DomsPlayer 
  simplePlayer h b = simplePlayerA (possPlays b h) -- checks poss plays (A1)
  simplePlayerA :: ([Domino],[Domino]) -> (Domino, End)
  simplePlayerA (x:_,_) = (x, L)
  simplePlayerA ([],y:_) = (y, R)
  
  -- Plays highest scoring move. Filters ds that are both playable and scoring.
  -- Checks all for values in passed [Int] starting at 8 and counting down
  hsPlayer :: Player
  hsPlayer = (hsdPlayer, [], 0)
  hsdPlayer :: DomsPlayer
  hsdPlayer h b = hsdPlayerA (possPlays b h) b [8,6,4,3,2,1,0]
  hsdPlayerA :: ([Domino],[Domino]) -> Board -> [Int] -> (Domino, End)
  hsdPlayerA (pDsLeft,pDsRight) b (psTarget:psTail) 
    | not (null (hsdChecker pDsLeft scoreNLeft))  = (head (hsdChecker pDsLeft scoreNLeft), L)
    | not (null (hsdChecker pDsRight scoreNRight)) = (head (hsdChecker pDsRight scoreNRight), R)
    | otherwise = hsdPlayerA (pDsLeft,pDsRight) b psTail
    where (scoreNLeft, scoreNRight) = scoreN b psTarget
  hsdChecker :: [Domino] -> [Domino] -> [Domino]
  hsdChecker playableDs scoreNds = (filter (\d -> ((elem d scoreNds)||(elem (swapDom d) scoreNds)))playableDs)
 
 
  -- Takes a two players and a seed. Generates and assigns hand, calls game handler.
  playDomsRound :: Player -> Player -> Int -> (Int,Int)
  playDomsRound (dp1,_,s1) (dp2,_,s2) seed = playDomsHandler (dp1,(take dsPH rs),s1)(dp2,(take dsPH srs),s2) [] 1
    where rs = (shuffleDoms seed) 
          srs = drop dsPH rs
  
  -- generates maxDoms number of random numbers, zips with domino set.
  -- mergesorts into new order, unzips pair and returns random order set
  shuffleDoms :: Int -> [Domino]
  shuffleDoms seed = (map fst (mergesort(\(_,n1)(_,n2)->n1<n2)(zip set raNs)))
    where raNs = take maxDs(randoms(mkStdGen seed)::[Int]) -- Random number generator 

  -- Game handler, checks if a player is knocking, if no, calls function to play a move.
  -- its possible to avoid repeated wheres but the code becomes messier because of the changes.
  -- This function does not edit hands, i found it easier to maintain and shorter by comparing
  playDomsHandler :: Player -> Player -> Board -> Int -> (Int,Int)
  playDomsHandler (dp1,h1,s1) (dp2,h2,s2) b _
    -- players 1 and 2 both knocking, game ends
    | knockingP b h1 && knockingP b h2 = (s1, s2)
    -- just player 1 knocking, player 2 makes a move
    | knockingP b h1 = playDomsHandler (dp1,h1,s1) (dp2,h2,nScore2) nBoard2 1
    -- just player 2 knocking, player 1 makes a move
    | knockingP b h2 = playDomsHandler (dp1,h1,nScore1) (dp2,h2,s2) nBoard1 2
    where 
      (nBoard1, nScore1) = nextPlay (dp1,h1,s1) b
      (nBoard2, nScore2) = nextPlay (dp2,h2,s2) b
  -- if no knocking, the called player (passed Int) takes their turn
  -- player 1 called
  playDomsHandler (dp1,h1,s1) p2 b 1 = playDomsHandler (dp1,h1,nScore) p2 nBoard 2
    where (nBoard, nScore) = nextPlay (dp1,h1,s1) b
  -- player 2 called
  playDomsHandler p1 (dp2,h2,s2) b 2 = playDomsHandler p1 (dp2,h2,nScore) nBoard 1
    where (nBoard, nScore) = nextPlay (dp2,h2,s2) b  
 
  -- takes a player and a board, returns the new board and their new score
  nextPlay :: Player -> Board -> (Board, Int)
  nextPlay (dp,h,s) b = (nBoard, (s+scoreBoard nBoard))
    where (d,e) = dp h b
          nBoard = resMaybe (playDom b d e) 

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  pips :: Int
  pips = 6
  ------------------------------------------------------------------------
  set = [(a, b)| a <- [0..pips], b <- [a..pips]]
  -- Utility functions
  -- Swaps the order of a tuple
  swapDom :: (a,b) -> (b,a)
  swapDom (a,b) = (b,a)
  -- Turns a Just x into its original type
  resMaybe :: (Maybe a) -> a
  resMaybe (Just a) = a
  -- Checks if a maybe function returns a Just or Nothing
  isJust :: (Maybe a) -> Bool
  isJust (Just _) = True
  isJust Nothing = False
  
  ------------------------------------------------------------------------
  -- goesP function 
  -- True if a domino can be played at a given end. 
  -- Also checks if a domino has already been played
  goesP :: Board -> Domino -> End -> Bool
  goesP [] _ _ = True
  goesP board (ld,rd) L = (leftP == ld || leftP == rd) && not(playedP board (ld,rd))
    where leftP = fst (head board) -- first domino's left pips
  goesP board (ld,rd) R = (rightP == ld || rightP == rd) && not(playedP board (ld,rd))
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
    -- Using If / Else for readability and space
    | (end == L) = Just (if leftP == fst domino 
                          then (swapDom domino):board
                            else domino:board)

    | (end == R) = Just (if rightP == fst domino 
                          then board ++ [domino]
                            else  board ++ [swapDom domino])
    where leftP = fst (head board)
          rightP = snd (last board)
 
  ------------------------------------------------------------------------
  -- scoreBoard function and Aux
  -- Takes a board, returns an integer - the board's score  
  scoreBoard :: Board -> Int
  scoreBoard [] = 0
  scoreBoard board
    | (scoreBoardA board == 0) = 0
    | (getTotm5 == 0) && (getTotm3 == 0) = 8 
    | (getTotm5 == 0) = (scoreBoardA board) `div` 5
    | (getTotm3 == 0) = (scoreBoardA board) `div` 3
    | otherwise = 0
   where getTotm5 = scoreBoardA board `mod` 5 
         getTotm3 = scoreBoardA board `mod` 3

  scoreBoardA :: Board -> Int
  scoreBoardA (hb:tb)
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
  ------------------------------------------------------------------------
  -- possPlays function
  -- Takes a Board and Hand, returns all scoring dominoes
  possPlays :: Board -> Hand -> ([Domino], [Domino])
  possPlays [] hand = (hand, hand)
  possPlays board hand = 
    (filter (\domino -> goesP board domino L) hand, -- Left end
    filter (\domino -> goesP board domino R) hand)  -- Right end

  ------------------------------------------------------------------------
  -- scoreN function
  -- Takes a Board and an integer n, returns all dominoes that score n
  scoreN :: Board -> Int -> ([Domino],[Domino])
  scoreN board target = scoreNA board target set ([],[])
  
  scoreNA :: Board -> Int -> [(Domino)] -> ([Domino],[Domino]) -> ([Domino],[Domino])
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


  --merge

  merge :: Ord a=> (a->a -> Bool)->[a]->[a] -> [a]
  
  merge _ [] lis2 = lis2

  merge _ lis1 [] = lis1
  merge compfn lis1 lis2 
    | compfn h1 h2 = (h1:merge compfn t1 lis2)
    | otherwise = (h2:merge compfn lis1 t2)
    where 
      (h1:t1)=lis1
      (h2:t2)=lis2
  -------------------------------------------------
  --mergesort
  
  mergesort :: Ord a=> (a->a -> Bool)->[a] -> [a]
 
  mergesort _ [] = [] --check this once only
  mergesort compfn dlis = 
              mergesortA compfn (map (\ e -> [e]) dlis) -- give aux fn list of lists length 1
  -------------------------------------------------
  --mergsortA
  mergesortA :: Ord a=> (a->a -> Bool)->[[a]] -> [a]

  mergesortA _ [lis] = lis -- one list only, it's the answer
  -- general case - merge list pairs & repeat
  mergesortA compfn mlis= mergesortA compfn (mergesortpass compfn mlis)
  ---------------------------------------------------------------------
  --mergesortpass
  -- merge pairs of lists 
  mergesortpass :: Ord a=> (a->a -> Bool)->[[a]] -> [[a]]

  mergesortpass _ [] = [] 
  mergesortpass _ [l]= [l] -- one element only, return list unchanged

  -- general case - merge first two lists, cons to remainder

  mergesortpass compfn (lis1:(lis2:rest)) =(merge compfn lis1 lis2): mergesortpass compfn rest
  