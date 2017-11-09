--------------------------------------------------------------------------
-- Assignment 2 - Dominoes (Part 2)
-- Written by Oliver Rahman
-- Written and fully tested functions go in this file.
--------------------------------------------------------------------------
module Assignment2 where 
  ------------------------------------------------------------------------
  import System.Random
  import Data.List
  import MergeSort
  import Assignment1
  
  type Player = (DomsPlayer, Hand, Int) 
  type DomsPlayer = Hand -> Board -> (Domino,End)
  
  maxDs :: Int  -- Total number of Dominoes in the game
  maxDs = 28
  dsPHand :: Int -- Number of Domino per hand in 2 player game
  dsPHand = 9
  
  -- Player Utility Functions
  getDP :: Player -> DomsPlayer 
  getDP (dp,_,_) = dp
  getH :: Player -> Hand 
  getH (_,hand,_) = hand
  getS :: Player -> Int 
  getS (_,_,score) = score

  -- generates maxDoms number of random numbers, zips with domino set.
  -- mergesorts into new order, unzips pair and returns random order set
  shuffleDoms :: Int -> [Domino]
  shuffleDoms seed = (map fst (mergesort(\(_,n1)(_,n2)->n1<n2)(zip set raNs)))
    where raNs = take maxDs(randoms(mkStdGen seed)::[Int]) -- Random number generator

  simplePlayer :: DomsPlayer
  simplePlayer board hand = simplePlayerA (possPlays board hand)
  
  simplePlayerA :: ([Domino],[Domino]) -> (Domino, End)
  simplePlayerA ((ha:_),_) = (ha, L)
  simplePlayerA (_,hb:_) = (hb, R)
  
  
  -- Takes the random domino set and filters every other domino into each hand
  -- Returns both hands as a tuple
  sDMakeHands :: [Domino] -> ([Domino],[Domino])
  sDMakeHands rs = (filter (\d -> sDI d rs 0) rs, filter (\d -> sDI d rs 1) rs)

  -- Indexer, checks modulus of index and if index < 2*dsPHand
  sDI :: Domino -> [Domino] -> Int -> Bool
  sDI d rs tar = let index = resMaybe(elemIndex d rs) in
    (index `mod` 2 == tar) && (index < (dsPHand*2)) 


  newHand :: Player -> [Domino] -> Player 
  newHand (dp,_,score) nHand = (dp, nHand, score)	
  
  playDomsRound :: Player -> Player -> Int -> (Int, Int)
  playDomsRound p1 p2 seed = playDomsLobby (newHand p1 rsf) (newHand p2 rss) [] 1
    where (rsf,rss) = sDMakeHands (shuffleDoms seed)

	
  playDomsLobby :: Player -> Player -> Board -> Int -> (Int, Int)
  
  playDomsLobby (dp1,hand1,score1) (dp2,hand2,score2) b _
    | knockingP b hand1 && knockingP b hand2 = (score1,score2)
	| knockingP b hand1 = playDomsLobby (dp1,hand1,score1) (dp2,hand2,nScore) nBoard 1
	where (nBoard, nScore) = nextPlay (dp2,hand2,score2) b
	
  playDomsLobby (dp,hand,score) p2 b 1 = playDomsLobby (dp,hand,nScore) p2 nBoard 2
    where (nBoard, nScore) = nextPlay (dp,hand,score) b
	
  playDomsLobby p1 (dp,hand,score) b 2 = playDomsLobby p1 (dp,hand,nScore) nBoard 1
    where (nBoard, nScore) = nextPlay (dp,hand,score) b
  
  
  nextPlay :: Player -> Board -> (Board, Int)
  nextPlay (dp,hand,score) b = (newb, (score+scoreBoard newb))
    where newb = (nextPlayDom b (dp b hand))
  
  nextPlayDom :: Board -> (Domino, End) -> Board
  nextPlayDom b (d,e) = resMaybe (playDom b d e)
  
  