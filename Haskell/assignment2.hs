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
  
  
  playDomsRound :: Player -> Player -> Int -> (Int, Int)
  playDomsRound p1 p2 seed = playDomsLobby (newHand p1 rsf) (newHand p2 rss) ([],1)
    where (rsf,rss) = sDMakeHands (shuffleDoms seed)

  newHand :: Player -> [Domino] -> Player 
  newHand (dp,_,score) newHand = (dp, newHand, score)

  playDomsLobby :: Player -> Player -> (Board, Int) -> (Int, Int)
  playDomsLobby (dp1,p1h,p1s) (dp2,p2h,p2s) (b,_) 
    | (knockingP b p1h) && (knockingP b p2h) = (p1s,p2s)
    | knockingP b p1h = playDomsLobby (dp1,p1h,p1s) (dp2,p2h,p2s) (newTurn (dp2 b p2h) (b,2))
  playDomsLobby (dp1,p1h,p1s) p2 (b,1) = playDomsLobby (dp1,p1h,p1s) p2 (newTurn (dp1 b p1h) (b,1))
  playDomsLobby p1 (dp2,p2h,p2s) (b,2) = playDomsLobby p1 (dp2,p2h,p2s) (newTurn (dp2 b p2h) (b,2))
  
  newTurn :: (Domino,End) -> (Board,Int) -> (Board, Int)
  newTurn (d,e) (b,1) = (resMaybe(playDom b d e), 2)
  newTurn (d,e) (b,2) = (resMaybe(playDom b d e), 1)
  