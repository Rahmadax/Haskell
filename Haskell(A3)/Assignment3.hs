module Assignment3 where

    import DomsMatch
    import System.Random
    import Data.List

    type Tactic = (Hand -> DomBoard -> Player -> Scores -> (Dom, End))

    

    -- The custom made DomsPlayer
    aiPlayer :: DomsPlayer
    aiPlayer h b p s 
      | null history && elem (5,4) h = ((5,4),L)
      | myScore >= 53 = endGame h b p s
      | otherwise = hsdPlayer h b p s
       where history = getHistory b
             board = rebuildBoard b
             (myScore,enemyScore) = getScore p s

             
             
             
    -- Utility functions
    getHistory :: DomBoard -> History
    getHistory (Board _ _ history) = history
    getHistory InitBoard = []
    
    rebuildBoard :: DomBoard -> [Dom]
    rebuildBoard b = let history = getHistory b in [d|(d,_,_) <- history]

    getScore :: Player -> Scores -> (Int,Int)
    getScore P1 (s1,s2) = (s1,s2)
    getScore P2 (s1,s2) = (s2,s1)
    
    getEnds :: [Dom] -> (a,b)
    getEnds board = (fst (head board), snd (last board))
      
    
    -- End Game Tactics
    endGame :: Tactic 
    endGame h b p s
       | nn (scoreGoal61 L)) = ((head (scoreGoal61 L)), L)
       | nn (scoreGoal61 R)) = ((head (scoreGoal61 R)), R)
       | myScore < 59 && nn (scoreGoal59 L)) = (head (scoreGoal59 L), L)
       | myScore < 59 && nn (scoreGoal59 R)) = (head (scoreGoal59 R), R)
       | otherwise = hsdPlayer h b p s
       where 
         nn = not null
         (myScore,enemyScore) = getScore p s
         board = rebuildBoard b
         pps = possPlays board h
         scoreGoal61 = scoreGoal pps b myScore 61
         scoreGoal59 = scoreGoal pps b myScore 59
            
    scoreGoal :: ([Dom],[Dom]) -> DomBoard -> Int -> Int -> End -> [Dom]
    scoreGoal (possLeft,possRight) b score target e 
      | e == L = [d| d <- possLeft, (scoreDom d L b == (target - score))]
      | e == R = [d| d <- possRight, (scoreDom d R b == (target - score))]
      where
        board = rebuildBoard b
        
    blockEnemy :: Tactic
    blockEnemy h b p s
      | nn (scoreGoal61 L) = blockEnemyA 
      | nn (scoreGoal61 R) = 
      where
        (myScore, enemyScore) = getScore s
        board = rebuildBoard b
        pEDs = possibleEnemyDoms
        pps = possPlays board pEDs
        scoreGoal61 = scoreGoal pps b enemyScore 61
        nn = not null
    


    possibleEnemyDoms :: [Dom] -> DomBoard -> [Dom] 
    possibleEnemyDoms h b = let domList = h ++ (rebuildBoard b)
      in filter (\d -> not (elem d domList) ) domSet

    -- Makes a list of how many of each spot number exist in a given list of Doms.
    countAllSpots :: [Dom] -> Int -> [Int] -> [Int]
    countAllSpots _ t spotList
      | (t == -1) = spotList
    countAllSpots dList t spotList = countAllSpots dList (t-1) (spotList ++ [countAllSpotsA dList t])
    countAllSpotsA :: [Dom] -> Int -> Int
    countAllSpotsA dList t  = length (filter (\(a,b) -> (a==t || b==t)) dList)

    howManyPlayed :: Hand -> DomBoard -> Int
    howManyPlayed h b = length (filter(\d -> (playedP board d) ) h)
      where board = rebuildBoard b

  
  
  
  
  
  
    -- Ported forward functions
    possPlays :: [Dom] -> Hand -> ([Dom], [Dom])
    possPlays [] hand = (hand, hand)
    possPlays board hand = 
      (filter (\d -> goesP board d L) hand, filter (\d -> goesP board d R) hand)


    goesP :: [Dom] -> Dom -> End -> Bool
    goesP [] _ _ = True
    goesP board (a,b) L = (fd == a) || (fd == b)
      where fd = fst (head board) -- first domino on the board (left pips)
    goesP board (a,b) R = (ld == a) || (ld == b)
      where ld = snd (last board) -- last domino on the board (right pips)


    -- type definition
    swapDom :: (a,b) -> (b,a)
    -- function definition
    swapDom (a,b) = (b,a)

    playedP :: [Dom] -> Dom -> Bool
    playedP board domino = (elem domino board) || elem (swapDom domino) board

     -- Checks if a maybe function returns a Just or Nothing
    isJust :: (Maybe a) -> Bool
    isJust (Just _) = True
    isJust Nothing = False
  
