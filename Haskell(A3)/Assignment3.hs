module Assignment3 where

    import DomsMatch
    import System.Random
    import Data.List
    import Data.Maybe

    type Tactic = (Hand -> DomBoard -> Player -> Scores -> (Dom, End))

    
    ------------------------------------------------------------------------
    -- The custom made DomsPlayer
    ------------------------------------------------------------------------
    aiPlayer :: DomsPlayer
    aiPlayer h b p s 
      | myScore >= 53 = endGame h b p s
      | null history = earlyGame h b p s
      | otherwise = hsdPlayer h b p s
      where 
        history = getHistory b
        board = rebuildBoard history
        (myScore,enemyScore) = getScore p s
    
    -- Minor Tactics are defined in the following 3 functions.
    -- Major Tactics defined later.
    -------------------------------------------------------------------------
    -- Early game Tactics called here
    -------------------------------------------------------------------------
    earlyGame :: Tactic
    earlyGame h b p s 
      -- Tactic plays the double if you have 3 or more of the same pip count
      | (bestSpotNo > 2 && haveDouble h bestSpot) = ((bestSpot,bestSpot), L)
      -- Plays (5,4) if you have it
      | elem (5,4) h = ((5,4),L)
      -- otherwise defaults to hsd
      | otherwise = hsdPlayer h b p s
      where 
        spotList = countAllSpots h 6 []
        (bestSpot,bestSpotNo) = getHighest spotList
    -------------------------------------------------------------------------
    -- Mid Game Tactics called here
    -------------------------------------------------------------------------
    midGame :: Tactic 
    midGame h b p s = hsdPlayer h b p s
     
    --------------------------------------------------------------------------
    -- End Game Tactics called here
    --------------------------------------------------------------------------
    endGame :: Tactic 
    endGame h b p s
    -- Would have liked to avoids using heads here, but wheres are Immutable  
      | nn sg61L = ( head sg61L, L) -- Try get to 61
      | nn sg61R = (head sg61R, R) 
      | nn tkL = (head tkL,L)  -- Try to force a knock
      | nn tkR = (head tkR,R)
      -- | nn dLL = (head dLL,L)
      -- | nn dLR = (head dLR,R)
      | nn sg59L = (head sg59L, L) -- Try get to 59
      | nn sg59R = (head sg59R, R)
      | otherwise = hsdPlayer h b p s -- default to hsd
      where 
        (myScore,enemyScore) = getScore p s
        history = getHistory b
        board = rebuildBoard history
        pps = possPlays board h
        (sg61L,sg61R) = scoreGoal pps b myScore 61
        (sg59L,sg59R) = scoreGoal pps b myScore 59
        (tkL,tkR) = tryKnock h history p 4
        -- (dLL,dLR) = avoidN h b p enemyScore 61
        
    
    nn :: [Dom] -> Bool
    nn [] = False
    nn a = True
        
        
    --------------------------------------------------------------------------
    -- Major Tactic Utility functions here 
    --------------------------------------------------------------------------
    -- tryKnock searches for plays that have less than a target number of responses
    tryKnock :: Hand -> History -> Player -> Int -> ([Dom],[Dom])
    tryKnock h history player t = 
      let 
        (lAL,lAR) = lookAhead h history player
      in  
        ([d | (d,l) <- lAL, l < t], [d | (d,l) <- lAR, l < t])

  {-  -- AvoidN searches to try and block the opposition from being able to play a dom
    -- that can score a specific target value next turn.
    avoidN :: Hand -> DomBoard -> Player -> Int -> Int -> (Dom,Dom)
    avoidN h b p s t =  
      let 
        history = getHistory b
        pEDs = possibleEnemyDoms h history p
        board = rebuildBoard history
        (ppsL,ppsR) = possPlays board h
        (hBoardsL, hBoardsR) = ((map (hypPlay board L) ppsL),(map (hypPlay board R) ppsR)) 
      in -- Generates 2 lists of boards, only kept if there are no responses that score t
        avoidNA ((filter (\d ->( null (fst(scoreGoal (possPlays d pEDs) b s t))
          && null (snd(scoreGoal (possPlays d pEDs) b s t)))) hBoardsL),
        (filter(\d ->( null (fst(scoreGoal (possPlays d pEDs) b s t))
          && null (snd(scoreGoal (possPlays d pEDs) b s t)))) hBoardsR))
    -- 
    avoidNA :: ([[Dom]],[[Dom]]) -> (Dom,Dom)
    avoidNA (nScoreL,nScoreR) = (map head nScoreL, map head nScoreR)
    -}      
        
    -- Sees how many response moves can be played for each of your doms.
    -- Search function
    -- Zips a list of playable dominos with the number of response moves against
    -- that play 
    lookAhead :: Hand -> History -> Player -> ([(Dom, Int)],[(Dom, Int)])
    lookAhead h history p =
      let 
        fkL = lookAheadA board L pEDs 
        fkR = lookAheadA board R pEDs
        board = rebuildBoard history
        pEDs = possibleEnemyDoms h history p
        (ppsL,ppsR) = possPlays board h
      in 
        ((zip ppsL (map fkL ppsL)), (zip ppsR (map fkR ppsR)))
        
    -- Calculates the number of response plays
    lookAheadA :: [Dom] -> End -> [Dom] -> Dom -> Int
    lookAheadA board e pEDs d 
      | e == L = (length (fst (possPlays (hypo d L) pEDs))) + 
        (length (snd (possPlays (hypo d L) pEDs)))
      | e == R = (length (fst (possPlays (hypo d R) pEDs))) + 
        (length (snd (possPlays (hypo d R) pEDs)))
      where
        hypo = hypoPlay board
        
        
    -- Similar to scoreN, returns a list of dominoes that score a given value
    -- from a given list
    scoreGoal :: ([Dom],[Dom]) -> DomBoard -> Int -> Int -> ([Dom],[Dom])
    scoreGoal (possLeft,possRight) b score target = 
      ([d| d <- possLeft, (scoreDom d L b == (target - score))]
        ,[d| d <- possRight, (scoreDom d R b == (target - score))])
      where
        history = getHistory b 
        board = rebuildBoard history
       
    --------------------------------------------------------------------------
    -- Utility functions related to guessing the opposition's dominos
    --------------------------------------------------------------------------
    -- Returns a list of Dominos that the opposition might have
    possibleEnemyDoms :: Hand -> History -> Player -> [Dom] 
    possibleEnemyDoms h history p = 
      let 
        enemyKnocks = getKnockingMoves history p -- Every time the opposition knocks
        ekl = length enemyKnocks -- number of times the opposition knocked
        impDoms = getImpossibleDoms history enemyKnocks ekl []
        -- excluded doms are those in players hand, on board
        -- and those calculated in getImpossibleDoms
        domList = h ++ (rebuildBoard history) ++ impDoms
      in 
        nub (filter (\d -> not ((elem d domList) || elem (swapDom d) domList) ) domSet)
             
    -- Calculates which dominoes weren't played each time the opposition knocks
    -- calls possplays with domSet, removes all doms in that call
    getImpossibleDoms :: History -> [Int] -> Int -> [Dom] -> [Dom]
    getImpossibleDoms [] _ _ _ = []
    getImpossibleDoms history eks ekl impDoms 
      | ekl == 0 = impDoms
      | otherwise = getImpossibleDoms history eks (ekl-1) impDoms++iD
      where 
        (idL, idR) = getImpossibleDomsA history (eks!!(ekl-1))
        iD = idL ++ idR
    
    getImpossibleDomsA :: History -> Int -> ([Dom],[Dom])
    getImpossibleDomsA history fek = 
      let 
        board = rebuildBoardTo history (fek-1)
      in 
        possPlays board domSet 
        
        
    ----------------------------------------------------------------------------------
    -- Utility functions related to Knocking
    ----------------------------------------------------------------------------------
    -- Gets all times the opposition has knocked
    getKnockingMoves :: History -> Player -> [Int]
    getKnockingMoves history p = getKnockingMovesA pMoves numDomsP []
      where 
        numDomsP = numDomsPlayed history
        pMoves = (sort [(moveNo)|(_,player,moveNo) <- history, (player /= p)]) --Get enemy moves
    
    -- helper function, 
    getKnockingMovesA :: [Int] -> Int -> [Int] -> [Int]
    getKnockingMovesA pMoves counter knockingMoves
      | counter == 1 = reverse knockingMoves
      | otherwise = getKnockingMovesA pMoves (counter-1) (knockingMoves ++ k)
      where 
        k = isKnocking pMoves counter
    -- helper function,   
    isKnocking :: [Int] -> Int -> [Int]
    isKnocking pMoves counter 
      | (elem counter pMoves && elem (counter-1) pMoves) = [counter]
      | otherwise = []
      
    
    --------------------------------------------------------------------------
    -- Very common Utility Functions
    --------------------------------------------------------------------------
    -- Gets history from a DomBoard
    getHistory :: DomBoard -> History
    getHistory (Board _ _ history) = history
    getHistory InitBoard = []
    
    -- Remakes the whole board from a DomBoard
    rebuildBoard :: History -> [Dom]
    rebuildBoard history = [d|(d,_,_) <- history]
    
    -- Remakes the board to a specific move
    rebuildBoardTo :: History -> Int -> [Dom]
    rebuildBoardTo history t = [d|(d,_,moveNo) <- history, moveNo <= t]
    
    -- Orients the score into (myScore, oppositionScore) using given Player
    getScore :: Player -> Scores -> (Int,Int)
    getScore P1 (s1,s2) = (s1,s2)
    getScore P2 (s1,s2) = (s2,s1)
    
    -- Returns the Player data type of the opposition
    getOpposition :: Player -> Player
    getOpposition P1 = P2
    getOpposition P2 = P1
    
    
    -------------------------------------------------------------------------------------------
    -- Less used / obsolete utility functions
    -------------------------------------------------------------------------------------------
    -- Makes a list of how many of each spot number exist in a given list of Doms, zipped with spot value
    countAllSpots :: [Dom] -> Int -> [Int] -> [Dom]
    countAllSpots _ t spotList
      | (t <= -1) = zip [6,5,4,3,2,1,0] spotList
    countAllSpots dList t spotList = countAllSpots dList (t-1) (spotList ++ [howMany dList t])
    
    -- How many of a given pip count exist in a given list of doms
    howMany :: [Dom] -> Int -> Int
    howMany dList t  = length (filter (\(a,b) -> (a==t || b==t)) dList)
    
    -- Returns all spot values with 4 or more of their spot count in given domList
    get4OrMore :: [(Int,Int)] -> [(Int,Int)]
    get4OrMore spotCount = [(a,b)|(a,b) <- spotCount, b > 3]
    
    -- returns the domino spot count with the most doms in a given dom list
    getHighest :: [(Int,Int)] -> (Int,Int)
    getHighest spotCount = maximumBy (comparing snd) (reverse spotCount)
    
    -- Returns true if the double exists in a given dom list
    haveDouble :: Hand -> Int -> Bool
    haveDouble h t = elem (t,t) h 

    -- How many dominos have been played
    howManyPlayed :: Hand -> DomBoard -> Int
    howManyPlayed h b = length (filter(\d -> (playedP board d)) h)
      where
        history = getHistory b      
        board = rebuildBoard history
      
    numDomsPlayed :: History -> Int
    numDomsPlayed [] = 0
    numDomsPlayed history = thrd (maximumBy(comparing thrd) history)
      
    thrd :: (Dom, Player, MoveNum) -> Int
    thrd (_,_,a) = a
      
      
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      
  
    -- Ported forward old functions
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
    
    hypoPlay :: [Dom] -> Dom -> End -> [Dom]
    hypoPlay [] d _ = [d]
    hypoPlay board (a,b) L 
      | fst (head board) == a = ((b,a)):board
      | otherwise = (a,b):board
    hypoPlay board (a,b) R  
      | snd (last board) == b = board ++ [(b,a)]
      | otherwise = board ++ [(a,b)]  
      
    hypPlay :: [Dom] -> End -> Dom -> [Dom]
    hypPlay [] _ d = [d]
    hypPlay board L (a,b) 
      | fst (head board) == a = ((b,a)):board
      | otherwise = (a,b):board
    hypPlay board R (a,b) 
      | snd (last board) == b = board ++ [(b,a)]
      | otherwise = board ++ [(a,b)] 

     -- Checks if a maybe function returns a Just or Nothing
    isJust :: (Maybe a) -> Bool
    isJust (Just _) = True
    isJust Nothing = False