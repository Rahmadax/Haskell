module Assignment3 where

    import DomsMatch
    import System.Random
    import Data.List
    import Data.Maybe
    import Debug.Trace
    
    {- Variable Dictionary 
        h = hand, b = DomBoard, p = player, s = scores, d = domino, l = array length
        t = target integer, c = counter, x/a/b = generic variable names
        history = a list of all the moves in a game
        board = the [Dom] form, rebuilt from history
        dList = generic name for a list of dominoes that aren't a board or hand
        bestSpot = the most common pip value in a [Dom]
        bestSpotNo = the count of the above
        spotList = the number of each spot value in a [Dom]
        tryN = shorthand for a curried look ahead call, awaiting the value to look for.
        avoidN = the result of a blocking search 
        doubleB = the result of a doubleBlock search 
        tryKnock) = the result of a Forced Knock search
        shortBoard = a fake short board formed with the two end doms next to each other
        nn = shorthand for nested not null call. n = shorthand for null call
        pps = ([Dom],[Dom]) The possible response plays to a board, both L and R
        ppsL, ppsR = The above, broken into two lists
        pEDs = A list of possible enemy dominoes
        lAL/lAR = returns from a look ahead function call
        h prefix  = hypothetical object, created in a look ahead search
        hypo = shorthand for a hypothetical domino play. hypoPlay
        eKs = a list of moves the opposition knocked on. eKl = length of eKs
        fEK = the first enemy knock move in eKs
        iD(_/L/R) = part of the list of dominoes the opposition cannot have
        impDoms = the complete list of dominoes the opposition cannot have
        moveNo = the move number of a play
    -}
    
    type Tactic = (Hand -> DomBoard -> Player -> Scores -> Maybe (Dom, End))
    
    ------------------------------------------------------------------------
    -- The custom made DomsPlayers
    ------------------------------------------------------------------------
    -- Some of this is fairly heavy computationally, but the >= 53 short circuits often 
    customAI:: DomsPlayer
    customAI h b p s 
      | myScore >= 53 && isJust try61R =  fromJust try61R         -- Try to win
      | enemyScore >= 53 && isJust doubleBR = fromJust doubleBR   -- Blocks the opposition getting to 61 or 59
      | enemyScore >= 53 && isJust block61R = fromJust block61R   -- Blocks the opposition getting to 61. 
      | enemyScore >= 53 && isJust block59R = fromJust block59R   -- Blocks the opposition getting to 59
      | myScore >= 53 && isJust try59R = (fromJust try59R)        -- Try to get to 59
      | null history && elem (5,4) h = ((5,4),L)                  -- Play 5,4 if you have it 
      | null history && spotCheck h = ((bS h,bS h), L)            -- Plays double if you have it +2 others of that spot
      | otherwise = hsdPlayer h b p s                             -- Default to hsd
      where 
        history = getHistory b
        (myScore, enemyScore) = getScore p s
        -- Tactic result list
        try61R = try61 h b p s
        try59R = try59 h b p s
        doubleBR = blockDub h b p s
        block61R = block61 h b p s
        block59R = block59 h b p s

        
    -----------------------------------------------------------------------------
    -- Old AI and fun builds
    -----------------------------------------------------------------------------
    
    -- This AI uses all tactics, including those that make it worse
    sprayAndPray:: DomsPlayer
    sprayAndPray h b p s 
      | myScore >= 53 && isJust try61R =  fromJust try61R         -- Try to win
      | enemyScore >= 53 && isJust doubleBR = fromJust doubleBR   -- Blocks the opposition getting to 61 or 59
      | enemyScore >= 53 && isJust block61R = fromJust block61R   -- Blocks the opposition getting to 61. 
      | enemyScore >= 53 && isJust block59R = fromJust block59R   -- Blocks the opposition getting to 59
      | enemyScore >= 53 && isJust block57R = fromJust block57R   -- Blocks the opposition getting to 59
      | myScore >= 53 && isJust try59R = (fromJust try59R)        -- Try to get to 59
      | myScore >= 53 && isJust try57R = (fromJust try57R)        -- Try to get to 59
      | null history && elem (5,4) h = ((5,4),L)                  -- Play 5,4 if you have it 
      | null history && spotCheck h = ((bS h,bS h), L)            -- Plays double if you have it +2 others of that spot
      | isJust tryKnR = fromJust tryKnR                           -- Tries to force a knock
      | otherwise = hsdPlayer h b p s                             -- Default to hsd
      where 
        history = getHistory b
        (myScore, enemyScore) = getScore p s
        -- Tactic result list
        try61R = try61 h b p s
        try59R = try59 h b p s
        try57R = try57 h b p s      
        tryKnR = tryKnock h b p s 
        doubleBR = blockDub h b p s
        block61R = block61 h b p s
        block59R = block59 h b p s
        block57R = block57 h b p s  
    
    -- this AI only uses early Game tactics
    twoSecondConcentration:: DomsPlayer
    twoSecondConcentration h b p s 
      | null history && elem (5,4) h = ((5,4),L)                  -- Play 5,4 if you have it 
      | null history && spotCheck h = ((bS h,bS h), L)            -- Plays double if you have it +2 others of that spot
      | otherwise = hsdPlayer h b p s                             -- Default to hsd
      where 
        history = getHistory b
        (myScore, enemyScore) = getScore p s
        
    -- This AI only uses endGame tactics
    slacker:: DomsPlayer
    slacker h b p s 
      | myScore >= 53 && isJust try61R =  fromJust try61R         -- Try to win
      | enemyScore >= 53 && isJust doubleBR = fromJust doubleBR   -- Blocks the opposition getting to 61 or 59
      | enemyScore >= 53 && isJust block61R = fromJust block61R   -- Blocks the opposition getting to 61. 
      | enemyScore >= 53 && isJust block59R = fromJust block59R   -- Blocks the opposition getting to 59
      | enemyScore >= 53 && isJust block57R = fromJust block57R   -- Blocks the opposition getting to 59
      | myScore >= 53 && isJust try59R = (fromJust try59R)        -- Try to get to 59
      | myScore >= 53 && isJust try57R = (fromJust try57R)        -- Try to get to 59
      | otherwise = hsdPlayer h b p s                             -- Default to hsd
      where 
        (myScore, enemyScore) = getScore p s
        -- Tactic result list
        try61R = try61 h b p s
        try59R = try59 h b p s
        try57R = try57 h b p s      
        doubleBR = blockDub h b p s
        block61R = block61 h b p s
        block59R = block59 h b p s
        block57R = block57 h b p s  
        
     -- This AI tries to block you from playing the game
    noFunAllowed:: DomsPlayer
    noFunAllowed h b p s 
      | enemyScore >= 53 && isJust doubleBR = fromJust doubleBR   -- Blocks the opposition getting to 61 or 59
      | enemyScore >= 53 && isJust block61R = fromJust block61R   -- Blocks the opposition getting to 61. 
      | enemyScore >= 53 && isJust block59R = fromJust block59R   -- Blocks the opposition getting to 59
      | enemyScore >= 53 && isJust block57R = fromJust block57R   -- Blocks the opposition getting to 57
      | isJust tryKnR = fromJust tryKnR                           -- Try to force a knock
      | otherwise = hsdPlayer h b p s                             -- Default to hsd
      where 
        (myScore, enemyScore) = getScore p s
        -- Tactic result list    
        doubleBR = blockDub h b p s
        block61R = block61 h b p s
        block59R = block59 h b p s
        block57R = block57 h b p s 
        tryKnR = tryKnock h b p s 

    
    
    ---------------------------------------------------------------------------------
    -- Tactic Functions
    -- Takes a hand, board, player and scores. Returns Maybe (Dom, End)
    ---------------------------------------------------------------------------------
    -- Try to win the game
    try61 :: Tactic 
    try61 h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        shortBoard = getShortB b
        pps = possPlays shortBoard h 
        (myScore,enemyScore) = getScore p s
        result = (scoreGoal pps b myScore 61)
        (domList, e) = (getLR result)
        
    -- Try to reach 59
    try59 :: Tactic 
    try59 h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        shortBoard = getShortB b
        pps = possPlays shortBoard h 
        (myScore,enemyScore) = getScore p s
        result = (scoreGoal pps b myScore 59)
        (domList, e) = (getLR result)
        
    -- Try to reach 57
    try57 :: Tactic 
    try57 h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        shortBoard = getShortB b
        pps = possPlays shortBoard h 
        (myScore,enemyScore) = getScore p s
        result = (scoreGoal pps b myScore 57)
        (domList, e) = (getLR result)
    
    -- Block the opposition from winning
    block61 :: Tactic
    block61 h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        result = avoidN h b p s 61
        (domList, e) = getLR result

    -- Block the opposition from reaching 59
    block59 :: Tactic
    block59 h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        result = avoidN h b p s 59
        (domList, e) = getLR result
    
    -- Block the opposition from reaching 57
    block57 :: Tactic
    block57 h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        result = avoidN h b p s 57
        (domList, e) = getLR result
        
    -- Block both 59 and 61
    blockDub :: Tactic
    blockDub h b p s 
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        result = doubleBlock h b p s 59 61
        (domList, e) = getLR result
    
    -- Try to force a knock
    tryKnock :: Tactic
    tryKnock h b p s
      | nn domList = Just (hsdPlayer2 domList b p s e)
      | otherwise = Nothing
      where 
        history = getHistory b
        result = tryKnockA h history p 4
        (domList, e) = getLR result
        
    -- Short hand for the double > 2 tactic
    spotCheck :: Hand -> Bool 
    spotCheck h = 
      let 
        spotList = countAllSpots h 6 []
        (bestSpot,bestSpotNo) = getHighest spotList
      in
        (bestSpotNo > 2 && haveDouble h bestSpot)
    -------------------------------------------------------------------------
    -- Major Tactic Utility functions here 
    --------------------------------------------------------------------------
    
    -- tryKnock searches for plays that have less than a target number of responses
    tryKnockA :: Hand -> History -> Player -> Int -> ([Dom],[Dom])
    tryKnockA h history player t = 
      let 
        (lAL,lAR) = zipLength h history player
      in  
        ([d | (d,l) <- lAL, l < t], [d | (d,l) <- lAR, l < t])

    -- AvoidN searches to try and block the opposition from being able to play a dom
    -- that can score a specific target value next turn. General function. 
    -- Note that this is heavy computationally but should short circuit most of the time
    avoidN :: Hand -> DomBoard -> Player -> (Int,Int) -> Int -> ([Dom],[Dom])
    avoidN h b p s t =  
      let 
        (myScore,oppScore) = getScore p s
        history = getHistory b 
        pEDs = possibleEnemyDoms h history p 
        shortBoard = getShortB b 
        (ppsL,ppsR) = possPlays shortBoard h
        -- Generates a list of new boards (search of depth 1)
        (hBoardsL, hBoardsR) = ((map (updateBoardh L p b) ppsL),(map (updateBoardh R p b) ppsR)) 
        -- Generates 2 lists of boards, only kept if there are no responses to new board, from the predicted enemy doms, that score t 
      in 
        getBoards
          ([newBoard | newBoard <- hBoardsL,
            speedChecker(scoreGoal (possPlays (rebuildBoard (getHistory newBoard)) pEDs) newBoard oppScore t)],
          [newBoard | newBoard <- hBoardsR,
            speedChecker(scoreGoal (possPlays (rebuildBoard (getHistory newBoard)) pEDs) newBoard oppScore t)])
    
    -- Searches for doms that prevent both 59 and 61 as responses        
    doubleBlock :: Hand -> DomBoard -> Player -> (Int,Int) -> Int -> Int -> ([Dom],[Dom])
    doubleBlock h b p s t1 t2 =  
      let 
        (myScore,oppScore) = getScore p s
        history = getHistory b 
        pEDs = possibleEnemyDoms h history p 
        shortBoard = rebuildBoard history 
        (ppsL,ppsR) = possPlays shortBoard h
        -- Generates a list of new boards (search of depth 1)
        (hBoardsL, hBoardsR) = ((map (updateBoardh L p b) ppsL),(map (updateBoardh R p b) ppsR)) 
        -- Generates 2 lists of boards, only kept if there are no responses to new board, from the predicted enemy doms, that score t 
      in 
        getBoards
          ([newBoard | newBoard <- hBoardsL,
            speedChecker(doubleBlockA (possPlays (rebuildBoard (getHistory newBoard)) pEDs) newBoard oppScore t1 t2)],
          [newBoard | newBoard <- hBoardsR,
            speedChecker(doubleBlockA (possPlays (rebuildBoard (getHistory newBoard)) pEDs) newBoard oppScore t1 t2)])
    -- Helper function, checks which doms fulfill the score conditions and then list comp     
    doubleBlockA :: ([Dom],[Dom]) -> DomBoard -> Int -> Int -> Int -> ([Dom],[Dom])
    doubleBlockA (possLeft,possRight) b score t1 t2 = 
      ([d| d <- possLeft, (scoreDom d L b == (t1 - score)) || (scoreDom d L b == (t2 - score))]
        ,[d| d <- possRight, (scoreDom d R b == (t1 - score)) || (scoreDom d R b == (t2 - score))])
        
    -- converts DomBoards into Doms
    -- getBoards converts the DomBoards produced in avoidN into usable ([Dom],[Dom]) lists that can be played.
    getBoards :: ([DomBoard],[DomBoard]) -> ([Dom],[Dom])
    getBoards (nScoreL,nScoreR) = 
            (map head (map rebuildBoard (map getHistory nScoreL)), map last (map rebuildBoard (map getHistory nScoreR)))
     
    -- Saves about half a gig of RAM in avoidN and cuts down computation time
    -- Checks if both arrays are empty
    speedChecker :: ([Dom],[Dom]) -> Bool
    speedChecker ([],[]) = True
    speedChecker (x,_) = False
    
    
    -- Similar to scoreN, returns a list of dominoes that score a given value
    -- from a given list
    scoreGoal :: ([Dom],[Dom]) -> DomBoard -> Int -> Int -> ([Dom],[Dom])
    scoreGoal (possLeft,possRight) b score t = 
      ([d| d <- possLeft, (scoreDom d L b == (t - score))]
        ,[d| d <- possRight, (scoreDom d R b == (t - score))])
   
        
    -- Sees how many response moves can be played for each of your doms.
    -- Zips a list of playable dominos with the number of response moves against
    -- that play 
    zipLength :: Hand -> History -> Player -> ([(Dom, Int)],[(Dom, Int)])
    zipLength h history p =
      let 
        lAL = lookAhead board L pEDs 
        lAR = lookAhead board R pEDs
        board = rebuildBoard history
        pEDs = possibleEnemyDoms h history p
        (ppsL,ppsR) = possPlays board h
      in 
        ((zip ppsL (map lAL ppsL)), (zip ppsR (map lAR ppsR)))
        

    -- Calculates the number of response plays to a hypothetical play
    lookAhead :: [Dom] -> End -> [Dom] -> Dom -> Int
    lookAhead board L pEDs d = getLen (possPlays (hypoPlay board d L) pEDs)
    lookAhead board R pEDs d = getLen (possPlays (hypoPlay board d R) pEDs)

    -- Adds the length of two tupled arrays
    getLen :: ([Dom],[Dom]) -> Int 
    getLen (ppsL,ppsR) = length ppsL + length ppsR
        
       
    --------------------------------------------------------------------------
    -- Utility functions related to guessing the opposition's dominoes
    --------------------------------------------------------------------------
    -- Returns a list of Dominoes that the opposition might have
    possibleEnemyDoms :: Hand -> History -> Player -> [Dom] 
    possibleEnemyDoms h history p = 
      let 
        eKs = getKnockingMoves history p -- Every time the opposition knocks
        eKl = length eKs -- number of times the opposition knocked
        impDoms = getImpossibleDoms history eKs eKl []
        -- excluded doms are those in players hand, on board
        -- and those calculated in getImpossibleDoms
        domList = h ++ (rebuildBoard history) ++ impDoms
      in 
        nub (filter (\d -> not ((elem d domList) || elem (swapDom d) domList) ) domSet)
             
    -- Calculates which dominoes weren't played each time the opposition knocks
    -- calls possplays with domSet, removes all doms in that call
    getImpossibleDoms :: History -> [Int] -> Int -> [Dom] -> [Dom]
    getImpossibleDoms [] _ _ _ = []
    getImpossibleDoms history eKs 0 impDoms = impDoms
    getImpossibleDoms history eKs eKl impDoms = 
      let 
        (idL, idR) = getImpossibleDomsA history (eKs!!(eKl-1))
        iD = idL ++ idR
      in 
        getImpossibleDoms history eKs (eKl-1) impDoms++iD
        
    -- Recurses through the given list and calls possPlays
    getImpossibleDomsA :: History -> Int -> ([Dom],[Dom])
    getImpossibleDomsA history fEK = possPlays (rebuildBoardTo history (fEK-1)) domSet 
        
        
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
    getKnockingMovesA pMoves 1 knockingMoves = reverse knockingMoves
    getKnockingMovesA pMoves c knockingMoves = 
      getKnockingMovesA pMoves (c-1) (knockingMoves ++ isKnocking pMoves c)

    -- helper function,   
    isKnocking :: [Int] -> Int -> [Int]
    isKnocking pMoves c 
      | (elem c pMoves && elem (c-1) pMoves) = [c]
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
    
    -- Creates a shortboard from the two end dominoes, saves memory
    getShortB :: DomBoard -> [Dom]
    getShortB InitBoard = []
    getShortB (Board a b _) = [a,b]
    
    -- Orients the score into (myScore, oppositionScore) using given Player
    getScore :: Player -> Scores -> (Int,Int)
    getScore P1 (s1,s2) = (s1,s2)
    getScore P2 (s1,s2) = (s2,s1)
    
    -- Returns the Player data type of the opposition
    getOpposition :: Player -> Player
    getOpposition P1 = P2
    getOpposition P2 = P1
    
    -- Shorthand for null
    n :: [Dom] -> Bool
    n [] = True
    n a = False

    -- Shorthand for not null
    nn :: [Dom] -> Bool
    nn [] = False
    nn a = True
    
    
    -------------------------------------------------------------------------------------------
    -- Less used / obsolete utility functions
    -------------------------------------------------------------------------------------------
    -- Makes a list of how many of each spot number exist in a given list of Doms, zipped with spot value
    -- Can't pattern match -1 for some reason
    countAllSpots :: [Dom] -> Int -> [Int] -> [Dom]
    countAllSpots _ t spotList
      | (t <= -1) = zip [6,5,4,3,2,1,0] spotList
    countAllSpots dList t spotList = countAllSpots dList (t-1) (spotList ++ [howMany dList t])
    
    -- How many of a given pip count exist in a given list of doms
    howMany :: [Dom] -> Int -> Int
    howMany dList t  = length (filter (\(a,b) -> (a==t || b==t)) dList)
    
    -- Returns the best spot value
    bS :: Hand -> Int 
    bS h = 
      let
        spotList = countAllSpots h 6 []
        (bestSpot,_) = getHighest spotList
      in
        bestSpot
    
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
      
    -- How many doms have been played so far.
    numDomsPlayed :: History -> Int
    numDomsPlayed [] = 0
    numDomsPlayed history = thrd (maximumBy(comparing thrd) history)
      
    -- fst / snd / thrd. Custom tuple call
    thrd :: (Dom, Player, MoveNum) -> Int
    thrd (_,_,a) = a
      
    --------------------------------------------------------------------------------------------------
    -- Re-purposed HSD.
    -- End specific implementation. 
    -- This allows tactics to hsd from a single end, maximising score gain (avoiding just using head xs)
    -- and without sacrificing the specific ability of the Tactic itself, as would be lost with hsd(Left,Right)
    --------------------------------------------------------------------------------------------------
    hsdPlayer2 :: Hand -> DomBoard -> Player -> Scores -> End -> (Dom,End)
    hsdPlayer2 h b p s end = (d,e)
                     where (d,e)=hsdPlayerRepurp h b end
                     
    hsdPlayerRepurp :: Hand->DomBoard->End->(Dom,End)
    hsdPlayerRepurp h InitBoard _ = (md,L)
      where
        dscores = zip h (map (\ (d1,d2)->score53 (d1+d2)) h)
        (md,ms) = maximumBy (comparing snd) dscores
    hsdPlayerRepurp h b L = 
      let
        ld=  leftdrops h b
        lscores = zip ld (map (\d->(scoreDom d L b)) ld) -- [(Dom, score)]
        (lb,ls) = if (not(null lscores)) then (maximumBy (comparing snd) lscores) else ((0,0),-1) 
      in
        (lb,L)
    hsdPlayerRepurp h b R = 
      let
        rd = rightdrops h b
        rscores = zip rd (map (\d->(scoreDom d R b)) rd)
        (rb,rs) = if (not(null rscores)) then (maximumBy (comparing snd) rscores) else ((0,0),-1)
      in
        (rb,R)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
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
    
    -- Update hypothetical board in look ahead searches
    updateBoardh :: End->Player->DomBoard -> Dom ->DomBoard
    updateBoardh e p b d
        |e==L = playleft p d b
        |otherwise = playright p d b
    
    getLR :: ([Dom],[Dom]) -> ([Dom],End)
    getLR (a,b) 
      | null a = (b,R)
      | otherwise = (a,L)
    
    -- Play a hypothetical domino in a look ahead search
    hypoPlay :: [Dom] -> Dom -> End -> [Dom]
    hypoPlay [] d _ = [d]
    hypoPlay board (a,b) L 
      | fst (head board) == a = ((b,a)):board
      | otherwise = (a,b):board
    hypoPlay board (a,b) R  
      | snd (last board) == b = board ++ [(b,a)]
      | otherwise = board ++ [(a,b)] 
