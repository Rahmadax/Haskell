{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

-- Oliver Rahman
-- 160153662
--

type Input  = Int
type Output = Int
 
-- A program is something that tells a computer how to
-- move from one configuration to another, how to
-- recognize when a configuration represents a valid
-- accept state, and so on.

class (Eq cfg) => ProgrammableComputer cfg where
  initialise   :: Program -> [Input] -> cfg
  getOutput    :: cfg -> Output
  acceptState  :: Program -> cfg -> Bool
  doNextMove   :: Program -> cfg -> cfg
  runFrom      :: Program -> cfg -> cfg
  runProgram   :: Program -> [Input] -> cfg
  -- Default implementation
  runProgram p is = runFrom p (initialise p is)


  
-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same 
--                 number of tokens, jump to instruction t
--
data Instruction
  = CLR {box :: Int} 
  | INC {box :: Int}
  | JEQ {box1   :: Int, 
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)
   
type Program = [Instruction]


-- PROBLEM 1. YOUR CODE HERE
-- --------------------------
-- Each instruction in a program refers to one or
-- more boxes.  What is the highest box number used
-- anywhere in the program?
maxBoxNum :: Program -> Int
maxBoxNum prog = maximum (map extract prog)

extract :: Instruction -> Int 
extract i = case i of
    CLR a -> a
    INC a -> a
    JEQ a b c -> maximum [a,b]


-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)
   
-- PROBLEM 2. YOUR CODE HERE
-- --------------------------
instance Show BATConfig where
    show (BATConfig boxes counter) = "boxes= " ++ (show boxes) ++ "; counter = " ++ (show counter)


-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
    initialise prog inputs = BATConfig{boxes = (0:inputs), counter = 0}

    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool 
    -- True ends the run. False continues
    acceptState prog (BATConfig _ counter) = (length prog) == counter

    -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg 
    doNextMove prog (BATConfig boxes counter) =
      let next = case (prog !! counter) of
                  CLR a -> BATConfig{ boxes = updateBox boxes a 0 counter, counter = counter+1}
                  INC a -> BATConfig{boxes = updateBox boxes a ((boxes!!a)+1) counter, counter = counter+1}
                  JEQ a b c -> BATConfig{boxes = boxes, counter = handleJump boxes a b c counter}
      in next

    -- PROBLEM 6: runFrom :: Program -> cfg -> cfg 
    runFrom prog config 
      | acceptState prog config = config
      | otherwise = runFrom prog (doNextMove prog config)

    -- PROBLEM 7: getOutput :: cfg -> Output 
    getOutput (BATConfig boxes _) = boxes!!1


updateBox :: [Int] -> Int -> Int -> Int -> [Int]
updateBox boxes a val newCounter = take a boxes ++ [val] ++ drop (a + 1) boxes

handleJump :: [Int] -> Int -> Int -> Int -> Int -> Int
handleJump boxes a b c counter
  | (boxes !! a == boxes !! b) = c
  | otherwise = counter+1

-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs  
execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)


-- PROBLEM 8. YOUR CODE HERE
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.
transpose :: Int -> Program -> Program
transpose n prog = map (trans n) prog

trans :: Int -> Instruction -> Instruction
trans n ins = case ins of
    JEQ a b c -> JEQ a b (c+n)
    otherwise -> ins

-- ---------------------------
-- join two programs together, so as to run one
-- after the other
(*->*) :: Program -> Program -> Program
p1 *->* p2 = p1++(transpose (length p1) p2)

-- ---------------------------
-- program to compute B1 = B1 + B2
adder :: Program
adder = [
  CLR 0,        -- Instruction 0: Clear box 0
  JEQ 0 2 6,    -- Instruction 1: Jumps to the end if box 2 == box 0
  INC 0,        -- Instruction 2: Increment box 0
  INC 1,        -- Instruction 3: Increment box 1
  JEQ 0 2 6,    -- Instruction 4: if box 0 == box 2, jump to the end.
  JEQ 0 0 2]    -- Instruction 5: Else, jump back to Instruction 2. 

-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)
copyBox :: Int -> Int -> Program
copyBox m n = [
  CLR n,       -- Instruction 0: Clears box n
  JEQ m n 5,   -- Instruction 1: Jumps to the end if box n == box m
  INC n,       -- Instruction 2: Increments box n
  JEQ m n 5,   -- Instruction 3: Jumps to the end if box n == box m
  JEQ 0 0 2    -- Instruction 4: Else, jumps to instruction 2
  ]

-- ---------------------------
-- program to compute B1 = Bx + By
addXY :: Int -> Int -> Program
addXY x y =[
  CLR 0,      -- Instruction 0: Clear box 0
  CLR 1,      -- Instruction 1: Clear box 1
  JEQ 0 x 7,  -- Instruction 2: Jumps to Instruction 7 if box 0 == box n
  INC 0,      -- Instruction 3: Increments box 0
  INC 1,      -- Instruction 4: Increments box 1
  JEQ 0 x 7,  -- Instruction 5: Jumps to Instruction 7 if box 0 == box x
  JEQ 0 0 3,  -- Instruction 6: Else, jump to Instruction 3
  CLR 0,      -- Instruction 7: Clear box 0
  INC 0,      -- Instruction 8: Increment box 0
  INC 1,      -- Instruction 9: Increment box 1
  JEQ 0 y 12, -- Instruction 10: Jumps to the end if box y == box 0
  JEQ 0 0 8   -- Instruction 11: Else, jump to Instruction 8
  ]

