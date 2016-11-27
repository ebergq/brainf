import           Control.Monad.State
import           Data.Char (ord, chr)
import qualified Data.Map as M
import           Data.Maybe (fromJust, fromMaybe)
import           System.IO

type InstructionState = (M.Map Int Char, M.Map Int Int)
type DataState = M.Map Int Int

type Program = String
type Input = String
type Output = String

data ProgramState = ProgramState
    { instrPtr   :: Int
    , dataPtr    :: Int
    , nbrOfOps   :: Int
    , instrState :: InstructionState
    , dataState  :: DataState
    , input      :: Input
    , output     :: Output
    }
  deriving (Eq, Show)

getData :: ProgramState -> Int
getData s = fromMaybe 0 (M.lookup (dataPtr s) (dataState s))

------------------------------------------------------------------------------
-- | initialize - Creates the initial program state from the instructions and
--                the input.
initialize :: Program -> Input -> ProgramState
initialize program input = ProgramState
    { instrPtr   = 0
    , dataPtr    = 0
    , nbrOfOps   = 0
    , instrState = go 0 [] (M.empty, M.empty) program
    , dataState  = M.empty
    , input      = input
    , output     = ""
    }
  where
    go :: Int -> [Int] -> InstructionState -> String -> InstructionState
    go _ _ ms []             = ms
    go n pis (m1, m2) (c:cs) = case c of
        '[' -> go (n+1) (n:pis) (m1', m2) cs
        ']' -> go (n+1) (tail pis) (m1', m2') cs
        _   -> go (n+1) pis (m1', m2) cs
      where
        m1' = M.insert n c m1
        m2' = M.insert n (head pis) (M.insert (head pis) n m2)

------------------------------------------------------------------------------
-- | brainf - State monad for executing BrainF__k programs.
brainf :: State ProgramState Output
brainf = do
    s <- get
    case isDone s of
        Just o -> return o
        _      -> case M.lookup (instrPtr s) (fst $ instrState s) of
            Just c -> modify (getCommand c) >> brainf
  where
    getCommand :: Char -> (ProgramState -> ProgramState)
    getCommand c = case c of
        '+' -> modifyDataValue (return . maybe 1 (\x -> (x+1) `mod` 256))
        '-' -> modifyDataValue (return . maybe 255 (\x -> (x-1) `mod` 256))
        '>' -> modifyDataPtr (+1)
        '<' -> modifyDataPtr (\x -> x-1)
        '.' -> printOutput
        ',' -> readInput
        '[' -> openLoop
        ']' -> closeLoop
    isDone :: ProgramState -> Maybe Output
    isDone s | isInstrPtrDone = return $ output s
             | isOverflow     = return $ output s ++ timeoutMessage
             | otherwise      = Nothing
      where
        isInstrPtrDone = instrPtr s >= M.size (fst $ instrState s)
        isOverflow     = nbrOfOps s >= 10^5
        timeoutMessage = "\nPROCESS TIME OUT. KILLED!!!"

------------------------------------------------------------------------------
-- | openLoop - Handle start of a loop.
openLoop :: ProgramState -> ProgramState
openLoop s = s
    { instrPtr = case getData s of
        0 -> fromJust $ M.lookup (instrPtr s) (snd $ instrState s)
        _ -> instrPtr s + 1
    , nbrOfOps = nbrOfOps s + 1
    }

------------------------------------------------------------------------------
-- | closeLoop - Handle end of loop.
closeLoop :: ProgramState -> ProgramState
closeLoop s = s
    { instrPtr = case getData s of
        0 -> instrPtr s + 1
        _ -> fromJust $ M.lookup (instrPtr s) (snd $ instrState s)
    , nbrOfOps = nbrOfOps s + 1
    }

------------------------------------------------------------------------------
-- | modifyDataValue - Increases or decreases the value pointed to by the
--                     data pointer.
modifyDataValue :: (Maybe Int -> Maybe Int) -> ProgramState -> ProgramState
modifyDataValue f s = s
    { instrPtr  = instrPtr s + 1
    , nbrOfOps  = nbrOfOps s + 1
    , dataState = M.alter f (dataPtr s) (dataState s)
    }

------------------------------------------------------------------------------
-- | modifyDataPtr - Apply a function on the data pointer.
modifyDataPtr :: (Int -> Int) -> ProgramState -> ProgramState
modifyDataPtr f s = s
    { instrPtr = instrPtr s + 1
    , nbrOfOps = nbrOfOps s + 1
    , dataPtr  = f $ dataPtr s
    }

------------------------------------------------------------------------------
-- printOutput - Add the current value pointed to by the data pointer to the
--               output.
printOutput :: ProgramState -> ProgramState
printOutput s = s
    { instrPtr = instrPtr s + 1
    , nbrOfOps = nbrOfOps s + 1
    , output   = output s ++ [chr $ getData s]
    }

------------------------------------------------------------------------------
-- readInput - Read a character from the input stream and set it as the
--             current data value.
readInput :: ProgramState -> ProgramState
readInput s = s
    { instrPtr  = instrPtr s + 1
    , nbrOfOps  = nbrOfOps s + 1
    , dataState = M.alter (\_ -> Just d) (dataPtr s) (dataState s)
    , input     = tail $ input s
    }
  where
    d = ord $ head $ input s

execute :: Program -> Input -> Output
execute program = evalState brainf . initialize program

play :: Handle -> IO ()
play h = do
    [n, m]  <- fmap (map read . words) (hGetLine h)
    input   <- fmap (take n) (hGetLine h)
    program <- fmap (filter isCommand . unlines) (replicateM m (hGetLine h))
    putStrLn $ execute program input
  where
    isCommand :: Char -> Bool
    isCommand c = c `elem` "+-<>.,[]"

test :: FilePath -> IO ()
test filePath = openFile filePath ReadMode >>= play

main :: IO ()
main = play stdin
