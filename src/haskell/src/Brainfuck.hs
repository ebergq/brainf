module Brainfuck
    ( play
    ) where

import Control.Monad (unless, when)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, get, modify, put, runStateT)
import Data.Char (chr, ord)
import Data.Word (Word8)
import Text.ParserCombinators.Parsec
import Text.Parsec.Error (ParseError)

($>) :: Functor f => f a -> b -> f b
($>) = flip $ fmap . const

type Byte = Word8
type Tape = ([Byte], Byte, [Byte])
type Program = String
type Input = [Byte]
type BrainfState = (Tape, Input, Integer)
data BrainfError = Overflow | PError ParseError
  deriving Eq

instance Show BrainfError where
  show e = case e of
    Overflow  -> "\nPROCESS TIME OUT. KILLED!!!"
    PError pe -> "PError: " ++ show pe

type Brainf a = StateT BrainfState (ExceptT BrainfError (Writer String)) a

runBrainf :: Input -> Brainf a -> String
runBrainf i s = transform . runWriter . runExceptT . runStateT s $ initialState
  where
    initialState :: BrainfState
    initialState = ((repeat 0, 0, repeat 0), i, 0)
    transform :: (Either BrainfError (a, BrainfState), String) -> String
    transform (x, w) = w ++ either show (\_ -> "") x

increaseCounter :: Brainf ()
increaseCounter = modify (\(t, i, ctr) -> (t, i, ctr + 1))

moveLeft :: Brainf ()
moveLeft = modify (\((l:ls, b, rs), i, ctr) -> ((ls, l, b:rs), i, ctr))

moveRight :: Brainf ()
moveRight = modify (\((ls, b, r:rs), i, ctr) -> ((b:ls, r, rs), i, ctr))

tick :: Brainf a -> Brainf a
tick bf = do
    (_, _, ctr) <- get
    when (ctr >= 10^5) (lift $ throwE Overflow) >> increaseCounter >> bf

------------------------------------------------------------------------------
-- | withCurrent - Apply a function on the byte the data pointer is at
withCurrent :: (Byte -> Byte) -> Brainf ()
withCurrent f = modify (\((l, b, r), i, ctr) -> ((l, f b, r), i, ctr))

------------------------------------------------------------------------------
-- | putByte - Puts the current byte on the output string
putByte :: Brainf ()
putByte = get >>= (\((_, b, _), _, _) -> tell [chr $ fromIntegral b])

------------------------------------------------------------------------------
-- | getByte - Gets a char from input and converts it into a Byte
getByte :: Brainf ()
getByte = modify (\((l, _, r), b:bs, ctr) -> ((l, b, r), bs, ctr))

------------------------------------------------------------------------------
-- | check - Get a boolean whether the current data byte is zero
check :: Brainf Bool
check = get >>= \((_, b, _), _, _) -> return $ b == 0

------------------------------------------------------------------------------
-- | withCheck - Only execute Brainf monad if current byte is non-zero
withCheck :: Brainf () -> Brainf ()
withCheck bf = tick check >>= flip unless bf

------------------------------------------------------------------------------
-- | loop - Loop until the current byte is zero
loop :: [Brainf ()] -> Brainf ()
loop body = withCheck (sequence_ body) >> withCheck (loop body)

------------------------------------------------------------------------------
-- | brainfParser - Parse input into a Brainf operation
brainfParser :: GenParser Char () (Brainf ())
brainfParser = (noneOf ",.+-[]<>" $> return ())
  <|> (char '+' $> tick (withCurrent (+1)))
  <|> (char '-' $> tick (withCurrent (\b -> b-1)))
  <|> (char '.' $> tick putByte)
  <|> (char ',' $> tick getByte)
  <|> (char '<' $> tick moveLeft)
  <|> (char '>' $> tick moveRight)
  <|> fmap loop (between (char '[') (char ']') (many brainfParser))

play :: Program -> String -> String
play program input = runProgram (toInput input) program
  where
    toInput :: String -> Input
    toInput = map $ fromIntegral . ord
    parseBf :: String -> Either ParseError [Brainf ()]
    parseBf = parse (many1 brainfParser) ""
    outerRight :: Input -> [Brainf a] -> String
    outerRight i = runBrainf i . sequence_
    runProgram :: Input -> String -> String
    runProgram i p = either (show . PError) (outerRight i) (parseBf p)
