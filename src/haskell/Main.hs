import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Char
import Data.Word
import System.IO
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

($>) :: Functor f => f a -> b -> f b
($>) = flip $ fmap . const

type Byte = Word8
type Tape = ([Byte], Byte, [Byte])
type Input = [Word8]
type BrainfState = (Tape, Input, Integer)
data BrainfError = Overflow | PError ParseError
  deriving Eq

instance Show BrainfError where
  show e = case e of
    Overflow  -> "\nPROCESS TIME OUT. KILLED!!!"
    PError pe -> "PError: " ++ show pe

type Brainf a = StateT BrainfState (ExceptT BrainfError IO) a

runBrainf :: Input -> Brainf a -> IO (Either BrainfError a)
runBrainf i s = fmap (fmap fst) . runExceptT . runStateT s $ initialState
  where
    initialState :: BrainfState
    initialState = ((repeat 0, 0, repeat 0), i, 0)

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
-- | withCurrentIO - Apply an IO operation on the byte the data pointer is at
withCurrentIO :: (Byte -> IO Byte) -> Brainf ()
withCurrentIO f =
  get >>= \((l, b, r), i, ctr) -> liftIO (f b) >>= \b' -> put ((l, b', r), i, ctr)

------------------------------------------------------------------------------
-- | putByte - Puts a byte on standard output
putByte :: Byte -> IO Byte
putByte b = putChar (chr $ fromIntegral b) >> return b

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
withCheck bf = tick check >>= \b -> when (not b) bf

------------------------------------------------------------------------------
-- | loop - Loop until the current byte is zero
loop :: [Brainf ()] -> Brainf ()
loop body = withCheck (sequence_ body) >> withCheck (loop body)

------------------------------------------------------------------------------
-- | brainfParser - Parse input into a Brainf operation
brainfParser :: GenParser Char () (Brainf ())
brainfParser = (noneOf ",.+-[]<>" $> return ())
  <|> (char '+' $> (tick $ withCurrent (+1)))
  <|> (char '-' $> (tick $ withCurrent (\b -> b-1)))
  <|> (char '.' $> (tick $ withCurrentIO putByte))
  <|> (char ',' $> (tick $ getByte))
  <|> (char '<' $> (tick $ moveLeft))
  <|> (char '>' $> (tick $ moveRight))
  <|> fmap loop (between (char '[') (char ']') (many brainfParser))

play :: Handle -> IO ()
play h = do
    [n, m]  <- fmap (map read . words) (hGetLine h)
    input   <- fmap (map (fromIntegral . ord) . take n) (hGetLine h)
    hGetContents h >>= runProgram input >>= either print (\_ -> putStrLn "")
  where
    parseBf :: String -> Either ParseError [Brainf ()]
    parseBf = parse (many1 brainfParser) ""
    outerLeft :: ParseError -> IO (Either BrainfError a)
    outerLeft = return . Left . PError
    outerRight :: Input -> [Brainf a] -> IO (Either BrainfError ())
    outerRight i = runBrainf i . sequence_
    runProgram :: Input -> String -> IO (Either BrainfError ())
    runProgram i p = either outerLeft (outerRight i) (parseBf p)

test :: FilePath -> IO ()
test filepath = openFile filepath ReadMode >>= play

main :: IO ()
main = play stdin
