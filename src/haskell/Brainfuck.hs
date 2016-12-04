module Brainfuck where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Char
import Data.Word
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

-- Why is this needed here? Shouldn't this be in Control.Monad.Trans.State?
instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> ma >>= \a -> return (a, s)

($>) :: Functor f => f a -> b -> f b
($>) = flip $ fmap . const

type Byte = Word8
type BrainfState = ([Byte], Byte, [Byte], Integer)
data BrainfError = Overflow | PError ParseError
  deriving (Eq, Show)

type Brainf a = StateT BrainfState (ExceptT BrainfError IO) a

runBrainf :: Brainf a -> IO (Either BrainfError a)
runBrainf s = fmap (fmap fst) . runExceptT . runStateT s $ initialState
  where
    initialState :: BrainfState
    initialState = (repeat 0, 0, repeat 0, 0)

incInstructionCounter :: Brainf ()
incInstructionCounter = modify (\(a, b, c, ctr) -> (a, b, c, ctr+1))

moveLeft :: Brainf ()
moveLeft = modify (\(l:ls, b, rs, ctr) -> (ls, l, b:rs, ctr))

moveRight :: Brainf ()
moveRight = modify (\(ls, b, r:rs, ctr) -> (b:ls, r, rs, ctr))

withCounter :: Brainf a -> Brainf a
withCounter bf = do
    (_, _, _, ctr) <- get
    when (ctr >= 10^5) (lift $ throwE Overflow) >> incInstructionCounter >> bf

------------------------------------------------------------------------------
-- | withCurrent - Apply a function on the byte the data pointer is at
withCurrent :: (Byte -> Byte) -> Brainf ()
withCurrent f = modify (\(l,b,r,ctr) -> (l,f b,r, ctr))

------------------------------------------------------------------------------
-- | withCurrentIO - Apply an IO operation on the byte the data pointer is at
withCurrentIO :: (Byte -> IO Byte) -> Brainf ()
withCurrentIO f =
  get >>= \(l,b,r,ctr) -> liftIO (f b) >>= \b' -> put (l,b',r,ctr)

------------------------------------------------------------------------------
-- | putByte - Puts a byte on standard output
putByte :: Byte -> IO Byte
putByte b = putChar (chr $ fromIntegral b) >> return b

------------------------------------------------------------------------------
-- | getByte - Gets a char from standard input and converts it into a Byte
getByte :: IO Byte
getByte = fmap (fromIntegral . ord) getChar

------------------------------------------------------------------------------
-- | loop - Loop until the current byte is zero
loop :: [Brainf ()] -> Brainf ()
loop body = do
  (_, b, _, _) <- get
  if b == 0 then withCounter $ return ()
  else sequence_ $ body ++ fmap withCounter [loop body]

------------------------------------------------------------------------------
-- | brainfParser - Parse input into a Brainf operation
brainfParser :: GenParser Char () (Brainf ())
brainfParser = (noneOf ",.+-[]<>" $> return ())
  <|> (char '+' $> (withCounter $ withCurrent (+1)))
  <|> (char '-' $> (withCounter $ withCurrent (\b -> b-1)))
  <|> (char '.' $> (withCounter $ withCurrentIO putByte))
  <|> (char ',' $> (withCounter $ withCurrentIO (\_ -> getByte)))
  <|> (char '<' $> (withCounter $ moveLeft))
  <|> (char '>' $> (withCounter $ moveRight))
  <|> fmap loop (between (char '[') (char ']') (many brainfParser))

play program = either outerLeft outerRight parseBf >>= either print (\_ -> putStrLn "")
  where
    parseBf = parse (many1 brainfParser) "" program
    outerLeft = return . Left . PError
    outerRight = runBrainf . sequence_

test :: FilePath -> IO ()
test filepath = readFile filepath >>= play
