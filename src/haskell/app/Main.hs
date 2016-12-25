module Main where

import Brainfuck (play)

main :: IO ()
main = do
    [n, _] <- fmap (map read . words) getLine
    input  <- fmap (take n) getLine
    getContents >>= putStrLn . flip play input
--    hGetContents h >>= runProgram input >>= either print (\_ -> putStrLn "")
