import Brainfuck (play)
import Test.HUnit hiding (test)

type Input = String
type ExpectedOutput = String

test :: FilePath -> String -> IO String
test filepath input = fmap (flip play input) (readFile filepath)

main :: IO ()
main = mapM_ assertBrainf tests
  where
    assertBrainf (filepath, input, expectedOutput) =
      test filepath input >>= assertEqual "" expectedOutput

tests :: [(FilePath, Input, ExpectedOutput)]
tests =
  [ ( "../../examples/CellSize.bf", "", "8 bit cells\n" )
  , ( "../../examples/helloworld.bf", "", "Hello World!" )
  , ( "../../examples/input00", "", "Hello World!" )
  , ( "../../examples/input01", "abcxyz", "bcdwxy" )
  , ( "../../examples/input02", "pm", "sp\nPROCESS TIME OUT. KILLED!!!" )
  , ( "../../examples/ROT13.bf"
    , "abcdeghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    , "nopqrtuvwxyzabcdefghijklmN\nPROCESS TIME OUT. KILLED!!!" )
  ]
