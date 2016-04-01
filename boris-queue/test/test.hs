import           Disorder.Core.Main
import qualified Test.Boris.Queue

main :: IO ()
main =
  disorderMain [
      Test.Boris.Queue.tests
    ]
