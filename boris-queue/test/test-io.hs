import qualified Test.IO.Boris.Queue

import           Disorder.Core.Main

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Queue.tests
    ]
