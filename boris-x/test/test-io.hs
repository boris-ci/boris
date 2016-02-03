import           Disorder.Core.Main

import qualified Test.IO.Boris.X


main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.X.tests
    ]
