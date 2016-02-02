import           Disorder.Core.Main

import qualified Test.Boris.X

main :: IO ()
main =
  disorderMain [
      Test.Boris.X.tests
    ]
