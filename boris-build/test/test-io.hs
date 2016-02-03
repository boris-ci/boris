import           Disorder.Core.Main

import qualified Test.IO.Boris.Build

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Build.tests
    ]
