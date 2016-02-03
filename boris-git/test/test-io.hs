import           Disorder.Core.Main

import qualified Test.IO.Boris.Git


main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Git.tests
    ]
