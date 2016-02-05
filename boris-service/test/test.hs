import           Disorder.Core.Main

import qualified Test.Boris.Service.Git

main :: IO ()
main =
  disorderMain [
      Test.Boris.Service.Git.tests
    ]
