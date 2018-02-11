import qualified Test.Boris.Representation.ApiV1

import           Disorder.Core.Main


main :: IO ()
main =
  disorderMain [
      Test.Boris.Representation.ApiV1.tests
    ]
