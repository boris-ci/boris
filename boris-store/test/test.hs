import           Disorder.Core.Main

import qualified Test.Boris.Store.Results

main :: IO ()
main =
  disorderMain [
      Test.Boris.Store.Results.tests
    ]
