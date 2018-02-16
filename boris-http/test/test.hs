import           Disorder.Core.Main

import qualified Test.Boris.Http.Client

main :: IO ()
main =
  disorderMain [
      Test.Boris.Http.Client.tests
    ]
