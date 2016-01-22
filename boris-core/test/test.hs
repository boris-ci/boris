import qualified Test.Boris.Core.Serial.Toml

import           Disorder.Core.Main


main :: IO ()
main =
  disorderMain [
      Test.Boris.Core.Serial.Toml.tests
    ]
