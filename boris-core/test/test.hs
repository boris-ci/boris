import qualified Test.Boris.Core.Serial.Ref
import qualified Test.Boris.Core.Serial.Command

import           Disorder.Core.Main


main :: IO ()
main =
  disorderMain [
      Test.Boris.Core.Serial.Ref.tests
    , Test.Boris.Core.Serial.Command.tests
    ]
