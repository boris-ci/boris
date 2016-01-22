import qualified Test.IO.Boris.Store.Build
import qualified Test.IO.Boris.Store.Meta

import           Disorder.Core.Main

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Store.Build.tests
    , Test.IO.Boris.Store.Meta.tests
    ]
