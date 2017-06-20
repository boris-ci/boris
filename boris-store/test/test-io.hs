import qualified Test.IO.Boris.Store.Build
import qualified Test.IO.Boris.Store.Results
import qualified Test.IO.Boris.Store.Index
import qualified Test.IO.Boris.Store.Tick


import           Disorder.Core.Main

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Store.Build.tests
    , Test.IO.Boris.Store.Results.tests
    , Test.IO.Boris.Store.Index.tests
    , Test.IO.Boris.Store.Tick.tests
    ]
