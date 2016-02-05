import           Disorder.Core.Main

import qualified Test.IO.Boris.Service.Git
import qualified Test.IO.Boris.Service.Workspace

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Service.Git.tests
    , Test.IO.Boris.Service.Workspace.tests
    ]
