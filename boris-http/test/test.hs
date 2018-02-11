import           Disorder.Core.Main

import qualified Test.Boris.Http.Client

import qualified Test.Boris.Http.Store.Dynamo.Results

main :: IO ()
main =
  disorderMain [
      Test.Boris.Http.Client.tests
    , Test.Boris.Http.Store.Dynamo.Results.tests
    ]
