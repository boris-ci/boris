import           Disorder.Core.Main

import qualified Test.IO.Boris.Http.Store.Dynamo.Build
import qualified Test.IO.Boris.Http.Store.Dynamo.Results
import qualified Test.IO.Boris.Http.Store.Dynamo.Index
import qualified Test.IO.Boris.Http.Store.Dynamo.Tick
import qualified Test.IO.Boris.Http.Store.Postgres.Schema
import qualified Test.IO.Boris.Http.Store.Postgres.Query

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Http.Store.Dynamo.Build.tests
    , Test.IO.Boris.Http.Store.Dynamo.Results.tests
    , Test.IO.Boris.Http.Store.Dynamo.Index.tests
    , Test.IO.Boris.Http.Store.Dynamo.Tick.tests
    , Test.IO.Boris.Http.Store.Postgres.Schema.tests
    , Test.IO.Boris.Http.Store.Postgres.Query.tests
    ]
