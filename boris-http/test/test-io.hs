import           Disorder.Core.Main

import qualified Test.IO.Boris.Http.Store.Postgres.Schema
import qualified Test.IO.Boris.Http.Store.Postgres.Query

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Http.Store.Postgres.Schema.tests
    , Test.IO.Boris.Http.Store.Postgres.Query.tests
    ]
