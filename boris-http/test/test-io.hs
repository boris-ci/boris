import           Disorder.Core.Main

import qualified Test.IO.Boris.Http.Db.Query
import qualified Test.IO.Boris.Http.Db.Schema

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Http.Db.Schema.tests
    , Test.IO.Boris.Http.Db.Query.tests
    ]
