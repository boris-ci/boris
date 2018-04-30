import           Disorder.Core.Main

import qualified Test.IO.Boris.Http.Db.Account
import qualified Test.IO.Boris.Http.Db.Build
import qualified Test.IO.Boris.Http.Db.Project
import qualified Test.IO.Boris.Http.Db.Schema
import qualified Test.IO.Boris.Http.Db.Session

main :: IO ()
main =
  disorderMain [
      Test.IO.Boris.Http.Db.Account.tests
    , Test.IO.Boris.Http.Db.Build.tests
    , Test.IO.Boris.Http.Db.Project.tests
    , Test.IO.Boris.Http.Db.Schema.tests
    , Test.IO.Boris.Http.Db.Session.tests
    ]
