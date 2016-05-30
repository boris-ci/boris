{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Dashboard (
    dashboard
  ) where


import           Airship (Resource (..), defaultResource)

import           Boris.Http.Data
import qualified Boris.Http.Html.Template as T

import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)


dashboard :: Env -> ConfigLocation -> Resource IO
dashboard _env _c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = return [
          (,) "text/html" $
             T.render T.dashboard
        ]
    }
