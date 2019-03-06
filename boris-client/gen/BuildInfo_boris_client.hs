module BuildInfo_boris_client where
import Prelude
data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String, gitVersion :: String }
buildInfo :: RuntimeBuildInfo
buildInfo = RuntimeBuildInfo "0.0.1" "20190306213123" "96eb607-M"
buildInfoVersion :: String
buildInfoVersion = "0.0.1-20190306213123-96eb607-M"
