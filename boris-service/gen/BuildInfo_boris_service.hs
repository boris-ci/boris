module BuildInfo_boris_service where
import Prelude
data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String, gitVersion :: String }
buildInfo :: RuntimeBuildInfo
buildInfo = RuntimeBuildInfo "0.0.1" "20190306214323" "0050a0a-M"
buildInfoVersion :: String
buildInfoVersion = "0.0.1-20190306214323-0050a0a-M"
