module BuildInfo_boris_service where
import Prelude
data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String, gitVersion :: String }
buildInfo :: RuntimeBuildInfo
buildInfo = RuntimeBuildInfo "0.0.1" "20190303114536" "de30d6e-M"
buildInfoVersion :: String
buildInfoVersion = "0.0.1-20190303114536-de30d6e-M"
