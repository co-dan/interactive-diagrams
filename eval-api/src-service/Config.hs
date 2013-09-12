module Config where

import Data.Default
import Diagrams.Interactive.Eval.EvalSettings


sockFile :: FilePath
sockFile = "/idia/run/sock/control.sock"

username :: String
username = "vagrant"

workersDir :: FilePath
workersDir = "/idia/run/workers/"

cgroups :: FilePath
cgroups = "/cgroups/cpu/"

limSettings :: LimitSettings
limSettings = def {
     rlimits = Just def {
        totalMemoryLimit = ResourceLimits memlim memlim
        }
     -- , secontext  = Just "idia_restricted_t"
     -- , cgroupPath = Just $ cgroups </> "idiaworkers"
     }
  where memlim = ResourceLimit $ 104857600 * 6
                                 --- 100mb * 6
settings :: EvalSettings
settings = def
  { limitSet     = limSettings
  , pkgDatabases = ["/home/vagrant/.ghc/i386-linux-7.7.20130908/package.conf.d"]
  , verbLevel    = 5
  , preloadFile  = "../common/Preload.hs"
  }


