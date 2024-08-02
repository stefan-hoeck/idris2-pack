module Pack.Runner.Uninstall

import Pack.Config
import Pack.Core

%default total

--------------------------------------------------------------------------------
--          Uninstalling Pack
--------------------------------------------------------------------------------

export covering
uninstallPack :
     {auto _ : HasIO io}
  -> {auto _ : Env}
  -> EitherT PackErr io ()
uninstallPack = do
  info "Uninstalling pack"
  rmDir packDir
