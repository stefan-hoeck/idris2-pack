module Pack.Runner.Uninstall

import Pack.Config.Types
import Pack.Core.IO
import Pack.Core.Logging
import Pack.Core.Types

%hide Pack.Config.Types.Env.packDir

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
  let msg := "$PACK_DIR: \{packDir}. Continue (yes/*no)?"
  "yes" <- prompt Info msg
    | _ => throwE SafetyAbort
  rmDir packDir
