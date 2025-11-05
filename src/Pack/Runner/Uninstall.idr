module Pack.Runner.Uninstall

import Pack.Config.Environment
import Pack.Config.Types
import Pack.Core.IO
import Pack.Core.Logging
import Pack.Core.Types

%default total

--------------------------------------------------------------------------------
--          Uninstalling Pack
--------------------------------------------------------------------------------

export covering
uninstallPack :
     {auto _  : LogRef}
  -> {auto pd : PackDirs}
  -> {auto _  : HasIO io}
  -> EitherT PackErr io ()
uninstallPack = do
  info "Uninstalling pack"
  let msg :=
    """
    This command will remove pack together with the Idris2 compiler
    managed by pack and all installed libraries. You might need
    to manually remove some of the executables managed by pack
    located in `\{pd.bin}`.

    Please note that your personal settings at `\{userDir}` will not
    be removed.
    """
  Yes <- confirm Info msg | _ => throwE SafetyAbort
  rmDir pd.state
  rmDir pd.cache
  rmFile packExec
  rmFile (pathExec "idris2")
  rmFile (pathExec "idris2-lsp")
