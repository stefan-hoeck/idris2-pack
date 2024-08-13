module Pack.Runner

import Data.List.Quantifiers
import Data.IORef
import Data.SortedMap
import Pack.CmdLn
import Pack.CmdLn.Completion
import Pack.Config
import public Pack.Core
import Pack.Database
import Pack.Runner.Database
import Pack.Runner.Develop
import Pack.Runner.Query
import Pack.Runner.Install
import Pack.Runner.New
import Pack.Runner.Uninstall

public export
Command Cmd where
  defaultCommand = PrintHelp

  usage = usageInfo

  cmdName = name

  appName = "pack"

  defaultLevel Build            = Build
  defaultLevel BuildDeps        = Build
  defaultLevel Typecheck        = Build
  defaultLevel Clean            = Build
  defaultLevel CleanBuild       = Build
  defaultLevel Exec             = Warning
  defaultLevel New              = Build
  defaultLevel Repl             = Warning
  defaultLevel Install          = Build
  defaultLevel InstallApp       = Build
  defaultLevel Remove           = Build
  defaultLevel RemoveApp        = Build
  defaultLevel Run              = Warning
  defaultLevel Test             = Warning
  defaultLevel Update           = Build
  defaultLevel Fetch            = Build
  defaultLevel PackagePath      = Silence
  defaultLevel LibsPath         = Silence
  defaultLevel DataPath         = Silence
  defaultLevel AppPath          = Silence
  defaultLevel Switch           = Build
  defaultLevel UpdateDB         = Build
  defaultLevel CollectGarbage   = Info
  defaultLevel Info             = Cache
  defaultLevel Query            = Cache
  defaultLevel Fuzzy            = Cache
  defaultLevel Completion       = Silence
  defaultLevel CompletionScript = Silence
  defaultLevel Uninstall        = Info
  defaultLevel PrintHelp        = Silence

  desc = cmdDesc

  ArgTypes Build            = [Maybe PkgOrIpkg]
  ArgTypes BuildDeps        = [Maybe PkgOrIpkg]
  ArgTypes Typecheck        = [Maybe PkgOrIpkg]
  ArgTypes Clean            = [Maybe PkgOrIpkg]
  ArgTypes CleanBuild       = [Maybe PkgOrIpkg]
  ArgTypes Repl             = [Maybe (File Abs)]
  ArgTypes Exec             = [File Abs, CmdArgList]
  ArgTypes Install          = [List PkgName]
  ArgTypes InstallApp       = [List PkgName]
  ArgTypes Remove           = [List PkgName]
  ArgTypes RemoveApp        = [List PkgName]
  ArgTypes Run              = [Maybe PkgOrIpkg, CmdArgList]
  ArgTypes Test             = [PkgName, CmdArgList]
  ArgTypes New              = [PkgType, Body]
  ArgTypes Update           = []
  ArgTypes Fetch            = []
  ArgTypes PackagePath      = []
  ArgTypes LibsPath         = []
  ArgTypes DataPath         = []
  ArgTypes AppPath          = [PkgName]
  ArgTypes Switch           = [DBName]
  ArgTypes UpdateDB         = []
  ArgTypes CollectGarbage   = []
  ArgTypes Info             = []
  ArgTypes Query            = [PkgQuery]
  ArgTypes Fuzzy            = [FuzzyQuery]
  ArgTypes Completion       = [String, String]
  ArgTypes CompletionScript = [String]
  ArgTypes Uninstall        = []
  ArgTypes PrintHelp        = [Maybe Cmd]

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ Switch [db] c = case db == MkDBName "latest" of
    True  => do
      latest <- copyLatest
      pure $ {collection := latest} c
    False => pure $ {collection := db} c

  -- we trust pack to be safe to install even though it uses
  -- custom build hooks
  adjConfig_ Update []  c = pure $ {safetyPrompt := False} c
  adjConfig_ _      _   c = pure c

  readArgs Build            = %search
  readArgs BuildDeps        = %search
  readArgs Typecheck        = %search
  readArgs Clean            = %search
  readArgs CleanBuild       = %search
  readArgs Repl             = %search
  readArgs Exec             = %search
  readArgs Install          = %search
  readArgs InstallApp       = %search
  readArgs Remove           = %search
  readArgs RemoveApp        = %search
  readArgs Run              = %search
  readArgs Test             = %search
  readArgs New              = %search
  readArgs Update           = %search
  readArgs Fetch            = %search
  readArgs PackagePath      = %search
  readArgs LibsPath         = %search
  readArgs DataPath         = %search
  readArgs AppPath          = %search
  readArgs Switch           = %search
  readArgs UpdateDB         = %search
  readArgs CollectGarbage   = %search
  readArgs Info             = %search
  readArgs Query            = %search
  readArgs Fuzzy            = %search
  readArgs Completion       = %search
  readArgs CompletionScript = %search
  readArgs Uninstall        = %search
  readArgs PrintHelp        = %search

isFetch : Cmd -> Bool
isFetch Fetch = True
isFetch _     = False

||| Main application entry point (modulo error handling).
export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  pd <- getPackDir
  withTmpDir $ do
    cd       <- CD <$> curDir
    cache    <- emptyCache
    (mc,cmd) <- getConfig Cmd
    let fetch := isFetch (fst cmd)
    linebuf  <- getLineBufferingCmd
    case cmd of
      (Completion ** [a,b])     => env mc fetch >>= complete a b
      (CompletionScript ** [f]) => putStrLn (completionScript f)
      (Query  ** [MkQ m s])     => env mc fetch >>= query m s
      (Fuzzy ** [MkFQ m s])     => idrisEnv mc fetch >>= fuzzy m s
      (UpdateDB ** [])          => updateDB
      (CollectGarbage ** [])    => env mc fetch >>= garbageCollector
      (Run ** [p,args])         => idrisEnv mc fetch >>= runApp !(refinePkg p) args
      (Test ** [p,args])        => idrisEnv mc fetch >>= runTest p args
      (Exec ** [p,args])        => idrisEnv mc fetch >>= exec p args
      (Repl ** [p])             => idrisEnv mc fetch >>= idrisRepl p
      (Build ** [p])            => idrisEnv mc fetch >>= build !(refinePkg p)
      (BuildDeps ** [p])        => idrisEnv mc fetch >>= buildDeps !(refinePkg p)
      (Typecheck ** [p])        => idrisEnv mc fetch >>= typecheck !(refinePkg p)
      (Clean ** [p])            => idrisEnv mc fetch >>= clean !(refinePkg p)
      (CleanBuild ** [p])       => do p <- refinePkg p
                                      e <- idrisEnv mc fetch
                                      clean p e >> build p e
      (PrintHelp ** [c])        => putStrLn (usageDesc c)
      (Install ** [ps])         => idrisEnv mc fetch >>= \e => installLibs ps
      (Remove ** [ps])          => idrisEnv mc fetch >>= \e => removeLibs ps
      (InstallApp ** [ps])      => idrisEnv mc fetch >>= \e => installApps ps
      (RemoveApp ** [ps])       => idrisEnv mc fetch >>= \e => removeApps ps
      (Update ** [])            => idrisEnv mc fetch >>= update
      (Fetch ** [])             => idrisEnv mc fetch >>= \e => install []
      (PackagePath ** [])       => env mc fetch >>= packagePathDirs >>= putStrLn
      (LibsPath ** [])          => env mc fetch >>= packageLibDirs  >>= putStrLn
      (DataPath ** [])          => env mc fetch >>= packageDataDirs >>= putStrLn
      (AppPath ** [n])          => env mc fetch >>= appPath n
      (Info ** [])              => env mc fetch >>= printInfo
      (New ** [pty,p])          => idrisEnv mc fetch >>= new cd pty p
      (Switch ** [db])          => do
        env <- idrisEnv mc fetch
        install []
        writeCollection
      (Uninstall ** [])         => uninstallPack @{metaConfigToLogRef @{mc}}
