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
Command ConfiguredCmd where
  defaultCommand = PrintHelp

  usage = usageInfo

  cmdName = name

  appName = "pack"

  defaultLevel (Build           ) = Build
  defaultLevel (BuildDeps       ) = Build
  defaultLevel (Typecheck       ) = Build
  defaultLevel (Clean           ) = Build
  defaultLevel (CleanBuild      ) = Build
  defaultLevel (Exec            ) = Warning
  defaultLevel (New             ) = Build
  defaultLevel (Repl            ) = Warning
  defaultLevel (Install         ) = Build
  defaultLevel (InstallApp      ) = Build
  defaultLevel (Remove          ) = Build
  defaultLevel (RemoveApp       ) = Build
  defaultLevel (Run             ) = Warning
  defaultLevel (Test            ) = Warning
  defaultLevel (Update          ) = Build
  defaultLevel (Fetch           ) = Build
  defaultLevel (PackagePath     ) = Silence
  defaultLevel (LibsPath        ) = Silence
  defaultLevel (DataPath        ) = Silence
  defaultLevel (AppPath         ) = Silence
  defaultLevel (Switch          ) = Build
  defaultLevel (UpdateDB        ) = Build
  defaultLevel (CollectGarbage  ) = Info
  defaultLevel (Info            ) = Cache
  defaultLevel (Query           ) = Cache
  defaultLevel (Fuzzy           ) = Cache
  defaultLevel (Completion      ) = Silence
  defaultLevel (Uninstall       ) = Info
  defaultLevel (PrintHelp       ) = Silence

  desc = cmdDesc

  ArgTypes (Build           ) = [Maybe PkgOrIpkg]
  ArgTypes (BuildDeps       ) = [Maybe PkgOrIpkg]
  ArgTypes (Typecheck       ) = [Maybe PkgOrIpkg]
  ArgTypes (Clean           ) = [Maybe PkgOrIpkg]
  ArgTypes (CleanBuild      ) = [Maybe PkgOrIpkg]
  ArgTypes (Repl            ) = [Maybe (File Abs)]
  ArgTypes (Exec            ) = [File Abs, CmdArgList]
  ArgTypes (Install         ) = [List PkgName]
  ArgTypes (InstallApp      ) = [List PkgName]
  ArgTypes (Remove          ) = [List PkgName]
  ArgTypes (RemoveApp       ) = [List PkgName]
  ArgTypes (Run             ) = [Maybe PkgOrIpkg, CmdArgList]
  ArgTypes (Test            ) = [PkgName, CmdArgList]
  ArgTypes (New             ) = [PkgType, Body]
  ArgTypes (Update          ) = []
  ArgTypes (Fetch           ) = []
  ArgTypes (PackagePath     ) = []
  ArgTypes (LibsPath        ) = []
  ArgTypes (DataPath        ) = []
  ArgTypes (AppPath         ) = [PkgName]
  ArgTypes (Switch          ) = [DBName]
  ArgTypes (UpdateDB        ) = []
  ArgTypes (CollectGarbage  ) = []
  ArgTypes (Info            ) = []
  ArgTypes (Query           ) = [PkgQuery]
  ArgTypes (Fuzzy           ) = [FuzzyQuery]
  ArgTypes (Completion      ) = [String, String]
  ArgTypes (Uninstall       ) = []
  ArgTypes (PrintHelp       ) = [Maybe Cmd]

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ (Switch) [db] c = case db == MkDBName "latest" of
    True  => do
      latest <- copyLatest
      pure $ {collection := latest} c
    False => pure $ {collection := db} c

  -- we trust pack to be safe to install even though it uses
  -- custom build hooks
  adjConfig_ (Update) []  c = pure $ {safetyPrompt := False} c
  adjConfig_ _        _   c = pure c

  readArgs (Build           ) = %search
  readArgs (BuildDeps       ) = %search
  readArgs (Typecheck       ) = %search
  readArgs (Clean           ) = %search
  readArgs (CleanBuild      ) = %search
  readArgs (Repl            ) = %search
  readArgs (Exec            ) = %search
  readArgs (Install         ) = %search
  readArgs (InstallApp      ) = %search
  readArgs (Remove          ) = %search
  readArgs (RemoveApp       ) = %search
  readArgs (Run             ) = %search
  readArgs (Test            ) = %search
  readArgs (New             ) = %search
  readArgs (Update          ) = %search
  readArgs (Fetch           ) = %search
  readArgs (PackagePath     ) = %search
  readArgs (LibsPath        ) = %search
  readArgs (DataPath        ) = %search
  readArgs (AppPath         ) = %search
  readArgs (Switch          ) = %search
  readArgs (UpdateDB        ) = %search
  readArgs (CollectGarbage  ) = %search
  readArgs (Info            ) = %search
  readArgs (Query           ) = %search
  readArgs (Fuzzy           ) = %search
  readArgs (Completion      ) = %search
  readArgs (Uninstall       ) = %search
  readArgs (PrintHelp       ) = %search

public export
Command TrivialCmd where
  defaultCommand = CompletionScript

  usage = usageInfo

  cmdName = name

  appName = "pack"

  defaultLevel (CompletionScript) = Silence

  desc = cmdDesc

  ArgTypes (CompletionScript) = [String]

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ _ _ c = pure c

  readArgs (CompletionScript) = %search

public export
Command Cmd where
  defaultCommand = Configured PrintHelp

  usage = usageInfo

  cmdName = name

  appName = "pack"

  defaultLevel (Configured cmd) = defaultLevel cmd
  defaultLevel (Trivial    cmd) = defaultLevel cmd

  desc (Configured cmd) = cmdDesc cmd
  desc (Trivial    cmd) = cmdDesc cmd

  ArgTypes (Configured cmd) = ArgTypes cmd
  ArgTypes (Trivial    cmd) = ArgTypes cmd

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ (Configured cmd) args c = adjConfig_ cmd args c
  adjConfig_ (Trivial    cmd) args c = adjConfig_ cmd args c

  readArgs (Configured cmd) = readArgs cmd
  readArgs (Trivial    cmd) = readArgs cmd

isFetch : ConfiguredCmd -> Bool
isFetch Fetch = True
isFetch _     = False

runConfiguredCmd :
     {auto _ : HasIO io}
  -> {auto _ : PackDirs}
  -> {auto _ : TmpDir}
  -> {auto _ : LibCache}
  -> {auto _ : LineBufferingCmd}
  -> CurDir
  -> MetaConfig
  -> (fetch : Bool)
  -> (cmd : CommandWithArgs ConfiguredCmd)
  -> EitherT PackErr io ()
runConfiguredCmd cd mc fetch = go
  where
    go : (cmd : CommandWithArgs ConfiguredCmd) -> EitherT PackErr io ()
    go (Completion ** [a,b])  = env mc fetch >>= complete a b
    go (Query  ** [MkQ m s])  = env mc fetch >>= query m s
    go (Fuzzy ** [MkFQ m s])  = idrisEnv mc fetch >>= fuzzy m s
    go (UpdateDB ** [])       = updateDB
    go (CollectGarbage ** []) = env mc fetch >>= garbageCollector
    go (Run ** [p,args])      = idrisEnv mc fetch >>= runApp !(refinePkg p) args
    go (Test ** [p,args])     = idrisEnv mc fetch >>= runTest p args
    go (Exec ** [p,args])     = idrisEnv mc fetch >>= exec p args
    go (Repl ** [p])          = idrisEnv mc fetch >>= idrisRepl p
    go (Build ** [p])         = idrisEnv mc fetch >>= build !(refinePkg p)
    go (BuildDeps ** [p])     = idrisEnv mc fetch >>= buildDeps !(refinePkg p)
    go (Typecheck ** [p])     = idrisEnv mc fetch >>= typecheck !(refinePkg p)
    go (Clean ** [p])         = idrisEnv mc fetch >>= clean !(refinePkg p)
    go (CleanBuild ** [p])    = do p <- refinePkg p
                                   e <- idrisEnv mc fetch
                                   clean p e >> build p e
    go (PrintHelp ** [c])     = putStrLn (usageDesc c)
    go (Install ** [ps])      = idrisEnv mc fetch >>= \e => installLibs ps
    go (Remove ** [ps])       = idrisEnv mc fetch >>= \e => removeLibs ps
    go (InstallApp ** [ps])   = idrisEnv mc fetch >>= \e => installApps ps
    go (RemoveApp ** [ps])    = idrisEnv mc fetch >>= \e => removeApps ps
    go (Update ** [])         = idrisEnv mc fetch >>= update
    go (Fetch ** [])          = idrisEnv mc fetch >>= \e => install []
    go (PackagePath ** [])    = env mc fetch >>= packagePathDirs >>= putStrLn . interpolate
    go (LibsPath ** [])       = env mc fetch >>= packageLibDirs  >>= putStrLn . interpolate
    go (DataPath ** [])       = env mc fetch >>= packageDataDirs >>= putStrLn . interpolate
    go (AppPath ** [n])       = env mc fetch >>= appPath n
    go (Info ** [])           = env mc fetch >>= printInfo
    go (New ** [pty,p])       = idrisEnv mc fetch >>= new cd pty p
    go (Switch ** [db])       = do env <- idrisEnv mc fetch
                                   install []
                                   writeCollection
    go (Uninstall ** [])      = uninstallPack @{metaConfigToLogRef @{mc}}

||| Main application entry point (modulo error handling).
export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  args       <- getArgs'
  cd         <- CD <$> curDir
  parsedArgs <- liftEither $ parseOpts Cmd cd args
  case parsedArgs.cmd of
       (Trivial CompletionScript ** [f]) => putStrLn (completionScript f)
       (Configured cmd ** args)          => do
         pd <- getPackDirs
         withTmpDir $ do
           cache    <- emptyCache
           mc <- getConfig Cmd parsedArgs
           let fetch := isFetch cmd
           linebuf  <- getLineBufferingCmd
           runConfiguredCmd cd mc fetch (cmd ** args)
