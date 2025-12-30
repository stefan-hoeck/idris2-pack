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
  defaultCommand = Configured PrintHelp

  usage = usageInfo

  cmdName = name

  appName = "pack"

  defaultLevel (Configured Build           ) = Build
  defaultLevel (Configured BuildDeps       ) = Build
  defaultLevel (Configured Typecheck       ) = Build
  defaultLevel (Configured Clean           ) = Build
  defaultLevel (Configured CleanBuild      ) = Build
  defaultLevel (Configured Exec            ) = Warning
  defaultLevel (Configured New             ) = Build
  defaultLevel (Configured Repl            ) = Warning
  defaultLevel (Configured Install         ) = Build
  defaultLevel (Configured InstallApp      ) = Build
  defaultLevel (Configured Remove          ) = Build
  defaultLevel (Configured RemoveApp       ) = Build
  defaultLevel (Configured Run             ) = Warning
  defaultLevel (Configured Test            ) = Warning
  defaultLevel (Configured Update          ) = Build
  defaultLevel (Configured Fetch           ) = Build
  defaultLevel (Configured PackagePath     ) = Silence
  defaultLevel (Configured LibsPath        ) = Silence
  defaultLevel (Configured DataPath        ) = Silence
  defaultLevel (Configured AppPath         ) = Silence
  defaultLevel (Configured Switch          ) = Build
  defaultLevel (Configured UpdateDB        ) = Build
  defaultLevel (Configured CollectGarbage  ) = Info
  defaultLevel (Configured Info            ) = Cache
  defaultLevel (Configured Query           ) = Cache
  defaultLevel (Configured Fuzzy           ) = Cache
  defaultLevel (Configured Completion      ) = Silence
  defaultLevel (Trivial    CompletionScript) = Silence
  defaultLevel (Configured Uninstall       ) = Info
  defaultLevel (Configured PrintHelp       ) = Silence

  desc = cmdDesc

  ArgTypes (Configured Build           ) = [Maybe PkgOrIpkg]
  ArgTypes (Configured BuildDeps       ) = [Maybe PkgOrIpkg]
  ArgTypes (Configured Typecheck       ) = [Maybe PkgOrIpkg]
  ArgTypes (Configured Clean           ) = [Maybe PkgOrIpkg]
  ArgTypes (Configured CleanBuild      ) = [Maybe PkgOrIpkg]
  ArgTypes (Configured Repl            ) = [Maybe (File Abs)]
  ArgTypes (Configured Exec            ) = [File Abs, CmdArgList]
  ArgTypes (Configured Install         ) = [List PkgName]
  ArgTypes (Configured InstallApp      ) = [List PkgName]
  ArgTypes (Configured Remove          ) = [List PkgName]
  ArgTypes (Configured RemoveApp       ) = [List PkgName]
  ArgTypes (Configured Run             ) = [Maybe PkgOrIpkg, CmdArgList]
  ArgTypes (Configured Test            ) = [PkgName, CmdArgList]
  ArgTypes (Configured New             ) = [PkgType, Body]
  ArgTypes (Configured Update          ) = []
  ArgTypes (Configured Fetch           ) = []
  ArgTypes (Configured PackagePath     ) = []
  ArgTypes (Configured LibsPath        ) = []
  ArgTypes (Configured DataPath        ) = []
  ArgTypes (Configured AppPath         ) = [PkgName]
  ArgTypes (Configured Switch          ) = [DBName]
  ArgTypes (Configured UpdateDB        ) = []
  ArgTypes (Configured CollectGarbage  ) = []
  ArgTypes (Configured Info            ) = []
  ArgTypes (Configured Query           ) = [PkgQuery]
  ArgTypes (Configured Fuzzy           ) = [FuzzyQuery]
  ArgTypes (Configured Completion      ) = [String, String]
  ArgTypes (Trivial    CompletionScript) = [String]
  ArgTypes (Configured Uninstall       ) = []
  ArgTypes (Configured PrintHelp       ) = [Maybe Cmd]

  readCommand_ n = lookup n namesAndCommands

  adjConfig_ (Configured Switch) [db] c = case db == MkDBName "latest" of
    True  => do
      latest <- copyLatest
      pure $ {collection := latest} c
    False => pure $ {collection := db} c

  -- we trust pack to be safe to install even though it uses
  -- custom build hooks
  adjConfig_ (Configured Update) []  c = pure $ {safetyPrompt := False} c
  adjConfig_ _      _   c = pure c

  readArgs (Configured Build           ) = %search
  readArgs (Configured BuildDeps       ) = %search
  readArgs (Configured Typecheck       ) = %search
  readArgs (Configured Clean           ) = %search
  readArgs (Configured CleanBuild      ) = %search
  readArgs (Configured Repl            ) = %search
  readArgs (Configured Exec            ) = %search
  readArgs (Configured Install         ) = %search
  readArgs (Configured InstallApp      ) = %search
  readArgs (Configured Remove          ) = %search
  readArgs (Configured RemoveApp       ) = %search
  readArgs (Configured Run             ) = %search
  readArgs (Configured Test            ) = %search
  readArgs (Configured New             ) = %search
  readArgs (Configured Update          ) = %search
  readArgs (Configured Fetch           ) = %search
  readArgs (Configured PackagePath     ) = %search
  readArgs (Configured LibsPath        ) = %search
  readArgs (Configured DataPath        ) = %search
  readArgs (Configured AppPath         ) = %search
  readArgs (Configured Switch          ) = %search
  readArgs (Configured UpdateDB        ) = %search
  readArgs (Configured CollectGarbage  ) = %search
  readArgs (Configured Info            ) = %search
  readArgs (Configured Query           ) = %search
  readArgs (Configured Fuzzy           ) = %search
  readArgs (Configured Completion      ) = %search
  readArgs (Trivial    CompletionScript) = %search
  readArgs (Configured Uninstall       ) = %search
  readArgs (Configured PrintHelp       ) = %search

isFetch : Cmd -> Bool
isFetch (Configured Fetch) = True
isFetch _     = False

data IsConfigured : Command c => CommandWithArgs c -> Type where
  ItIsConfigured : IsConfigured (Configured _ ** _)

runConfiguredCmd :
     {auto _ : HasIO io}
  -> {auto _ : PackDirs}
  -> {auto _ : TmpDir}
  -> {auto _ : LibCache}
  -> {auto _ : LineBufferingCmd}
  -> CurDir
  -> MetaConfig
  -> (fetch : Bool)
  -> (cmd : CommandWithArgs Cmd)
  -> {auto _ : IsConfigured cmd}
  -> EitherT PackErr io ()
runConfiguredCmd cd mc fetch = go
  where
    go : (cmd : CommandWithArgs Cmd) -> IsConfigured cmd => EitherT PackErr io ()
    go (Configured Completion ** [a,b])  = env mc fetch >>= complete a b
    go (Configured Query  ** [MkQ m s])  = env mc fetch >>= query m s
    go (Configured Fuzzy ** [MkFQ m s])  = idrisEnv mc fetch >>= fuzzy m s
    go (Configured UpdateDB ** [])       = updateDB
    go (Configured CollectGarbage ** []) = env mc fetch >>= garbageCollector
    go (Configured Run ** [p,args])      = idrisEnv mc fetch >>= runApp !(refinePkg p) args
    go (Configured Test ** [p,args])     = idrisEnv mc fetch >>= runTest p args
    go (Configured Exec ** [p,args])     = idrisEnv mc fetch >>= exec p args
    go (Configured Repl ** [p])          = idrisEnv mc fetch >>= idrisRepl p
    go (Configured Build ** [p])         = idrisEnv mc fetch >>= build !(refinePkg p)
    go (Configured BuildDeps ** [p])     = idrisEnv mc fetch >>= buildDeps !(refinePkg p)
    go (Configured Typecheck ** [p])     = idrisEnv mc fetch >>= typecheck !(refinePkg p)
    go (Configured Clean ** [p])         = idrisEnv mc fetch >>= clean !(refinePkg p)
    go (Configured CleanBuild ** [p])    = do p <- refinePkg p
                                              e <- idrisEnv mc fetch
                                              clean p e >> build p e
    go (Configured PrintHelp ** [c])     = putStrLn (usageDesc c)
    go (Configured Install ** [ps])      = idrisEnv mc fetch >>= \e => installLibs ps
    go (Configured Remove ** [ps])       = idrisEnv mc fetch >>= \e => removeLibs ps
    go (Configured InstallApp ** [ps])   = idrisEnv mc fetch >>= \e => installApps ps
    go (Configured RemoveApp ** [ps])    = idrisEnv mc fetch >>= \e => removeApps ps
    go (Configured Update ** [])         = idrisEnv mc fetch >>= update
    go (Configured Fetch ** [])          = idrisEnv mc fetch >>= \e => install []
    go (Configured PackagePath ** [])    = env mc fetch >>= packagePathDirs >>= putStrLn . interpolate
    go (Configured LibsPath ** [])       = env mc fetch >>= packageLibDirs  >>= putStrLn . interpolate
    go (Configured DataPath ** [])       = env mc fetch >>= packageDataDirs >>= putStrLn . interpolate
    go (Configured AppPath ** [n])       = env mc fetch >>= appPath n
    go (Configured Info ** [])           = env mc fetch >>= printInfo
    go (Configured New ** [pty,p])       = idrisEnv mc fetch >>= new cd pty p
    go (Configured Switch ** [db])       = do
            env <- idrisEnv mc fetch
            install []
            writeCollection
    go (Configured Uninstall ** [])      = uninstallPack @{metaConfigToLogRef @{mc}}

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
           let fetch := isFetch (fst parsedArgs.cmd)
           linebuf  <- getLineBufferingCmd
           runConfiguredCmd cd mc fetch (Configured cmd ** args)
