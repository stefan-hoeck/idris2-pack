module Pack.Runner

import Data.IORef
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

ipkgFile : CurDir -> String -> (File Abs -> Cmd) -> Either PackErr Cmd
ipkgFile (CD dir) s f = f <$> readAbsFile dir s

pkgOrIpkg :  CurDir
          -> String
          -> (Either (File Abs) PkgName -> Cmd)
          -> Either PackErr Cmd
pkgOrIpkg (CD dir) s f = case readAbsFile dir s of
  Left  _    => Right (f . Right $ MkPkgName s)
  Right file => if isIpkgBody file.file
                   then Right (f $ Left file)
                   else Right (f . Right $ MkPkgName s)

Command Cmd where
  defaultLevel (Build x)              = Info
  defaultLevel (BuildDeps x)          = Info
  defaultLevel (Typecheck x)          = Info
  defaultLevel (Clean x)              = Info
  defaultLevel (Exec _ _)             = Warning
  defaultLevel (New _ _ _)            = Info
  defaultLevel (Repl x)               = Warning
  defaultLevel (Install xs)           = Info
  defaultLevel (Remove xs)            = Info
  defaultLevel (Run x strs)           = Warning
  defaultLevel Update                 = Info
  defaultLevel Fetch                  = Info
  defaultLevel PackagePath            = Silence
  defaultLevel LibsPath               = Silence
  defaultLevel DataPath               = Silence
  defaultLevel (AppPath x)            = Silence
  defaultLevel (Switch x)             = Info
  defaultLevel UpdateDB               = Info
  defaultLevel Info                   = Warning
  defaultLevel (Query x str)          = Warning
  defaultLevel (Fuzzy xs str)         = Warning
  defaultLevel (Completion str str1)  = Silence
  defaultLevel (CompletionScript str) = Silence
  defaultLevel PrintHelp              = Silence

  defaultCommand_ = PrintHelp

  readCommand_ _  []                         = Right PrintHelp
  readCommand_ _  ["help"]                   = Right PrintHelp
  readCommand_ _  ["info"]                   = Right Info
  readCommand_ _  ["update-db"]              = Right UpdateDB
  readCommand_ _  ["update"]                 = Right Update
  readCommand_ _  ["fetch"]                  = Right Fetch
  readCommand_ _  ["fuzzy", s]               = Right $ Fuzzy [] s
  readCommand_ _  ["fuzzy", p, s]            =
    Right $ Fuzzy (forget $ map MkPkgName $ split (',' ==) p) s

  readCommand_ _  ["query", s]               = Right $ Query PkgName s
  readCommand_ _  ["query", "dep", s]        = Right $ Query Dependency s
  readCommand_ _  ["query", "module", s]     = Right $ Query Module s
  readCommand_ cd ("exec" :: idr :: args)    =
    (`Exec` args) <$> readAbsFile curDir idr

  readCommand_ _  ["repl"]                   = Right $ Repl Nothing
  readCommand_ cd ["repl", s]                =
    Repl . Just <$> readAbsFile curDir s
  readCommand_ cd ("run" :: p :: args)       = pkgOrIpkg cd p (`Run` args)

  readCommand_ cd ["build", file]            = pkgOrIpkg cd file Build
  readCommand_ cd ["install-deps", file]     = pkgOrIpkg cd file BuildDeps
  readCommand_ cd ["typecheck", file]        = pkgOrIpkg cd file Typecheck
  readCommand_ cd ["clean", file]            = pkgOrIpkg cd file Clean
  readCommand_ _  ("install" :: xs)          =
    Right . Install $ map (\s => (Library, MkPkgName s)) xs

  readCommand_ _  ("remove" :: xs)           =
    Right . Remove $ map (\s => (Lib, MkPkgName s)) xs

  readCommand_ _  ("remove-app" :: xs)           =
    Right . Remove $ map (\s => (Bin, MkPkgName s)) xs

  readCommand_ _  ("install-app" :: xs)      =
    Right . Install $ map (\s => (App True, MkPkgName s)) xs

  readCommand_ _  ["completion",a,b]         = Right $ Completion a b

  readCommand_ _  ["completion-script",f]    = Right $ CompletionScript f
  readCommand_ _  ["package-path"]           = Right PackagePath
  readCommand_ _  ["libs-path"]              = Right LibsPath
  readCommand_ _  ["data-path"]              = Right DataPath
  readCommand_ _  ["app-path", n]            = Right $ AppPath (MkPkgName n)
  readCommand_ _  ["switch",db]              = Switch <$> readDBName db
  readCommand_ cd ["new", pty, p]            =
    New cd <$> readPkgType pty <*> readBody p

  readCommand_ _  xs                         = Left  $ UnknownCommand xs

  adjConfig (Switch db) c = case db == MkDBName "latest" of
    True  => do
      updateDB
      latest <- defaultColl
      pure $ {collection := latest} c
    False => pure $ {collection := db} c

  -- we trust pack to be safe to install even though it uses
  -- custom build hooks
  adjConfig Update c = pure $ {safetyPrompt := False} c
  adjConfig _      c = pure c

isFetch : Cmd -> Bool
isFetch Fetch = True
isFetch _     = False

||| Main application entry point (modulo error handling).
export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  pd       <- getPackDir
  td       <- mkTmpDir
  cd       <- CD <$> curDir
  cache    <- emptyCache
  (mc,cmd) <- getConfig Cmd
  let fetch := isFetch cmd
  finally (rmDir tmpDir) $ case cmd of
    Completion a b     => env mc fetch >>= complete a b
    CompletionScript f => putStrLn (completionScript f)
    Query m s          => env mc fetch >>= query m s
    Fuzzy m s          => idrisEnv mc fetch >>= fuzzy m s
    UpdateDB           => updateDB
    Run (Right p) args => idrisEnv mc fetch >>= execApp p args
    Run (Left p)  args => idrisEnv mc fetch >>= runIpkg p args
    Exec p args        => idrisEnv mc fetch >>= exec p args
    Repl p             => idrisEnv mc fetch >>= idrisRepl p
    Build p            => idrisEnv mc fetch >>= build p
    BuildDeps p        => idrisEnv mc fetch >>= buildDeps p
    Typecheck p        => idrisEnv mc fetch >>= typecheck p
    Clean p            => idrisEnv mc fetch >>= clean p
    PrintHelp          => putStrLn usageInfo
    Install ps         => idrisEnv mc fetch >>= \e => install ps
    Remove ps          => idrisEnv mc fetch >>= \e => remove ps
    Update             => idrisEnv mc fetch >>= update
    Fetch              => idrisEnv mc fetch >>= \e => install []
    PackagePath        => env mc fetch >>= packagePathDirs >>= putStrLn
    LibsPath           => env mc fetch >>= packageLibDirs  >>= putStrLn
    DataPath           => env mc fetch >>= packageDataDirs >>= putStrLn
    AppPath n          => env mc fetch >>= appPath n
    Info               => env mc fetch >>= printInfo
    New dir pty p      => idrisEnv mc fetch >>= new dir pty p
    Switch db          => do
      env <- idrisEnv mc fetch
      install []
      writeCollection
