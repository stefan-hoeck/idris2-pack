module Pack.Runner

import Pack.CmdLn.Completion
import Pack.CmdLn.Opts
import Pack.CmdLn.Types
import Pack.Config.Env
import Pack.Config.Types
import public Pack.Core
import Pack.Runner.Database
import Pack.Runner.Query
import Pack.Runner.Install

export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  (c,cmd) <- getConfig cmd PrintHelp
  case cmd of
    Completion a b     => env c >>= complete a b
    CompletionScript f => putStrLn (completionScript f)
    Query s            => env c >>= query s
    UpdateDB           => updateDB c
    Exec p args        => idrisEnv c >>= execApp p args
    Repl p             => idrisEnv c >>= repl p
    Build p            => idrisEnv c >>= build p
    Typecheck p        => idrisEnv c >>= typecheck p
    PrintHelp          => putStrLn usageInfo
    Install ps         => idrisEnv c >>= \e => traverse_ (installLib e) ps
    Remove ps          => idrisEnv c >>= \e => traverse_ (remove e) ps
    InstallApp ps      => idrisEnv c >>= \e => traverse_ (installApp e) ps
