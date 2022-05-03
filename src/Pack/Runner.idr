module Pack.Runner

import Pack.CmdLn.Completion
import Pack.CmdLn.Env
import Pack.CmdLn.Opts
import Pack.CmdLn.Types
import public Pack.Core
import Pack.Runner.Check
import Pack.Runner.Database
import Pack.Runner.Install

export covering
runCmd : HasIO io => EitherT PackErr io ()
runCmd = do
  c <- getConfig
  case c.cmd of
    Completion a b     => env c >>= complete a b
    CompletionScript f => putStrLn (completionScript f)
    UpdateDB           => updateDB c
    Exec p args        => idrisEnv c >>= execApp p args
    Build p            => idrisEnv c >>= build p
    Typecheck p        => idrisEnv c >>= typecheck p
    CheckDB _          => idrisEnv c >>= checkDB
    PrintHelp          => putStrLn usageInfo
    Install ps         => idrisEnv c >>= \e => traverse_ (installLib False e) ps
    Remove ps          => idrisEnv c >>= \e => traverse_ (remove e) ps
    InstallWithSrc ps  => idrisEnv c >>= \e => traverse_ (installLib True e) ps
    InstallApp ps      => idrisEnv c >>= \e => traverse_ (installApp e) ps
    SwitchRepo _       => idrisEnv c >>= switchCollection
    FromHEAD p         => env c >>= writeLatestDB p
