module Pack.Core.Logging

import Pack.Core.Types

%default total

--------------------------------------------------------------------------------
--          Logging
--------------------------------------------------------------------------------

printLogMessage :  HasIO io
                => (lvl : LogLevel)
                -> (msg : String)
                -> (msgs : List String)
                -> io ()
printLogMessage lvl msg msgs = do
  let prefx = "[ \{lvl} ] "
  let baseIndent = replicate (length prefx) ' '
  printMultilineIndented prefx baseIndent msg
  for_ msgs $ printMultilineIndented "\{baseIndent}- " "\{baseIndent}  "

  where
    printMultilineIndented :  (fstPrefix, restPrefix : String)
                           -> (msg : String)
                           -> io ()
    printMultilineIndented fstPrefix restPrefix msg = do
      let (s::ss) = lines msg
        | [] => putStrLn "\{fstPrefix}"
      putStrLn "\{fstPrefix}\{s}"
      for_ ss $ \s => putStrLn "\{restPrefix}\{s}"

||| Logs a message to stdout if the log level is greater than or equal
||| than the reference level `ref`.
||| If the given string contains newlines, all lines are printed indented
||| to the beginning of the first one.
export
log :  HasIO io
    => (ref : LogLevel)
    -> (lvl : LogLevel)
    -> (msg : Lazy String)
    -> io ()
log ref lvl msg =
  when (lvl >= ref) $ printLogMessage lvl msg []

||| Logs an idented list of values to stdout if the given log level
||| is greater than or equal than the (auto-implicit) reference level `ref`.
||| If messages list is empty, no log message is printed.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export
logMany :  HasIO io
        => (ref  : LogLevel)
        => (lvl  : LogLevel)
        -> (msg  : Lazy String)
        -> (msgs : Lazy (List String))
        -> io ()
logMany lvl msg msgs =
  when (lvl >= ref && not (null msgs)) $ printLogMessage lvl msg msgs

||| Alias for `log ref Debug`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
debug : HasIO io => (ref : LogLevel) => (msg  : Lazy String) -> io ()
debug = log ref Debug

||| Alias for `log ref Info`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
info : HasIO io => (ref : LogLevel) => (msg  : Lazy String) -> io ()
info = log ref Info

||| Alias for `log ref Warning`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
warn : HasIO io => (ref : LogLevel) => (msg  : Lazy String) -> io ()
warn = log ref Warning
