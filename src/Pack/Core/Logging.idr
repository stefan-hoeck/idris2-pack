module Pack.Core.Logging

import Pack.Core.Types

import System

%default total

--------------------------------------------------------------------------------
--          Logging
--------------------------------------------------------------------------------

printLogMessage :
     {auto _ : HasIO io}
  -> {auto _ : Interpolation logLevel}
  -> (lvl : logLevel)
  -> (msg : String)
  -> (msgs : List String)
  -> io ()
printLogMessage lvl msg msgs = do
  let prefx := "[ \{lvl} ] "
  let baseIndent := replicate (length prefx) ' '
  printMultilineIndented prefx baseIndent msg
  for_ msgs $ printMultilineIndented "\{baseIndent}- " "\{baseIndent}  "

  where
    printMultilineIndented :
         (fstPrefix, restPrefix : String)
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
log :
     {auto _ : HasIO io}
  -> (ref : LogRef)
  -> (lvl : LogLevel)
  -> (msg : Lazy String)
  -> io ()
log ref lvl msg =
  when (lvl >= ref.level) $ printLogMessage lvl msg []

||| Logs a message to stdout and reads an reply from stdin.
||| This uses the given log level but makes sure the message is always
||| printed no matter the current log level preferences.
export %inline
prompt : HasIO io => (lvl : LogLevel) -> (msg : String) -> io String
prompt lvl msg = log (MkLogRef lvl) lvl msg >> map trim getLine

||| Logs an idented list of values to stdout if the given log level
||| is greater than or equal than the (auto-implicit) reference level `ref`.
||| If messages list is empty, no log message is printed.
|||
||| `inlineSingle` parameter being set to `True` makes a single element of
||| the given list to be printed at the same line as the main message.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export
logMany :
     {auto _ : HasIO io}
  -> {auto ref  : LogRef}
  -> {default False inlineSingle : Bool}
  -> (lvl  : LogLevel)
  -> (msg  : Lazy String)
  -> (msgs : Lazy (List String))
  -> io ()
logMany lvl msg msgs =
  when (lvl >= ref.level && not (null msgs)) $
    case (inlineSingle, force msgs) of
      (True, [x] ) => printLogMessage lvl "\{msg} \{x}" []
      (_   , msgs) => printLogMessage lvl msg msgs

||| Logs an indented list of values to stdout and reads a reply from stdin.
|||
||| This uses the given log level but makes sure the message is always
||| printed no matter the current log level preferences.
export %inline
promptMany :
     {auto _ : HasIO io}
  -> (lvl : LogLevel)
  -> (msg : String)
  -> (msgs : List String)
  -> io String
promptMany lvl msg msgs =
  let ref := MkLogRef lvl
   in logMany lvl msg msgs >> map trim getLine

||| Alias for `log ref Debug`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
debug : HasIO io => (ref : LogRef) => (msg  : Lazy String) -> io ()
debug = log ref Debug

||| Alias for `log ref Info`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
info : HasIO io => (ref : LogRef) => (msg  : Lazy String) -> io ()
info = log ref Info

||| Alias for `log ref Cache`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
cache : HasIO io => (ref : LogRef) => (msg  : Lazy String) -> io ()
cache = log ref Cache

||| Alias for `log ref Warning`.
|||
||| Note: Most of the time `ref` is automatically being extracted from
||| a value of type `Pack.Config.Types.Config` in scope.
export %inline
warn : HasIO io => (ref : LogRef) => (msg  : Lazy String) -> io ()
warn = log ref Warning

||| Fail fatally with error message logged.
||| This is like `System.die` but with error printer like a log message.
export
fatal : HasIO io => (err : PackErr) -> io a
fatal err = do
  printLogMessage "fatal" (printErr err) []
  exitFailure
