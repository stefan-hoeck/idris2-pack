module Pack.Config.Environment.Variable

import public Data.FilePath.File
import Pack.Config.Types

--------------------------------------------------------------------------------
---         Types
--------------------------------------------------------------------------------

public export %inline
0 DirList : Type
DirList = List (Path Abs)

export
data EnvVar : Type where
  MkEnvVar : (name : String) -> (value : String) -> EnvVar

%inline
envVar : Interpolation a => (name : String) -> (val : a) -> EnvVar
envVar name = MkEnvVar name . interpolate

--------------------------------------------------------------------------------
---         Interpolation
--------------------------------------------------------------------------------

||| Interpolate a list of directories as a colon-separated string
export
Interpolation DirList where
  interpolate = fastConcat . intersperse ":" . map interpolate

||| Interpolate an environment variable for use in a shell
export
Interpolation EnvVar where
  interpolate (MkEnvVar var val) = "\{var}=\{val}"

--------------------------------------------------------------------------------
--          Environment Variables
--------------------------------------------------------------------------------

-- The official documentation for Idris 2 compiler environment variables:
-- https://github.com/idris-lang/Idris2/blob/main/docs/source/reference/envvars.rst

||| Code generation backend used by the Idris 2 compiler
export %inline
IdrisCodegenVar : Codegen -> EnvVar
IdrisCodegenVar = envVar "IDRIS2_CG"

||| Path to the Scheme executable used by the Scheme backend
export %inline
SchemeVar : FilePath -> EnvVar
SchemeVar = envVar "SCHEME"

||| Path to the Idris 2 boot file, used by Idris 2 Makefile
export %inline
IdrisBootVar : File Abs -> EnvVar
IdrisBootVar = envVar "IDRIS2_BOOT"

||| Installation prefix for Idris 2
export %inline
PrefixVar : Path Abs -> EnvVar
PrefixVar = envVar "PREFIX"

||| Alternative way to set the Idris 2 installation prefix
export %inline
IdrisPrefixVar : Path Abs -> EnvVar
IdrisPrefixVar = envVar "IDRIS2_PREFIX"

||| Directories where Idris 2 searches for package definitions,
||| in addition to the default locations
export %inline
IdrisPackagePathVar : DirList -> EnvVar
IdrisPackagePathVar = envVar "IDRIS2_PACKAGE_PATH"

||| Directories where Idris 2 looks for data files, typically
||| support code for code generators
export %inline
IdrisDataVar : DirList -> EnvVar
IdrisDataVar = envVar "IDRIS2_DATA"

||| Directories where Idris 2 searches for libraries used by code generators
export %inline
IdrisLibsVar : DirList -> EnvVar
IdrisLibsVar = envVar "IDRIS2_LIBS"
