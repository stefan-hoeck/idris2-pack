module Pack.Config.Environment.Variable

import public Data.FilePath.File
import Pack.Config.Types

--------------------------------------------------------------------------------
---         Types
--------------------------------------------------------------------------------

public export %inline
DirList : Type
DirList = List (Path Abs)

export
data EnvVar : Type where
  MkEnvVar : (name : String) -> Interpolation a => (value : a) -> EnvVar

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
public export %inline
IdrisCodegenVar : Codegen -> EnvVar
IdrisCodegenVar = MkEnvVar "IDRIS2_CG"

||| Path to the Scheme executable used by the Scheme backend
public export %inline
SchemeVar : FilePath -> EnvVar
SchemeVar = MkEnvVar "SCHEME"

||| Path to the Idris 2 boot file, used by Idris 2 Makefile
public export %inline
IdrisBootVar : File Abs -> EnvVar
IdrisBootVar = MkEnvVar "IDRIS2_BOOT"

||| Installation prefix for Idris 2
public export %inline
PrefixVar : Path Abs -> EnvVar
PrefixVar = MkEnvVar "PREFIX"

||| Alternative way to set the Idris 2 installation prefix
public export %inline
IdrisPrefixVar : Path Abs -> EnvVar
IdrisPrefixVar = MkEnvVar "IDRIS2_PREFIX"

||| Directories where Idris 2 searches for package definitions,
||| in addition to the default locations
public export %inline
IdrisPackagePathVar : DirList -> EnvVar
IdrisPackagePathVar = MkEnvVar "IDRIS2_PACKAGE_PATH"

||| Directories where Idris 2 looks for data files, typically
||| support code for code generators
public export %inline
IdrisDataVar : DirList -> EnvVar
IdrisDataVar = MkEnvVar "IDRIS2_DATA"

||| Directories where Idris 2 searches for libraries used by code generators
public export %inline
IdrisLibsVar : DirList -> EnvVar
IdrisLibsVar = MkEnvVar "IDRIS2_LIBS"
