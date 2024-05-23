module Pack.Core.Git.Consts

import Pack.Core.Types

%default total

||| URL of package collections repository
export
dbRepo : URL
dbRepo = "https://github.com/stefan-hoeck/idris2-pack-db"

||| Default URL of pack repository
export
defaultPackRepo : URL
defaultPackRepo = "https://github.com/stefan-hoeck/idris2-pack"

||| Default branch of the pack repository
export
defaultPackBranch : Branch
defaultPackBranch = "main"

||| Package name we use for temp dirs involving the idris compiler
||| and its core libraries.
export
compiler : PkgName
compiler = "idris2-compiler"

||| Package name we use for temp dirs involving pack-db project.
export
packDB : PkgName
packDB = "pack-db"
