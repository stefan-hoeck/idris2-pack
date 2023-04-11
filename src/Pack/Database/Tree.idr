module Pack.Database.Tree

import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Nat
import Data.SortedMap
import Idris.Package.Types
import Pack.Core.Types
import Pack.Database.Types

%default covering

--------------------------------------------------------------------------------
--         Graphs
--------------------------------------------------------------------------------

||| A dependency graph
public export
0 Graph : Type -> Type
Graph a = SortedMap a (List a)

||| Returns the graph with all edges inverted
export
invertGraph : Ord a => Graph a -> Graph a
invertGraph = foldl add empty . SortedMap.toList
  where
    ins : a -> Graph a -> a -> Graph a
    ins v g k = case lookup k g of
      Nothing => insert k [v] g
      Just vs => insert k (v::vs) g

    add : Graph a -> (a,List a) -> Graph a
    add g (k,vs) = foldl (ins k) g vs

export
dependencyGraph : SortedMap PkgName (ResolvedLib t) -> Graph PkgName
dependencyGraph = map dependencies

--------------------------------------------------------------------------------
--         Trees
--------------------------------------------------------------------------------

public export
record Tree a where
  constructor T
  label : a
  deps  : List (Tree a)

export
Functor Tree where
  map f (T l ds) = T (f l) (map f <$> ds)

export
filter : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter f (T l ds) =
   if f l then Just $ T l (mapMaybe (filter f) ds) else Nothing

public export
0 TreeMap : Type -> Type
TreeMap a = SortedMap a (Tree a)

0 TreeST : Type -> Type -> Type
TreeST a t = State (TreeMap a) t

tree : Ord a => Graph a -> a -> TreeST a (Tree a)
tree g k = do
  Nothing <- lookup k <$> get | Just t => pure t
  ts      <- traverse (tree g) (maybe [] id $ lookup k g)
  modify (insert k $ T k ts)
  pure $ T k ts

export
treeMap : Ord a => Graph a -> TreeMap a
treeMap g = execState empty $ traverse_ (ignore . tree g) (keys g)

export %inline
childMap : SortedMap PkgName (ResolvedLib t) -> TreeMap PkgName
childMap = treeMap . dependencyGraph

export %inline
parentMap : SortedMap PkgName (ResolvedLib t) -> TreeMap PkgName
parentMap = treeMap . invertGraph . dependencyGraph

export
treeLookup : Ord a => a -> TreeMap a -> Tree a
treeLookup v = fromMaybe (T v []) . lookup v

--------------------------------------------------------------------------------
--          Pretty-printing Tree
--------------------------------------------------------------------------------

merge1 :
     SnocList (String,String)
  -> Nat
  -> List String
  -> List String
  -> (Nat,List String)
merge1 sp n (x::xs) (y::ys) = merge1 (sp :< (x,y)) (max n (length x)) xs ys
merge1 sp n xs      _       =
  (S n, map (\(x,y) => padRight (S n) ' ' x ++ y) sp <>> xs)

header : Nat -> String
header m = "|" ++ replicate (pred m) '_'

merge :
     Nat
  -> List String
  -> List (List String)
  -> (List String, Nat)
merge m ss []      = (ss,m)
merge m ss (x::xs) = let (n,ss2) := merge1 [<] 0 ss x in merge n ss2 xs

treeLines : Interpolation a => Tree a -> (Nat, List String)
treeLines (T l ds) =
  let s       := interpolate l
      lls     := map (("|" ::) . snd)
               . reverse
               . sortBy (comparing fst)
               $ map treeLines ds

      (h::t)  := lls | [] => (1, [s])
      (ls,os) := merge 0 h t
      in (length ls + 2, s :: header os :: ls)

export %inline
prettyTree : Interpolation a => Tree a -> String
prettyTree = unlines . snd . treeLines
