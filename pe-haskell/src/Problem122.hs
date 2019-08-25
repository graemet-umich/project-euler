-- Efficient exponentiation

{-
This problem is a "shortest addition chain" variety. See OEIS A003313.

The algorithmic approach is a breadth first seach (BFS) based on the
one described in CLRS (Introduction to Algorithms, 3e). Instead of
passing a pre-generated graph G and source vertex s to bfs, a graph
with s=1 is generated in bfs. The implementation of a queue uses a
Data.Sequence with just queue-like operations.

In the CLRS BFS, an enqueued vertex is colored gray (not necessary,
see CLRS exercise 22.2-3), and a visited vertex is colored black,
never to be visited again.

Here no colors are used. It is important to enqueue not only a vertex
v, but also a list of the ancestors of v, i.e. the addition chain that
results in v (also called a path). Because there are multiple paths to
get v, the queue elements are (v, path) pairs, and a vertex v can be
enqueued multiple times. For example, e1=(9,[6,3,2,1]) and
e2=(9,[8,4,2,1]) are enqueued. If e3=(9,[5,4,3,2,1]) or
e4=(9,[7,5,3,2,1]) were visited before e1 and e2, they would also be
enqueued.

Vertex v can be enqueued multiple times, because different paths to v
can result in different shortest paths to children of v. For example,
the shortest path to 17 requires [9,8,4,2,1], not [9,6,3,2,1].

The shortest path to vertex v is stored in an array (numAncestors or
pathLens).

The CLRS BFS attributes v.d (distance/pathLength from s to v), v.pi
(parent of v), and G.Adj(u) (vertices adjacent to vertex u) are v.d =
pathLen, v.pi = head path (path is a list of all ancestors), and
G.Adj(u) = vs, the next elements of the addition chain. For example,
if u=5, then vs(5,[3,2,1]) = [10,8,7,6] and vs(5,[4,2,1]) = [10,9,7,6].
-}

module Problem122
  ( problem122
  , m
  ) where

import Data.Array ((!), (//), Array, array, elems)
-- import queue-like operations
import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as Seq

problem122 :: IO ()
problem122 = print $ sum_ms 200

type Vertex = Int

m :: Vertex -> Int
m = last . elems . bfs

sum_ms :: Vertex -> Int
sum_ms = sum . elems . bfs

bfs :: Vertex -> Array Vertex Int
bfs limit = numAncestors' where
  numAncestors = array (1, limit) [ (i, 0) | i <- [1..limit] ]
  -- enqueue s = (1, [])
  (_, numAncestors') = go (Seq.singleton (1, [])) numAncestors

  go :: Seq (Vertex, [Vertex]) -> Array Vertex Int ->
        (Seq (Vertex, [Vertex]), Array Vertex Int)
  go queue pathLens
    | queue == Seq.empty = (queue, pathLens)
    | otherwise = go q' pathLens'
    where
      -- dequeue u and path, ancestors of u
      ((u, path) :<| q) = queue
      -- the updated path for vs
      newPath = u : path
      -- the distance from s to u (addition chain length)
      npLen = length newPath
      -- find vs = G.Adj(u)
      vs = map (+ u) newPath
      -- visit vs
      (q', pathLens') = foldl f (q, pathLens) vs where
        f :: (Seq (Vertex, [Vertex]), Array Vertex Int) -> Vertex ->
             (Seq (Vertex, [Vertex]), Array Vertex Int)
        f acc@(fq, fpathLens) v
          -- do not visit chain lengths greater than limit
          | v > limit     = acc
          -- first visit to v ... enqueue & save result in array
          | fpathLens!v == 0 = (q', pathLens')
          -- enqueue & replace the path length of v with this shorter
          -- path length
          | fpathLens!v > npLen = (q', pathLens')
          -- enqueue this v & path that has the same length as the
          -- recorded chain length
          | fpathLens!v == npLen = (q', fpathLens)
          | otherwise     = acc
          where
            q' = fq |> (v, newPath)
            pathLens' = fpathLens // [(v, npLen)]
