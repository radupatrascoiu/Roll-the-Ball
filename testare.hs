import qualified Data.Set as S

testing :: Int -> [Int]
testing 0 = [4]
testing 1 = [4]
testing 2 = [5]
testing 3 = [5]
testing 4 = [0, 1, 6]
testing 6 = [4, 5, 7]
testing 5 = [2, 3, 6]
testing 7 = [6, 8]
testing 8 = [7, 9, 10]
testing 9 = [8, 11, 12]
testing 11 = [9]
testing 12 = [9]
testing 10 = [8, 13, 14]
testing 13 = [10]
testing 14 = [10]
               
                    --lista noduri         start        vizitati      coada             lista rezultata
bfs_helper :: Ord Int => [Int] -> Int -> S.Set Int -> [Int] -> [([Int], [Int])]
bfs_helper _ Nothing _ _ = [([], [])]
bfs_helper nodes start visited queue
    | null unvisited = [(nodes, [])]
    | otherwise = bfs_helper updated_nodes start updated_visited updated_queue
        where
            unvisited = filter (\nod -> not $ S.member nod visited) (testing nodes)
            updated_nodes = nodes ++ [head unvisited]
            updated_visited = S.insert (head unvisited) visited
            updated_queue = queue ++ [head unvisited]
 
bfs :: Ord s => Int -> [([Int], [Int])]
bfs node = bfs_helper [] node S.empty []