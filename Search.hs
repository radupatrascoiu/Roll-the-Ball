{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.Set as S
-- import qualified Data.List as L
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = StateNode { getNodeState :: s,
                            getPastAction :: Maybe a,
                            getParentNode :: Maybe (Node s a),
                            getDepth :: Int,
                            getChildren :: [Node s a] } deriving (Eq, Show)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = getNodeState node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = getParentNode node

nodeDepth :: Node s a -> Int
nodeDepth node = getDepth node

nodeAction :: Node s a -> Maybe a
nodeAction node = getPastAction node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = getChildren node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

-- helper_StateSpace :: (ProblemState s a) => (s, a) -> Node s a -> Int -> Node s a
-- helper_StateSpace (state, action) parent depth = create_recursive state action parent (depth + 1)

-- create_recursive :: (ProblemState s a) => s -> a -> Node s a -> Int -> Node s a
-- create_recursive state action parent depth = new_node
--     where   
--         new_node = StateNode state (Just action) (Just parent) depth (map f (successors state))
--         f (state_param, action_param) = create_recursive action_param state_param new_node (depth + 1)


-- createSSH :: (ProblemState s a) => s -> Node s a -> Node s a
-- createSSH lv father = new_node
--                         where
--                         succs = successors lv -- ((Pos, dir), Lv)
--                         me = createSSH lv father
--                         children = [(createSSH lvC me) |   (acC ,lvC) <- succs ]
--                         new_node = StateNode lv Nothing (Just father) 0 children

-- createStateSpace :: (ProblemState s a) => s -> Node s a
-- createStateSpace state = new_node 
--     where 
--         new_node = StateNode state Nothing Nothing 0 (map f (successors state))  
--         f (state_param, action_param) = create_recursive action_param state_param new_node 1

createSSH :: (ProblemState s a) => s -> Node s a -> Node s a
createSSH lv father = StateNode lv Nothing (Just father) 0 [(createSSH lvC (createSSH lv father)) | (acC, lvC) <- (successors lv)]

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace state = createSSH state start_node
                        where 
                        start_node = StateNode state Nothing Nothing 0 []  


{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}


bfs_helper :: Ord s => [([Node s a], [Node s a])] -> S.Set s -> [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfs_helper result _ [] _ = result
bfs_helper result visited queue last_added = bfs_helper nres (S.union visited (S.fromList (map getNodeState validC)))  ((tail queue) ++ validC) validC
                                            where
                                            nres = result ++ [(last_added, queue)]    
                                            queue_head = head queue
                                            validC = filter (\nod -> not ( S.member (getNodeState nod) visited)) (getChildren queue_head)

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = bfs_helper [] S.empty [node] [node]




{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
