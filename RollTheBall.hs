{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Debug.Trace

import qualified Data.Array as A
-- import Data.Maybe
-- import Data.List

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {content :: Char}
    deriving (Eq, Ord)

instance Show Cell 
    where show (Cell continut) = show $ continut

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level  {cells :: (A.Array Position Cell)} -- aveam Cell
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level 
    where show (Level level) = foldl f [] cellList ++ [endl]
            where
            cellList = A.assocs $ cells (Level level)
            f out cell
              | snd (fst cell) /= 0 = out ++ [content (snd cell)]
              | otherwise = out ++ [endl] ++ [content (snd cell)]
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel position = Level $ A.array ((0, 0), (x, y))
    [((i, j), Cell emptySpace) | i <- [0..x], j <- [0..y]]
    -- A.// [(position, EmptySpace)]
    where x = fst position
          y = snd position

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (typeC, position) level = 
    if (outOfBounds level (0, 0) == True && (content ((cells level) A.! position)) == emptySpace)
    then Level newCell
    else level
    where 
        newCell = (cells level) A.// [(position, (Cell typeC))]
        outOfBounds lvl offset
            | (x + (fst offset)) < 0 || (x + (fst offset)) > (fst (snd (A.bounds (cells lvl))))  = False
            | (y + (snd offset)) < 0 || (y + (snd offset)) > (snd (snd (A.bounds (cells lvl))))  = False
            | otherwise = True
        x = fst position
        y = snd position 
{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel position l = Level ((cells (emptyLevel position)) A.// (foldr f [] l))
    where f (continut, pozitie) out = out ++ [(pozitie, Cell (continut))]

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell position direction level

  | (content ((cells level) A.! position)) == startUp = level

  | (content ((cells level) A.! position)) == startDown = level

  | (content ((cells level) A.! position)) == startRight = level

  | (content ((cells level) A.! position)) == startLeft = level

  | (content ((cells level) A.! position)) == winUp = level

  | (content ((cells level) A.! position)) == winDown = level

  | (content ((cells level) A.! position)) == winRight = level

  | (content ((cells level) A.! position)) == winLeft = level

  | direction == North && outOfBounds level (-1, 0) == True && isEmpty level (-1, 0) == True
    = Level ((cells level) A.// [((x - 1, y), (cells level) A.! position), (position, Cell emptySpace)])

  | direction == South && outOfBounds level (1, 0) == True && isEmpty level (1, 0) == True
    = Level ((cells level) A.// [((x + 1, y), (cells level) A.! position), (position, Cell emptySpace)])

  | direction == West && outOfBounds level (0, -1) == True && isEmpty level (0, -1) == True
    = Level ((cells level) A.// [((x, y - 1), (cells level) A.! position), (position, Cell emptySpace)])

  | direction == East && outOfBounds level (0, 1) == True && isEmpty level (0, 1) == True
    = Level ((cells level) A.// [((x, y + 1), (cells level) A.! position), (position, Cell emptySpace)])

  | otherwise = level

    where 
        isEmpty lvl offset =
             if((content ((cells lvl) A.! ((x + (fst offset)), (y + (snd offset))))) == emptySpace)
                 then True
                  else False
        outOfBounds lvl offset
            | (x + (fst offset)) < 0 || (x + (fst offset)) > (fst (snd (A.bounds (cells lvl))))  = False
            | (y + (snd offset)) < 0 || (y + (snd offset)) > (snd (snd (A.bounds (cells lvl))))  = False
            | otherwise = True
            
        x = fst position
        y = snd position 
 
{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 dir
    | content cell1 == horPipe && content cell2 == horPipe && dir == West = True
    | content cell1 == horPipe && content cell2 == horPipe && dir == East = True
    | content cell1 == horPipe && content cell2 == topLeft && dir == West = True
    | content cell1 == horPipe && content cell2 == botLeft && dir == West = True
    | content cell1 == horPipe && content cell2 == botRight && dir == East = True
    | content cell1 == horPipe && content cell2 == topRight && dir == East = True
    | content cell1 == horPipe && content cell2 == startLeft && dir == East = True
    | content cell1 == horPipe && content cell2 == startRight && dir == West = True
    | content cell1 == horPipe && content cell2 == winLeft && dir == East = True
    | content cell1 == horPipe && content cell2 == winRight && dir == West = True

    | content cell1 == verPipe && content cell2 == verPipe && dir == North = True
    | content cell1 == verPipe && content cell2 == verPipe && dir == South = True
    | content cell1 == verPipe && content cell2 == topLeft && dir == North = True
    | content cell1 == verPipe && content cell2 == botLeft && dir == South = True
    | content cell1 == verPipe && content cell2 == botRight && dir == South = True
    | content cell1 == verPipe && content cell2 == topRight && dir == North = True
    | content cell1 == verPipe && content cell2 == startUp && dir == South = True
    | content cell1 == verPipe && content cell2 == winUp && dir == South = True

    | content cell1 == topLeft && content cell2 == horPipe && dir == East = True
    | content cell1 == topLeft && content cell2 == verPipe && dir == South = True
    | content cell1 == topLeft && content cell2 == botLeft && dir == South = True
    | content cell1 == topLeft && content cell2 == botRight && dir == East = True
    | content cell1 == topLeft && content cell2 == topRight && dir == East = True
    | content cell1 == topLeft && content cell2 == startUp && dir == South = True
    | content cell1 == topLeft && content cell2 == startLeft && dir == East = True
    | content cell1 == topLeft && content cell2 == winUp && dir == South = True
    | content cell1 == topLeft && content cell2 == winLeft && dir == East = True


    | content cell1 == botLeft && content cell2 == horPipe && dir == East = True
    | content cell1 == botLeft && content cell2 == verPipe && dir == North = True
    | content cell1 == botLeft && content cell2 == topLeft && dir == North = True
    | content cell1 == botLeft && content cell2 == botRight && dir == East = True
    | content cell1 == botLeft && content cell2 == topRight && dir == East = True
    | content cell1 == botLeft && content cell2 == startDown && dir == North = True
    | content cell1 == botLeft && content cell2 == startLeft && dir == East = True
    | content cell1 == botLeft && content cell2 == winDown && dir == North = True
    | content cell1 == botLeft && content cell2 == winLeft && dir == East = True

    | content cell1 == botRight && content cell2 == horPipe && dir == West = True
    | content cell1 == botRight && content cell2 == verPipe && dir == North = True
    | content cell1 == botRight && content cell2 == topLeft && dir == West = True
    | content cell1 == botRight && content cell2 == botLeft && dir == West = True
    | content cell1 == botRight && content cell2 == topRight && dir == North = True
    | content cell1 == botRight && content cell2 == startDown && dir == North = True
    | content cell1 == botRight && content cell2 == startRight && dir == West = True
    | content cell1 == botRight && content cell2 == winDown && dir == North = True
    | content cell1 == botRight && content cell2 == winRight && dir == West = True

    | content cell1 == topRight && content cell2 == horPipe && dir == West = True
    | content cell1 == topRight && content cell2 == verPipe && dir == South = True
    | content cell1 == topRight && content cell2 == topLeft && dir == West = True
    | content cell1 == topRight && content cell2 == botLeft && dir == West = True
    | content cell1 == topRight && content cell2 == botRight && dir == South = True
    | content cell1 == topRight && content cell2 == startUp && dir == South = True
    | content cell1 == topRight && content cell2 == startRight && dir == West = True
    | content cell1 == topRight && content cell2 == winUp && dir == South = True
    | content cell1 == topRight && content cell2 == winRight && dir == West = True

    | content cell1 == startUp && content cell2 == verPipe && dir == North = True
    | content cell1 == startUp && content cell2 == topLeft && dir == North = True
    | content cell1 == startUp && content cell2 == topRight && dir == North = True
    | content cell1 == startUp && content cell2 == startDown && dir == North = True

    | content cell1 == startDown && content cell2 == verPipe && dir == South = True
    | content cell1 == startDown && content cell2 == botLeft && dir == South = True
    | content cell1 == startDown && content cell2 == botRight && dir == South = True
    | content cell1 == startDown && content cell2 == startUp && dir == South = True

    | content cell1 == startLeft && content cell2 == horPipe && dir == West = True
    | content cell1 == startLeft && content cell2 == topLeft && dir == West = True
    | content cell1 == startLeft && content cell2 == botLeft && dir == West = True
    | content cell1 == startLeft && content cell2 == winRight && dir == West = True

    | content cell1 == startRight && content cell2 == horPipe && dir == East = True
    | content cell1 == startRight && content cell2 == botRight && dir == East = True
    | content cell1 == startRight && content cell2 == topRight && dir == East = True
    | content cell1 == startRight && content cell2 == winLeft && dir == East = True

    | content cell1 == winUp && content cell2 == verPipe && dir == North = True
    | content cell1 == winUp && content cell2 == topLeft && dir == North = True
    | content cell1 == winUp && content cell2 == topRight && dir == North = True
    | content cell1 == winUp && content cell2 == startDown && dir == North = True

    | content cell1 == winDown && content cell2 == verPipe && dir == South = True
    | content cell1 == winDown && content cell2 == botLeft && dir == South = True
    | content cell1 == winDown && content cell2 == botRight && dir == South = True
    | content cell1 == winDown && content cell2 == startUp && dir == South = True

    | content cell1 == winLeft && content cell2 == horPipe && dir == West = True
    | content cell1 == winLeft && content cell2 == topLeft && dir == West = True
    | content cell1 == winLeft && content cell2 == botLeft && dir == West = True
    | content cell1 == winLeft && content cell2 == startRight && dir == West = True

    | content cell1 == winRight && content cell2 == horPipe && dir == East = True
    | content cell1 == winRight && content cell2 == botRight && dir == East = True
    | content cell1 == winRight && content cell2 == topRight && dir == East = True
    | content cell1 == winRight && content cell2 == startLeft && dir == East = True

    | otherwise = False


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

-- isInArray :: [Char] -> Char -> Bool
-- isInArray arr elem = (length [x | x <- arr, x == elem] /= 0)

-- returneaza pozitia celulei de start in lista de cells (TODO: return (pozitie, celula))
getStartPoz :: Level -> [Cell] -> Int -> (Position, Cell)
getStartPoz level arrayCell counter
    | content (head arrayCell) == startUp || content (head arrayCell) == startDown ||
    content (head arrayCell) == startLeft || content (head arrayCell) == startRight =
    ((counter `div` (nr_coloane + 1), counter `mod` (nr_coloane + 1)), head arrayCell)
    | otherwise = getStartPoz level (tail arrayCell) (counter + 1)
    where
        -- nr_linii = fst $ snd $ A.bounds (cells level)
        nr_coloane = snd $ snd $ A.bounds (cells level)

arr2 =  A.array ((0, 0), (1, 1)) [((0,0), (Cell emptySpace)), ((0,1), (Cell emptySpace)), ((1, 0), (Cell startUp)), ((1,1), (Cell emptySpace))]
arr3 =  A.array ((0, 0), (2, 2)) [((0, 0), (Cell emptySpace)), ((0, 1), (Cell startDown)), ((0, 2), (Cell emptySpace)),((1, 0), (Cell emptySpace)), ((1, 1), (Cell emptySpace)), ((1, 2), (Cell emptySpace)),((2, 0), (Cell emptySpace)), ((2, 1), (Cell emptySpace)), ((2, 2), (Cell emptySpace))]
arr4 = A.array ((0, 0), (3, 3)) [
        ((0, 0), (Cell startDown)), ((0, 1), (Cell emptyCell)), ((0, 2), (Cell emptyCell)), ((0, 3), (Cell emptyCell)), 
        ((1, 0), (Cell verPipe)), ((1, 1), (Cell emptyCell)), ((1, 2), (Cell emptyCell)), ((1, 3), (Cell emptyCell)), 
        ((2, 0), (Cell verPipe)), ((2, 1), (Cell emptySpace)), ((2, 2), (Cell emptyCell)), ((2, 3), (Cell emptyCell)), 
        ((3, 0), (Cell botLeft)), ((3, 1), (Cell horPipe)), ((3, 2), (Cell horPipe)), ((3, 3), (Cell winLeft))]

arr5 = A.array ((0, 0), (4, 4)) [
        ((0,0), Cell emptyCell), ((0,1), Cell emptyCell), ((0,2), Cell emptyCell), ((0,3), Cell horPipe), ((0,4), Cell botRight),
        ((1,0), Cell botRight), ((1,1), Cell emptySpace), ((1,2), Cell emptySpace), ((1,3), Cell emptySpace), ((1,4), Cell emptySpace),
        ((2,0), Cell topLeft), ((2,1), Cell topLeft), ((2,2), Cell horPipe), ((2,3), Cell horPipe), ((2,4), Cell topRight),
        ((3,0), Cell winDown), ((3,1), Cell verPipe), ((3,2), Cell verPipe), ((3,3), Cell topLeft), ((3,4), Cell botRight),
        ((4,0), Cell botLeft), ((4,1), Cell botRight), ((4,2), Cell emptySpace), ((4,3), Cell startUp), ((4,4), Cell verPipe)]

sol5 = A.array ((0, 0), (4, 4)) [
        ((0,0), Cell emptyCell), ((0,1), Cell emptyCell), ((0,2), Cell emptyCell), ((0,3), Cell horPipe), ((0,4), Cell botRight),
        ((1,0), Cell emptySpace), ((1,1), Cell botRight), ((1,2), Cell horPipe), ((1,3), Cell horPipe), ((1,4), Cell topRight),
        ((2,0), Cell topLeft), ((2,1), Cell emptySpace), ((2,2), Cell topLeft), ((2,3), Cell emptySpace), ((2,4), Cell botRight),
        ((3,0), Cell winDown), ((3,1), Cell emptySpace), ((3,2), Cell verPipe),((3,3), Cell topLeft), ((3,4), Cell emptySpace),
        ((4,0), Cell botLeft), ((4,1), Cell verPipe), ((4,2), Cell botRight), ((4,3), Cell startUp), ((4,4), Cell verPipe)]

-- [
--     (emptyCell, (0,0)), (emptyCell, (0,1)), (emptyCell, (0,2)), (horPipe, (0,3)), (botRight, (0,4)),
--     (botRight, (1,1)), (horPipe, (1,2)), (horPipe, (1,3)), (topRight, (1,4)),
--     (topLeft, (2,0)), (topLeft, (2,2)), (botRight, (2,4)),
--     (winDown, (3,0)), (verPipe, (3,2)), (topLeft, (3,3)),
--     (botLeft, (4,0)), (verPipe, (4,1)), (botRight, (4,2)), (startUp, (4,3)), (verPipe, (4,4))
--     ]


elems2 = A.assocs arr2
elems3 = A.assocs arr3
elems4 = A.assocs arr4

lv2 = Level arr2
lv3 = Level arr3
lv4 = Level arr4
lv5 = Level arr5
rez5 = Level sol5

helper_wonLevel :: Level -> Position -> Cell -> Directions -> Bool
helper_wonLevel level (x, y) crtCell pastDir
    | content crtCell == winUp || content crtCell == winDown || content crtCell == winLeft || content crtCell == winRight = True

    | pastDir == North && outOfBounds level (1, 0) == True && connection crtCell ((cells level) A.! (x + 1, y)) South == True = helper_wonLevel level (x + 1, y) ((cells level) A.! (x + 1, y)) North  
    | pastDir == North && outOfBounds level (0, -1) == True && connection crtCell ((cells level) A.! (x, y - 1)) West == True = helper_wonLevel level (x, y - 1) ((cells level) A.! (x, y - 1)) East
    | pastDir == North && outOfBounds level (0, 1) == True && connection crtCell ((cells level) A.! (x, y + 1)) East == True = helper_wonLevel level (x, y + 1) ((cells level) A.! (x, y + 1)) West

    | pastDir == South && outOfBounds level (-1, 0) == True && connection crtCell ((cells level) A.! (x - 1, y)) North == True = helper_wonLevel level (x - 1, y) ((cells level) A.! (x - 1, y)) South  
    | pastDir == South && outOfBounds level (0, -1) == True && connection crtCell ((cells level) A.! (x, y - 1)) West == True = helper_wonLevel level (x, y - 1) ((cells level) A.! (x, y - 1)) East
    | pastDir == South && outOfBounds level (0, 1) == True && connection crtCell ((cells level) A.! (x, y + 1)) East == True = helper_wonLevel level (x, y + 1) ((cells level) A.! (x, y + 1)) West
        
    | pastDir == West && outOfBounds level (-1, 0) == True && connection crtCell ((cells level) A.! (x - 1, y)) North == True = helper_wonLevel level (x - 1, y) ((cells level) A.! (x - 1, y)) South  
    | pastDir == West && outOfBounds level (1, 0) == True && connection crtCell ((cells level) A.! (x + 1, y)) South == True = helper_wonLevel level (x + 1, y) ((cells level) A.! (x + 1, y)) North
    | pastDir == West && outOfBounds level (0, 1) == True && connection crtCell ((cells level) A.! (x, y + 1)) East == True = helper_wonLevel level (x, y + 1) ((cells level) A.! (x, y + 1)) West        

    | pastDir == East && outOfBounds level (-1, 0) == True && connection crtCell ((cells level) A.! (x - 1, y)) North == True = helper_wonLevel level (x - 1, y) ((cells level) A.! (x - 1, y)) South  
    | pastDir == East && outOfBounds level (1, 0) == True && connection crtCell ((cells level) A.! (x + 1, y)) South == True = helper_wonLevel level (x + 1, y) ((cells level) A.! (x + 1, y)) North
    | pastDir == East && outOfBounds level (0, -1) == True && connection crtCell ((cells level) A.! (x, y - 1)) West == True = helper_wonLevel level (x, y - 1) ((cells level) A.! (x, y - 1)) East
       
    | otherwise = False

    where
        outOfBounds lvl offset
            | (x + (fst offset)) < 0 || (x + (fst offset)) > (fst (snd (A.bounds (cells lvl))))  = False
            | (y + (snd offset)) < 0 || (y + (snd offset)) > (snd (snd (A.bounds (cells lvl))))  = False
            | otherwise = True

wonLevel :: Level -> Bool
wonLevel level
    | typeCell == (Cell startUp) = helper_wonLevel level startPosition typeCell South
    | typeCell == (Cell startDown) = helper_wonLevel level startPosition typeCell North
    | typeCell == (Cell startLeft) = helper_wonLevel level startPosition typeCell East
    | typeCell == (Cell startRight) = helper_wonLevel level startPosition typeCell West
    | otherwise = undefined

    where
        (startPosition, typeCell) = getStartPoz level (A.elems (cells level)) 0

-- arrTest = A.array ((0,0), (1, 1)) [((0, 0), 0), ((0, 1), 1), ((1, 0), 2), ((1, 1), 3)]

my_filter_for_level :: Level -> ((Position, Directions), Level) -> Bool
my_filter_for_level level_initial (_, level_to_check) = (level_initial /= level_to_check)

get_opposite_dir :: Directions -> Directions
get_opposite_dir dir
                    | dir == North = South
                    | dir == South = North
                    | dir == West = East
                    | dir == East = West

get_past_pos :: Position -> Directions -> Position
get_past_pos pos@(x, y) dir
                                | dir == North = (x - 1, y)
                                | dir == South = (x + 1, y)
                                | dir == West = (x, y - 1)
                                | dir == East = (x, y + 1)

instance ProblemState Level (Position, Directions) where
    successors :: Level -> [((Position, Directions), Level)]
    successors level@(Level celule) = filter (my_filter_for_level level) posibil_succesori_toate_sensuri
                        where
                            lista_poz_cel = A.assocs celule
                            posibili_succesori_North = [((pos, North), (moveCell pos North level)) | (pos, _) <- lista_poz_cel]
                            posibili_succesori_South = [((pos, South), (moveCell pos South level)) | (pos, _) <- lista_poz_cel]
                            posibili_succesori_West = [((pos, West), (moveCell pos West level)) | (pos, _) <- lista_poz_cel]
                            posibili_succesori_East = [((pos, East), (moveCell pos East level)) | (pos, _) <- lista_poz_cel]
                            posibil_succesori_toate_sensuri = concat [posibili_succesori_North, posibili_succesori_South, posibili_succesori_West, posibili_succesori_East]

    isGoal :: Level -> Bool
    isGoal level = wonLevel level

    reverseAction :: ((Position, Directions), Level) -> ((Position, Directions), Level)
    reverseAction ((pos, dir), level) = (((get_past_pos pos dir), get_opposite_dir dir), moveCell pos dir level)