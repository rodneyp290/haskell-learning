module TicTacToe where

import Data.Maybe
import Data.Bool

data Index = A | B | C deriving (Show)

data Token = E | X | O deriving (Show,Eq)

data Row = Row Token Token Token deriving (Show)

emptyRow :: Row
emptyRow = Row E E E

threeInARow :: Row -> Maybe Token
threeInARow (Row a b c)
    | (a == b) && (b == c) && (a /= E) = Just a
    | otherwise = Nothing

data Board =
    TTT Row Row Row deriving (Show)

emptyBoard :: Board
emptyBoard = TTT emptyRow emptyRow emptyRow

transpose :: Board -> Board
transpose (TTT (Row a b c) (Row d e f) (Row g h i))
    =  (TTT (Row a d g) (Row b e h) (Row c f i))

boardToRowList :: Board -> [Row]
boardToRowList (TTT a b c) = a:b:c:[]

diagonalRows :: Board -> [Row]
diagonalRows (TTT (Row a _ c) (Row _ e _) (Row g _ i))
    = (Row a e i):(Row g e c):[]

possibleRows :: Board -> [Row]
possibleRows b = (boardToRowList b) ++ (boardToRowList (transpose b)) ++ (diagonalRows b)

boardFull :: Board -> Bool
boardFull (TTT (Row a b c) (Row d e f) (Row g h i))
    |   (a /= E) && (b /= E) && (c /= E)
     && (d /= E) && (e /= E) && (f /= E)
     && (g /= E) && (h /= E) && (i /= E)
                = True
    | otherwise = False

data BoardStatus = Won Token | Draw | InPlay | Invalid

status :: Board -> BoardStatus
status b = winPlayersToStatus winPlayers
  where
    winPlayers :: [Token]
    winPlayers = catMaybes (map threeInARow (possibleRows b))

    winPlayersToStatus :: [Token] -> BoardStatus
    winPlayersToStatus [] = bool (InPlay) (Draw) (boardFull b)
    winPlayersToStatus [a] = Won a
    winPlayersToStatus _ = Invalid

data GameTree = Node Board [GameTree] | Leaf

--gameTreeToList :: GameTree -> [[Board]]
--gameTreeToList Leaf = [[]]
--gameTreeToList (Node b gts) = [b]:gts:(map getBoardLists gts)
    --where
        --getBoardLists :: GameTree -> [Board]
        --getBoardLists Leaf = []
        --getBoardLists (Node _ gts) = gts:(map getBoardLists gts)

        --getHeadBoard =

--instance Show (GameTree) where
    --show = (show).(take 20).(gameTreeToList)

data Move = Move Index Index Token deriving (Show)

maybeBoard :: Maybe Row -> Maybe Row -> Maybe Row -> Maybe Board
maybeBoard (Just a) (Just b) (Just c) = Just (TTT a b c)
maybeBoard _ _ _ = Nothing

makeMove :: Board -> Move -> Maybe Board
makeMove (TTT a b c) (Move h A t) = maybeBoard (placeInRow a h t) (Just b) (Just c)
makeMove (TTT a b c) (Move h B t) = maybeBoard (Just a) (placeInRow b h t) (Just c)
makeMove (TTT a b c) (Move h C t) = maybeBoard (Just a) (Just b) (placeInRow c h t)

placeInRow :: Row -> Index -> Token -> Maybe Row
placeInRow (Row E b c) A t = Just (Row t b c)
placeInRow (Row a E c) B t = Just (Row a t c)
placeInRow (Row a b E) C t = Just (Row a b t)
placeInRow _ _ _ = Nothing

bruteMoves :: Token -> [Move]
bruteMoves t = [Move a b t | a <- [A,B,C],
                             b <- [A,B,C]]

getNextBoards :: Token -> Board -> [Board]
getNextBoards t b = catMaybes (map (makeMove b) (bruteMoves t))


gameTree :: GameTree
gameTree = buildTree getNextBoards X emptyBoard
    where
        buildTree :: (Token -> Board -> [Board]) -> Token -> Board -> GameTree
        buildTree f t b = Node emptyBoard (map (buildTree getNextBoards (swapToken t)) (getNextBoards t emptyBoard))

        swapToken :: Token -> Token
        swapToken X = O
        swapToken _ = X
