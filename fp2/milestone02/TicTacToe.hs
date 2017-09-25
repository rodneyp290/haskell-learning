module TicTacToe where

import Data.Maybe

data Index = A | B | C deriving (Show)

data Token = E | X | O deriving (Show)

data Row = Row Token Token Token deriving (Show)

type MRow = Maybe Row

emptyRow :: Row 
emptyRow = Row E E E

data Board = 
    TTT Row Row Row deriving (Show)

type MBoard = Maybe Board

emptyBoard :: Board 
emptyBoard = TTT emptyRow emptyRow emptyRow

transpose :: Board -> Board
transpose (TTT (Row a b c) (Row d e f) (Row g h i))
    =  (TTT (Row a d g) (Row b e h) (Row c f i))

data GameTree = Node Board [GameTree] | Leaf

--gameTreeToList :: GameTree -> [[Board]]
--gameTreeToList Leaf = [[]]
--gameTreeToList (Node b gts) = [b]:gts:(map getBoardLists gts)
    --where
        --getBoardLists :: GameTree -> [Board]
        --getBoardLists Leaf = []
        --getBoardLists (Node _ gts) = gts:(map getBoardLists gts)

        --getHeadBoard = 

instance Show (GameTree) where
    show = (show).(take 20).(gameTreeToList)

data Move = Move Index Index Token deriving (Show)

maybeBoard :: MRow -> MRow -> MRow -> MBoard
maybeBoard (Just a) (Just b) (Just c) = Just (TTT a b c)
maybeBoard _ _ _ = Nothing

makeMove :: Board -> Move -> MBoard
makeMove (TTT a b c) (Move h A t) = maybeBoard (placeInRow a h t) (Just b) (Just c)
makeMove (TTT a b c) (Move h B t) = maybeBoard (Just a) (placeInRow b h t) (Just c)
makeMove (TTT a b c) (Move h C t) = maybeBoard (Just a) (Just b) (placeInRow c h t)

placeInRow :: Row -> Index -> Token -> MRow
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
