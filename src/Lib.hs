module Lib
  ( someFunc
  ) where

import           Data.Array
import           Data.List

data GameTree a
  = TerminatedNode a
  | PlayingNode a
                (Maybe [GameTree a])
  deriving (Show)

data GameTreeCrumb a =
  GameTreeCrumb a
                [GameTree a]
                [GameTree a]
  deriving (Show)

type GameTreeZipper a = (GameTree a, [GameTreeCrumb a])

gameTreeUp :: GameTreeZipper a -> Maybe (GameTreeZipper a)
gameTreeUp (currentGameTree, []) = Nothing
gameTreeUp (currentGameTree, GameTreeCrumb d ls rs:bs) =
  Just (PlayingNode d (Just (ls ++ [currentGameTree] ++ rs)), bs)

getElementWithLeftAndRight :: [a] -> [(a, [a], [a])]
getElementWithLeftAndRight list =
  foldr
    (\currentElem currentResult ->
       case currentResult of
         [] -> [(currentElem, init list, [])]
         (last, lastLeft, lastRight):r ->
           (currentElem, init lastLeft, last : lastRight) : currentResult)
    []
    list

getChildren :: GameTreeZipper a -> [GameTreeZipper a]
getChildren (TerminatedNode _, _) = []
getChildren (PlayingNode d (Just children), treeCrumbs) =
  let withLeftAndRight = getElementWithLeftAndRight children
      convertToZipper (elem, left, right) =
        (elem, GameTreeCrumb d left right : treeCrumbs)
   in map convertToZipper withLeftAndRight

data Cell
  = NotPlayed
  | Self
  | Opponent
  deriving (Eq)

data Player
  = SelfPlayer
  | OpponentPlayer
  deriving (Eq, Show)

type NextPlayer = Player

type Board = (Array (Int, Int) Cell)

data GameState =
  GameState NextPlayer
            Board

data VisitedBoard = VisitedBoard
  { q     :: Int
  , n     :: Int
  , board :: GameState
  }

data GameNode
  = Unvisited GameState
  | Visited VisitedBoard

computeWinner :: Board -> Maybe Player
computeWinner board =
  let winPositions :: [[(Int, Int)]]
      winPositions =
        [ [(1, 1), (2, 1), (3, 1)]
        , [(1, 2), (2, 2), (3, 2)]
        , [(1, 3), (2, 3), (3, 3)]
        , [(1, 1), (1, 2), (1, 3)]
        , [(2, 1), (2, 2), (2, 3)]
        , [(3, 1), (3, 2), (3, 3)]
        , [(1, 1), (2, 2), (3, 3)]
        , [(1, 3), (2, 2), (3, 1)]
        ]
      matchCell :: Cell -> (Int, Int) -> Bool
      matchCell player (i, j) = (board ! (i, j)) == player
      isWinPosition :: Cell -> [(Int, Int)] -> Bool
      isWinPosition player position = all (matchCell player) position
      isWin :: Cell -> Bool
      isWin player = any (isWinPosition player) winPositions
   in if (isWin Self)
        then Just SelfPlayer
        else if (isWin Opponent)
               then Just OpponentPlayer
               else Nothing

b :: Board
b =
  listArray
    ((1, 1), (3, 3))
    [ Self
    , Self
    , Self
    , Opponent
    , NotPlayed
    , NotPlayed
    , Opponent
    , Opponent
    , NotPlayed
    ]

-- simulate a node until the end
simulate :: GameTreeZipper GameNode -> GameTreeZipper GameNode
simulate (TerminatedNode t, b) = (TerminatedNode t, b)

-- simulate (PlayingNode )
computeNextPlayer :: Player -> Player
computeNextPlayer OpponentPlayer = SelfPlayer
computeNextPlayer SelfPlayer     = OpponentPlayer

nextMoves :: GameState -> [GameState]
nextMoves (GameState nextPlayer board) =
  let nextCell =
        case nextPlayer of
          SelfPlayer     -> Self
          OpponentPlayer -> Opponent
      playNextMove (i, j) board = board // [((i, j), nextCell)]
      possibilities =
        [ nextGameState
        | i <- [1 .. 3]
        , j <- [1 .. 3]
        , (board ! (i, j)) == NotPlayed
        , let nextBoard = playNextMove (i, j) board
        , let nextGameState = GameState (computeNextPlayer nextPlayer) nextBoard
        ]
   in possibilities

someFunc :: IO ()
someFunc = putStrLn "someFunc"
