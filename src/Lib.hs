module Lib
  ( someFunc
  ) where

import           Data.List
import Data.Array.IArray


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

data Player
  = SelfPlayer
  | OpponentPlayer
  deriving (Eq)

type NextPlayer = Player

data Board =
  Board NextPlayer
        [[Cell]]

data VisitedBoard = VisitedBoard
  { q     :: Int
  , n     :: Int
  , board :: Board
  }

data GameNode
  = Unvisited Board
  | Visited VisitedBoard

-- computeWinner :: Board -> Maybe Player
-- computeWinner (Board nextPlayer board) =
--   let currentPlayer =
--         case nextPlayer of
--           SelfPlayer     -> OpponentPlayer
--           OpponentPlayer -> SelfPlayer
--       playedCell player cell =
--         case cell of
--           NotPlayed -> False
--           Self      -> currentPlayer == SelfPlayer
--           Opponent  -> currentPlayer == OpponentPlayer
--       isLineWinner player line = all (playedCell player) line
--       isLinesWinner player = any (isLineWinner player) board
--       transposedBoard = transpose board
--       isColumnsWinner player = isLinesWinner player transposedBoard
   in Nothing

someFunc :: IO ()
someFunc = putStrLn "someFunc"
