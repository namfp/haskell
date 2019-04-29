module Lib
  ( someFunc
  ) where

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

data Cell
  = NotPlayed
  | Self
  | Opponent

data NextPlayer = SelfPlayer | OpponentPlayer

data Board = Board NextPlayer [[Cell]]

data VisitedBoard = VisitedBoard
  { q     :: Int
  , n     :: Int
  , board :: Board
  }

data GameNode
  = Unvisited Board
  | Visited VisitedBoard

getChildren :: GameTreeZipper a -> [GameTreeZipper a]
getChildren (TerminatedNode _, _) = []
getChildren (PlayingNode d (Just children), treeCrumbs) =
  let withLeftAndRight = getElementWithLeftAndRight children
      convertToZipper (elem, left, right) =
        (elem, GameTreeCrumb d left right : treeCrumbs)
   in map convertToZipper withLeftAndRight

someFunc :: IO ()
someFunc = putStrLn "someFunc"
