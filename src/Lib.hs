module Lib
    ( someFunc
    ) where


data GameTree a = TerminatedNode a | PlayingNode a [GameTree a] deriving (Show)

data GameTreeCrumb a = GameTreeCrumb a [GameTree a] [GameTree a] deriving (Show)

type GameTreeZipper a = (GameTree a, [GameTreeCrumb a])

gameTreeUp :: GameTreeZipper a -> Maybe (GameTreeZipper a)
gameTreeUp (currentGameTree, []) = Nothing
gameTreeUp (currentGameTree, GameTreeCrumb d ls rs:bs) = Just (PlayingNode d (ls ++ [currentGameTree] ++ rs), bs)

-- gameTreeFocus :: GameTreeZipper a -> Maybe (GameTreeZipper a)
-- gameTreeFocus (TerminatedNode _, _) = Nothing
-- gameTreeFocus (PlayingNode d childNodes, _) = Nothing


-- getChildren :: GameTreeZipper a -> [GameTreeZipper a]
-- getChildren (TerminatedNode _, _) = []
-- getChildren (PlayingNode d children, treeCrumbs) = children

someFunc :: IO ()
someFunc = putStrLn "someFunc"
