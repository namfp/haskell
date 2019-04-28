import Test.HUnit

test1 = TestCase (assertEqual "shouldBeGood" 1 1)
tests = TestList [TestLabel "test1" test1]



main :: IO ()
main = do
        counts <- runTestTT tests
        putStrLn $ show counts
