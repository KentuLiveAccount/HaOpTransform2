module FunctionalTestRunner
(
    TestCase (TestCase, tsName, tsInit, tsChanges, tsExpected),
    test
) where

    
data TestCase a b c = TestCase {tsName :: String, tsInit :: a, tsChanges :: [b], tsExpected :: c} deriving (Eq, Show)

test :: (Eq c, Eq b, Show a, Show b, Show c) => (a -> [b] -> c) -> TestCase a b c -> IO ()
test fn tc@(TestCase name init changes expected)  
    | (fn init changes) == expected = putStrLn $ unwords [name, "passed"]
    | otherwise = putStrLn $ name ++ " failed" ++ 
        "\n\tInit:     " ++ (show init) ++
        "\n\tExpected: " ++ (show expected) ++ 
        "\n\tResult:   " ++  (show $ fn init changes)
