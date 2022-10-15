module Main where

import DataModel
import DataModelChange
import DataModelTransform
import FunctionalTestRunner

testCases :: [TestCase DataModel DataModelChange DataModel]
testCases = [
    TestCase "insertNothingIntoNothing" (initDM "") [DMCReplace 0 0 ""] (initDM ""),
    TestCase "insertTextIntoNothing" (initDM "") [DMCReplace 0 0 "Hello"] (initDM "Hello"),
    TestCase "insertAtBegining" (initDM "Init") [DMCReplace 0 0 "Hello"] (initDM "HelloInit"),
    TestCase "insertAtEnd" (initDM "Init") [DMCReplace 4 0 "Hello"] (initDM "InitHello"),
    TestCase "replaceFirstChar" (initDM "Init") [DMCReplace 0 1 "Hello"] (initDM "Hellonit")
    ]

main :: IO ()
main = mapM_ (test onDM) testCases