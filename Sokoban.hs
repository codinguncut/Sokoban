module Sokoban where


-- TESTS

testsSoko = TestList $ map TestCase
  [assertEqual ""  1 
                   1 
  ]

prop_empty c1 = (c1::Int) == c1

runTests = do
  runTestTT testsSoko
  quickCheck prop_empty
