import Algebra.Graph (adjacencyList)
import Lib (GameMap (..), gameMapFromList)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Game Map"
    [ testCase "Adjacency list of constructed GameMap" $
        let pos = (0, 0)
            rawGameMap =
              [ (pos, [1, 2]),
                (pos, [0, 3]),
                (pos, [0, 3]),
                (pos, [1, 2])
              ]
            GameMap {topology = topology_} = gameMapFromList rawGameMap
            adjList = adjacencyList topology_
         in adjList @?= [(0, [1, 2]), (1, [0, 3]), (2, [0, 3]), (3, [1, 2])]
    ]
