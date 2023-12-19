import NodeParser

main :: IO ()
main = do
  putStrLn "Time to parse some json!"
  f <- parseFile "test/test.json"
  print f
