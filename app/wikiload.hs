{- Functions incorporated into src/Lib.hs -}

-- -- import Control.Applicative
-- import Control.Monad
-- import Data.Binary
-- import Data.Binary.Get
-- import Data.Map

-- -- import qualified Data.ByteString as BS
-- -- import Data.Binary.Get
-- import qualified Data.ByteString.Lazy as BL
-- -- import Data.Int
-- import Data.Set
 



    
-- readBinaryFiletoIO :: FilePath -> IO (Map Int (Set Int))
-- readBinaryFiletoIO filePath = do
--     -- Read the binary file into a lazy ByteString
--     fileContents <- BL.readFile filePath
    
--     return $ parseListToMap2 $ runGet (replicateM (fromIntegral $ (BL.length fileContents) `div` 4) getWord32le) fileContents

-- main :: IO ()
-- main = do
--     -- Replace "your_binary_file.bin" with the actual path to your binary file
--     let filePath = "wikigraphtestquartsmall.edges"
--     result <- readBinaryFiletoIO filePath
--     print (result)

-- -- intListToTupleList ::  [Int] -> Int ->Bool ->[ (Int, [Int])]
-- -- intListToTupleList [] _ _ =[]
-- -- intListToTupleList (x:ns) cur_idx del |(del==True) = (intListToTupleList ns (cur_idx+1) False)
-- --                                       | otherwise =(cur_idx, neighbors) : (intListToTupleList xs (cur_idx) True) 
-- --                                       where (neighbors,xs) = span( /=0)(x:ns)

-- -- parseListToMap:: [Word32] -> (Map Int [Int])
-- -- parseListToMap x = Data.Map.fromList ( intListToTupleList (Prelude.map fromIntegral x) 1 False)


-- intListToTupleList2 ::  [Int] -> Int ->Bool ->[(Int, (Set Int))]
-- intListToTupleList2 [] _ _ =[]
-- intListToTupleList2 (x:ns) cur_idx del |(del==True) = (intListToTupleList2 ns (cur_idx+1) False)
--                                        | otherwise =(cur_idx, (Data.Set.fromList neighbors)) : (intListToTupleList2 xs (cur_idx) True) 
--                                       where (neighbors,xs) = span( /=0)(x:ns)

-- parseListToMap2:: [Word32] -> (Map Int (Set Int))


-- parseListToMap2 x = Data.Map.fromList ( intListToTupleList2 (Prelude.map fromIntegral x) 1 False)


-- -- listToSet:: [Int]->Set Int
-- -- listToSet x = Data.Set.fromList x