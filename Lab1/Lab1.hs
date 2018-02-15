import System.IO
import Data.Char
import Control.Exception

main = return ()

zipL :: [a] -> [a] -> [[a]]
zipL [] [] = []
zipL (x:xs) (y:ys) = [x, y] : zipL xs ys

unzipL :: [[a]] -> ([a],[a])
unzipL [] = ([], [])
unzipL ([x, y] : zs) = (x:xs, y:ys)
    where result = unzipL zs
          xs = fst result
          ys = snd result

zipL2 :: [a] -> [a] -> [[a]]
zipL2 [] [] = []
zipL2 [] ys = [ys]
zipL2 xs [] = [xs]
zipL2 (x:xs) (y:ys) = [x, y] : zipL2 xs ys

multiZipL :: [[a]] -> [[a]]
multiZipL [] = []
multiZipL xs | all null xs = []
             | otherwise = [head x | x <- xs, (not.null) x] : rest
    where rest = multiZipL [tail x | x <- xs, (not.null) x]

csvLine :: Maybe [Int] -> String -> Maybe [Int]
csvLine Nothing _ = Nothing
csvLine acc [] = pure reverse <*> acc
csvLine (Just acc) (x:xs) | x == ',' = csvLine (Just (0 : acc)) xs
                          | isDigit x = csvLine (Just ((head acc * 10 + digitToInt x) : tail acc)) xs
                          | otherwise = Nothing

multiZipF = do
    c <- multiZipF'
    let lists = lines c
    let parsed = mapM (csvLine $ Just [0]) lists
    let zipped = pure multiZipL <*> parsed
    multiZipF'' zipped

multiZipF' = do
    handle <- openFile "Lab1.csv" ReadMode
    c <- hGetContents handle
    return c
multiZipF'' :: Show a => Maybe a -> IO ()
multiZipF'' Nothing = return ()
multiZipF'' (Just x) = writeFile "output.txt" (show x)