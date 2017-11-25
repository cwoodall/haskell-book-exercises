module SecretSanta where
    
import Data.List 
import System.Random

dropN :: Int -> [a] -> (a, [a])
dropN i xs = (xs !! i, take i xs ++ drop (i+1) xs)

-- shuffle :: [a] -> IO()
shuffle xs = do
    g <- getStdGen
    take (length xs) (randomRs (0, (length xs - 1)) g :: [Int])