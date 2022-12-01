import System.IO
import Control.Monad
import Data.List


popNextSum :: [String] -> (Integer, [String])
popNextSum [] = (0, [])
popNextSum ("": t) = (0, t)
popNextSum (h: t) = ((read h :: Integer) + rs, rt)
    where (rs, rt) = popNextSum(t)


getSums :: [String] -> [Integer]
getSums [] = []
getSums l = [s] ++ getSums r
    where (s, r) = popNextSum l


main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let inputlines = lines contents
        sums = getSums inputlines
        sortedSums = reverse $ sort sums

    -- PART I: highest sum
    print $ sortedSums !! 0

    -- PART II: sum of three highest sums
    print $ sortedSums !! 0 + sortedSums !! 1 + sortedSums !! 2
