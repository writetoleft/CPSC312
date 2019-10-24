{- Sessival (feat. Haskell) - Algorithma -}
module Song where

import Euterpea
import System.Random

keycmaj = [0,2,4,5,7,9,11] 
cmaj = map (\n -> (n::AbsPitch)) (filter (\n -> (n `mod` 12) `elem` keycmaj) [0..127])

main2 :: IO ()
main2 = do
    -- num :: Float
    num <- getStdRandom (randomR (1,12)) :: IO Int
    -- This "extracts" the float from IO Float and binds it to the name num
    print num
    print (num * 2)

playmusic = do
    x <- playtreble
    y <- playbass
    play (x :=: y)

playtreble = do
        let g0 = mkStdGen 5
        let nums = take 48 (randomRs (72::AbsPitch, 127) g0)
        let keyednums = take 16 (filter (isinkey) nums)
        let m = line $ map (\n -> addcorrectnote n) keyednums 
        return m

isinkey x
    | x `elem` cmaj = True
    | otherwise = False

addcorrectnote x
    | x `elem` cmaj = note qn x
    | otherwise = rest sn

playbass = do
        let g0 = mkStdGen 5
        let nums = take 48 (randomRs (20::AbsPitch, 40) g0)
        let nums2 = take 16 (filter (isinkey) nums)
        let m = line $ map (\n -> addcorrectnote n) nums2
        return m

showmod x = print (mod x 12)

abspitchelem x = (x::AbsPitch) `elem` cmaj