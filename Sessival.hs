module Sessival where
{- Sessival (feat. Haskell) - Algorithma -}
--lol sarp

import Euterpea
import System.Random

-- keys represented as lists of absolute pitches
type Key = [AbsPitch]

cmaj = [0,2,4,5,7,9,11]

gmaj = [0,2,4,6,7,9,11]
dmaj = [1,2,4,6,7,9,11]
amaj = [1,2,4,6,8,9,11]
emaj = [1,3,4,6,8,9,11]
bmaj = [1,3,4,6,8,10,11]
fsmaj = [1,3,5,6,8,10,11]

fmaj = [0,2,4,5,7,9,10]
bfmaj = [0,2,3,5,7,9,10]
efmaj = [0,2,3,5,7,8,10]
afmaj = [0,1,3,5,7,8,10]
dfmaj = [0,1,3,5,6,8,10]
cfmaj = [1,3,4,6,8,10,11]

cphryg = [0,1,4,5,6,8,11]

keylist = [cmaj,gmaj,dmaj,amaj,emaj,bmaj,fsmaj,
           fmaj,bfmaj,efmaj,afmaj,dfmaj,cfmaj] 

-- possible bass instruments
getBass :: Int -> InstrumentName
getBass 0 = AcousticGrandPiano
getBass 1 = ChorusedPiano
getBass 2 = AcousticBass
getBass 3 = Lead1Square
getBass 4 = SynthBass1
getBass 5 = SynthStrings1
getBass 6 = RhodesPiano
getBass 7 = SlapBass1
getBass 8 = Cello
getBass 9 = OrchestralHarp

-- possible treble instruments
getTreble :: Int -> InstrumentName
getTreble 0 = AcousticGrandPiano
getTreble 1 = ChorusedPiano
getTreble 2 = ElectricGuitarJazz
getTreble 3 = Lead1Square
getTreble 4 = Lead6Voice
getTreble 5 = SynthStrings1 
getTreble 6 = RhodesPiano 
getTreble 7 = Xylophone
getTreble 8 = DistortionGuitar
getTreble 9 = Percussion

genKeyNotes :: Key -> [AbsPitch]
genKeyNotes l = 
    map (\n -> (n::AbsPitch)) (filter (\n -> (n `mod` 12) `elem` l) [0..127])
 
-- returns the key that best matches a list of absolute pitches using two accumulators   
findBestKey :: [AbsPitch] -> [Key] -> Key -> Int -> Key
findBestKey [] _ _ _= cmaj
findBestKey _ [] acckey accint = acckey
findBestKey l (k:ks) acckey accint = 
    if matches > accint
        then findBestKey l ks k matches
        else findBestKey l ks acckey accint
    where matches = keyMatchesHelper l k

keyMatchesHelper :: [AbsPitch] -> Key -> Int
keyMatchesHelper l k = foldr (\x -> if (isinkey x k) then (+1) else (+0)) 0 l        

main :: IO ()
main = do
    -- num :: Float
    num <- getStdRandom (randomR (1,12)) :: IO Int
    -- This "extracts" the float from IO Float and binds it to the name num
    print num
    print (num * 2)

genMusic :: Key -> IO (Music AbsPitch)
genMusic k = do
        treble1 <- playTreble k
        let trebleslow = tempo (1/2) (cut wn treble1) 
        treble2 <- playTreble k
        let treblefast = tempo 2 (treble2 :+: (retro treble2))
        bassroot <- playBass k
        let bassthird = transpose 4 bassroot 
        let bassfifth = transpose 7 bassroot
        let bass = bassroot :=: bassthird :=: bassfifth
        let music = trebleslow :=: treblefast :=: bass
        return music

playTreble :: Key -> IO (Music AbsPitch)
playTreble k = do
        g0 <- newStdGen
        let nums = take 48 (randomRs (72::AbsPitch, 96) g0)
        let keyednums = take 16 (filter (\x -> isinkey x k) nums)
        let inst = fst (randomR (1,9) g0)
        let m = instrument (getTreble inst) (line $ map (\n -> (note qn n)) keyednums) 
        return m

playBass :: Key -> IO (Music AbsPitch)    
playBass k = do
        g0 <- newStdGen
        let nums = take 48 (randomRs (36::AbsPitch, 60) g0)
        let nums2 = take 8 (filter (\x -> isinkey x k) nums)
        let inst = fst (randomR (1,9) g0)
        let m = instrument (getBass inst) (line $ map (\n -> (note hn n)) nums2)
        return m    

isinkey :: AbsPitch -> Key -> Bool        
isinkey x k
    | x `elem` (genKeyNotes k) = True
    | otherwise = False
{-    
showmod :: AbsPitch -> IO ()        
showmod x = print (mod x 12)

abspitchelem :: AbsPitch -> Bool
abspitchelem x = (x::AbsPitch) `elem` (genKeyNotes keycmaj)
-}