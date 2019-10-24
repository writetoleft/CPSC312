module Sessival where
{- Sessival (feat. Haskell) - Algorithma -}
--lol sarp

import Euterpea
import System.Random

-- keys represented as lists of absolute pitches
type Key = [AbsPitch]

-- can add more interesting keys using absolute pitches in [0,11] 
-- this uses lowest octave for genKeyNotes modulo
-- e.g. cmaj is C,D,E,F,G,A,B in octave -1
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
           fmaj,bfmaj,efmaj,afmaj,dfmaj,cfmaj,cphryg] 

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

-- generate a list of absolute pitches across all octaves from key
genKeyNotes :: Key -> [AbsPitch]
genKeyNotes l = 
    map (\n -> (n::AbsPitch)) (filter (\n -> (n `mod` 12) `elem` l) [0..127])
 
-- returns the first key with most matches to a user input list of absolute pitches  
--     acckey preserves the best matching key so far
--     accint preserves the highest number of matches in acckey  
findBestKey :: [AbsPitch] -> [Key] -> Key -> Int -> Key
findBestKey [] _ _ _= cmaj
findBestKey _ [] acckey accint = acckey
findBestKey l (k:ks) acckey accint = 
    if matches > accint
        then findBestKey l ks k matches
        else findBestKey l ks acckey accint
    where matches = keyMatchesHelper l k

-- foldr to calculate number  of matches between list of absolute pitches and a given key
keyMatchesHelper :: [AbsPitch] -> Key -> Int
keyMatchesHelper l k = foldr (\x -> if (isinkey x k) then (+1) else (+0)) 0 l        

-- IO number generation test
main :: IO ()
main = do
    -- num :: Float
    num <- getStdRandom (randomR (1,12)) :: IO Int
    -- This "extracts" the float from IO Float and binds it to the name num
    print num
    print (num * 2)

-- threads current key into 3 separate generators to return larger composition  
--      treble1 is a melodic line at half the speed it was generated
--      treble2 is a melodic line at twice the speed it was generated
--      bassroot is a line of root notes for chords to be built from by transposing    
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

-- initializes new generator and generates a Music type containing
--      a random line of treble-constrained quarter notes filtered by key 
--      a random instrument modifying the whole tree     
playTreble :: Key -> IO (Music AbsPitch)
playTreble k = do
        g0 <- newStdGen
        let nums = take 48 (randomRs (72::AbsPitch, 96) g0)
        let keyednums = take 16 (filter (\x -> isinkey x k) nums)
        let inst = fst (randomR (1,9) g0)
        let m = instrument (getTreble inst) (line $ map (\n -> (note qn n)) keyednums) 
        return m

-- initializes new generator and generates a Music type containing
--      a random line of bass-constrained half notes filtered by key 
--      a random instrument modifying the whole tree 
playBass :: Key -> IO (Music AbsPitch)    
playBass k = do
        g0 <- newStdGen
        let nums = take 48 (randomRs (36::AbsPitch, 60) g0)
        let nums2 = take 8 (filter (\x -> isinkey x k) nums)
        let inst = fst (randomR (1,9) g0)
        let m = instrument (getBass inst) (line $ map (\n -> (note hn n)) nums2)
        return m    

-- return whether absolute pitch x is in key k          
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