module Main where

-- To run it, try:
-- ghci
-- :load Main
-- main

import Euterpea
import System.Random
import Song

-- hand transcription of the Windows shutdown melody
shutdownMelody :: Music Pitch
shutdownMelody = line [af 6 (2/3 * qn), ef 6 (2/3 * qn), af 5 (2/3 * qn), bf 5 hn]
    
shutdownChords :: Music Pitch
shutdownChords =
    chord [af 3 hn, ef 3 hn] :+:
    chord [bf 3 hn, ef 3 hn]
    
shutdown :: Music Pitch
shutdown = tempo 1.3 (shutdownMelody :=: shutdownChords)    

-- a sixteenth note rest for testing 
initMusic :: Music Pitch
initMusic = instrument SynthBrass1 (rest sn)

-- list of possible "yes" answers
affirmlist = ["y","yes","ye","oui","yea","yup","yee"]

main :: IO()
main = 
    do 
        putStrLn "Would you like to generate a song? 'y' for yes, anything else for no"
        ans <- getLine
        if (ans `elem` affirmlist)
            then do
                melody <- melodyInit
                play melody
                putStrLn "Send this melody off for generation?"
                gans <- getLine    
                if (gans `elem` affirmlist)
                    then do
                        putStrLn "Cool, WORK IN PROGRESS"
                        main
                    else do
                        putStrLn "Oh okay :("
                        main                
            else do
                putStrLn "Goodbye!"
                play shutdown

melodyInit :: IO (Music Pitch)
melodyInit = 
    do
        putStrLn "Enter your starting note. (see legend for key mappings)"
        pitch <- pitchReader
        play pitch
        melodyBuilder pitch
            
melodyBuilder :: Music Pitch -> IO (Music Pitch)   
melodyBuilder music = 
    do 
        putStrLn "Would you like to continue with your melody? y/n"
        ans <- getLine
        if (ans `elem` affirmlist)            
            then do
                putStrLn "Enter your next note. (see legend for key mappings)"
                pitch <- pitchReader
                let newmusic = music :+: pitch
                play newmusic    
                melodyBuilder newmusic
            else return music           

-- reads first char of user entry until it returns a music primitive              
pitchReader :: IO (Music Pitch)
pitchReader = 
    do
        readPitch <- getLine
        let trynote = head readPitch
        if (trynote `elem` ['a','w','s','e','d','f','t','g','y','h','u','j','k'])
            then do
                let pitch = pitchHelper trynote 
                putStrLn ("Adding " ++ (show pitch))
                return pitch
            else do
                putStrLn "Not a legal pitch entry."
                pitchReader

-- converts a legal char to corresponding music primitive
pitchHelper :: Char -> (Music Pitch) 
pitchHelper 'a' = (c 4 qn)
pitchHelper 'w' = (cs 4 qn)
pitchHelper 's' = (d 4 qn)
pitchHelper 'e' = (ds 4 qn)
pitchHelper 'd' = (e 4 qn)
pitchHelper 'f' = (f 4 qn)
pitchHelper 't' = (fs 4 qn)
pitchHelper 'g' = (g 4 qn)
pitchHelper 'y' = (gs 4 qn)
pitchHelper 'h' = (a 4 qn)
pitchHelper 'u' = (as 4 qn)
pitchHelper 'j' = (b 4 qn)
pitchHelper 'k' = (c 5 qn)


    
    
--go :: IO Music Pitch
--go =            