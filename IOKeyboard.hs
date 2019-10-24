module IOKeyboard where

-- To run it, try:
-- ghci
-- :load IOKeyboard
-- start

import Euterpea
import System.Random
import Sessival

-- hand transcription of a shutdown melody
shutdownHarmony1 :: Music Pitch
shutdownHarmony1 = chord[ef 6 (2/3 * qn), c 5 (2/3 * qn)] 
shutdownHarmony2 :: Music Pitch
shutdownHarmony2 = chord[af 5 (2/3 * qn), c 5 (2/3 * qn)]

shutdownMelody :: Music Pitch
shutdownMelody = line [shutdownHarmony2, shutdownHarmony1]
    
shutdownChords :: Music Pitch
shutdownChords = chord [af 3 hn]
    
shutdown :: Music Pitch
shutdown = retro (tempo 1.3 (shutdownMelody :=: shutdownChords))    

-- list of possible "yes" answers
affirmlist = ["y","yes","ye","oui","yea","yup","yee"]

start :: IO()
start = 
    do
        putStrLn ("--MUSIC GENERATOR--")
        melody <- melodyInit
        play (fst melody)
        let key = findBestKey (snd melody) keylist cmaj 0
        play (line $ map (\n -> (transpose 60 (note en n))) key)
        putStrLn ("Should I generate a song in that key? (the closest matching key to your input) y/n")
        gans <- getLine    
        if gans `elem` affirmlist
            then do
                callGen key
                putStrLn "Thank you :)"
                play shutdown
            else do
                putStrLn "Do you want to start over? y/n"
                qans <- getLine
                if qans `elem` affirmlist
                    then do
                        start
                    else do
                        putStrLn "Okay bye :("
                        play shutdown
                
callGen :: Key -> IO ()
callGen k = do
    song <- genMusic k
    play song
    putStrLn "Do you want to save that one? y/n"
    ans <- getLine
    if ans `elem` affirmlist
        then do
            writeToFile song
            putStrLn "Do you want to generate another? y/n"
            gans <- getLine
            if gans `elem` affirmlist
                then do
                    callGen k
                else do
                    putStrLn "Generation is over!"
        else do
            putStrLn "Do you want to generate another? y/n"
            gans <- getLine
            if gans `elem` affirmlist
                then do
                    callGen k
                else do
                    putStrLn "Generation is over!"
                    
writeToFile :: Music AbsPitch -> IO ()
writeToFile s = do
    putStrLn "Give your file a name:"
    name <- getLine
    writeMidi (name ++ ".midi") s
    putStrLn "File is saved in the same directory as this Haskell code."

melodyInit :: IO ((Music AbsPitch), Key) 
melodyInit = 
    do
        putStrLn "What sorts of notes would you like to hear? (see legend in this directory for key mappings)"
        p <- pitchReader
        let abs = absPitch p
        let n = note qn abs        
        play n
        melodyBuilder (n,(abs:[]))
            
melodyBuilder :: ((Music AbsPitch), Key) -> IO ((Music AbsPitch), Key)   
melodyBuilder (m, k) = 
    do 
        putStrLn "Would you like to enter more notes? y/n"
        ans <- getLine
        if ans `elem` affirmlist            
            then do
                putStrLn "Enter your next note. (see legend in this directory for key mappings)"
                p <- pitchReader
                let abs = absPitch p
                let n = note qn abs                
                let newmusic = m :+: n
                play newmusic
                --putStrLn (show abs ++ show k)       
                melodyBuilder (newmusic, (abs:k)) 
            else return (m, k)           

-- reads first char of user entry until it returns a music primitive              
pitchReader :: IO (Pitch)
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

-- converts a legal char to corresponding pitch 
pitchHelper :: Char -> Pitch 
pitchHelper 'a' = (C,4) 
pitchHelper 'w' = (Cs,4)
pitchHelper 's' = (D,4)
pitchHelper 'e' = (Ds,4)
pitchHelper 'd' = (E,4)
pitchHelper 'f' = (F,4)
pitchHelper 't' = (Fs,4)
pitchHelper 'g' = (G,4)
pitchHelper 'y' = (Gs,4)
pitchHelper 'h' = (A,4)
pitchHelper 'u' = (As,4)
pitchHelper 'j' = (B,4)
pitchHelper 'k' = (C,5)          