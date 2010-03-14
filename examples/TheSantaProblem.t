module TheSantaProblem where
    
import POSIX
import RandomGenerator

struct Helper where
    doSomethingElse  :: Action
    helpSanta        :: Action
    
struct Santa where
    announceElf      :: Action -> Action
    announceReindeer :: Action -> Action
    
helper env rand announce msg =
    class
        helpSanta = action
            env.stdout.write msg
            doSomethingElse
            
        doSomethingElse = action
            t <- rand.next
            after (millisec (t `mod` 3000 + 3000)) (announce helpSanta)
        
        result Helper {..}
                
santaClaus env =
    class
        reindeers := []
        elves := []
        
        announceReindeer r = action
            reindeers := r : reindeers
            if length reindeers == 9 then
                env.stdout.write "------\nHo! Ho! Ho! Let's deliver toys!\n"
                sequence reindeers
                reindeers := []
        
        announceElf e = action
            elves := e : elves
            if length elves == 3 then
                env.stdout.write "------\nHo! Ho! Ho! Let's meet in the study!\n"
                sequence elves
                elves := []
        
        result Santa {..}

elfMsg n      = "Elf " ++ show n ++ " meeting in the study\n"
reindeerMsg n = "Reindeer " ++ show n ++ " delivering toys\n"

root :: World -> Cmd () ()
root w = do
   env = new posix w
   rand = new baseGen (microsecOf env.startTime)
   santa = new santaClaus env
   elves = new mapM (\n -> helper env rand santa.announceElf (elfMsg n)) [1..10]
   reindeer = new mapM (\n -> helper env rand santa.announceReindeer (reindeerMsg n)) [1..9]
   mapM (.doSomethingElse) (elves ++ reindeer)
