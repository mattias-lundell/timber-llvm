module SantaProblem2 where

    import POSIX
    import RandomGenerator

    root :: World -> Cmd () ()
    root w = do
       env = new posix w
       rand = new baseGen (microsecOf env.startTime)
       santa = new santaClaus env
       secretary = new semaphore santa.announce santa.confirm
       shepherdRin  = new batch 9 (secretary.lock @ Reindeer)
       shepherdRout = new batch 9 (secretary.unlock @ Reindeer)
       shepherdEin  = new batch 3 (secretary.lock @ Elves)
       shepherdEout = new batch 3 (secretary.unlock @ Elves)
       reindeer = forall n <- [1..10] new helper env rand shepherdRin shepherdRout (rMsg n)
       elves    = forall n <- [1..9] new helper env rand shepherdEin shepherdEout (eMsg n)
       forall x <- elves ++ reindeer do x.doSomethingElse

private

struct Helper where
    doSomethingElse  :: Action
    helpSanta        :: Action
    
helper env rand announce confirm msg =
    class
        doSomethingElse = action
            t <- rand.next
            after (millisec (t `mod` 3000 + 3000)) action
                announce helpSanta
            
        helpSanta = action
            env.stdout.write msg
            confirm doSomethingElse
            
        result Helper {..}

batch n out =
    class
        pending := []
        
        announce a = request
            pending := a : pending
            if length pending == n then
                out pending
                pending := []
            
        result announce

struct Semaphore a where
    lock   :: a -> Request ()
    unlock :: a -> Request ()
    
semaphore out1 out2 =
    class
        pending := []
        taken := False
        
        lock e = request
            if taken then
                pending := pending ++ [e]
            else
                out1 e
                taken := True
        
        unlock e = request
            out2 e
            case pending of
                [] ->   taken := False
                p:ps -> out1 p
                        pending := ps
        
        result Semaphore {..}

data SantaEvent = Reindeer [Action]
                | Elves [Action]
                
struct Santa where
    announce :: SantaEvent -> Request ()
    confirm  :: SantaEvent -> Request ()
    
santaClaus env =
    class
        announce (Reindeer rs) = request
            env.stdout.write "Ho! Ho! Ho! Let's deliver some toys!\n"
            forall r <- rs do r
        
        announce (Elves es) = request
            env.stdout.write "Ho! Ho! Ho! Let's meet in the study!\n"
            forall e <- es do e
        
        confirm (Reindeer rs) = request
            env.stdout.write "Well done, reindeer, off you go now!\n----\n"
            forall r <- rs do r
            
        confirm (Elves es) = request
            env.stdout.write "Good suggestions, elves, off you go now!\n----\n"
            forall e <- es do e
            
        result Santa {..}

eMsg n = "Elf " ++ show n ++ " meeting in the study\n"
rMsg n = "Reindeer " ++ show n ++ " delivering toys\n"
