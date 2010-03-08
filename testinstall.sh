timberDir = /home/capitrane/installed/timber/share/timberc-1.0.3
rtsDir    = $timberDir/rtsPOSIX

compiling timber
----------------

#gcc -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -Wall -O2 -fno-strict-aliasing -g -I../include -I../lib -I. -c rts.c
llvm-gcc -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -Wall -fno-strict-aliasing -I../include -I../lib -I. -c rts.c
cd ../lib
timberc *.t
cp *.bc ../rtsPOSIX
cd ../rtsPOSIX
#ar rc libTimber.a Prelude.o ARM.o BitOps.o Data.Functional.List.o Data.Objects.Dictionary.o Data.Objects.Stack.o POSIX.o RandomGenerator.o rts.o
llvm-ar rc libTimber.a *.bc rts.o



/usr/bin/install -c -d          $rtsDir
/usr/bin/install -c timberc.cfg $rtsDir
/usr/bin/install -c rts.h       $rtsDir
/usr/bin/install -c rts.c       $rtsDir
/usr/bin/install -c cyclic.c    $rtsDir
/usr/bin/install -c config.h    $rtsDir
/usr/bin/install -c gc.c        $rtsDir
/usr/bin/install -c timer.c     $rtsDir
/usr/bin/install -c main.c      $rtsDir
/usr/bin/install -c Makefile    $rtsDir

cd 

linking a timber program
------------------------

gcc 
-DPOSIX -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -o Primes Primes.o  
-L$timberDir/rtsPOSIX  
-I$timberDir/include  
-I$timberDir/lib  
-I$timberDir/rtsPOSIX  
-I .  
-DROOT=root_Primes -DROOTINIT=_init_Primes $timberDir/rtsPOSIX/main.c 
-lTimber 

llvm-gcc -DPOSIX -g -m32 -pthread -D_GNU_SOURCE=1 -o Primes Primes.o -L
rtsDir    = $timberDir/rtsPOSIX

compiling timber
----------------

#gcc -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -Wall -O2 -fno-strict-aliasing -g -I../include -I../lib -I. -c rts.c
llvm-gcc -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -Wall -fno-strict-aliasing -I../include -I../lib -I. -c rts.c
cd ../lib
timberc *.t
cp *.bc ../rtsPOSIX
cd ../rtsPOSIX
#ar rc libTimber.a Prelude.o ARM.o BitOps.o Data.Functional.List.o Data.Objects.Dictionary.o Data.Objects.Stack.o POSIX.o RandomGenerator.o rts.o
llvm-ar rc libTimber.a *.bc rts.o



/usr/bin/install -c -d          $rtsDir
/usr/bin/install -c timberc.cfg $rtsDir
/usr/bin/install -c rts.h       $rtsDir
/usr/bin/install -c rts.c       $rtsDir
/usr/bin/install -c cyclic.c    $rtsDir
/usr/bin/install -c config.h    $rtsDir
/usr/bin/install -c gc.c        $rtsDir
/usr/bin/install -c timer.c     $rtsDir
/usr/bin/install -c main.c      $rtsDir
/usr/bin/install -c Makefile    $rtsDir

cd 

linking a timber program
------------------------

gcc 
-DPOSIX -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -o Primes Primes.o  
-L$timberDir/rtsPOSIX  
-I$timberDir/include  
-I$timberDir/lib  
-I$timberDir/rtsPOSIX  
-I .  
-DROOT=root_Primes -DROOTINIT=_init_Primes $timberDir/rtsPOSIX/main.c 
-lTimber 

llvm-ld -o Primes Primes.bc -L/home/capitrane/installed/timber/share/timberc-1.0.3/rtsPOSIX -DROOT=root_Primes -DROOTINIT=_init_Primes /home/capitrane/installed/timber/share/timberc-1.0.3/rtsPOSIX/main.c -lTimber

llvm-gcc -DPOSIX -g -m32 -pthread -D_GNU_SOURCE=1 -o Primes Primes.bc -L/home/capitrane/installed/timber/share/timberc-1.0.3/rtsPOSIX -I/home/capitrane/installed/timber/share/timberc-1.0.3/rtsPOSIX -I. -DROOT=root_Primes -DROOTINIT=_init_Primes /home/capitrane/installed/timber/share/timberc-1.0.3/rtsPOSIX/main.c -lTimber

llvm-gcc -g -O2 -m32 -pthread -D_GNU_SOURCE=1 -Wall -fno-strict-aliasing -I/home/capitrane/installed/timber/share/timberc-1.0.3/rtsPOSIX -I/home/capitrane/installed/timber/share/timberc-1.0.3/lib -I/home/capitrane/installed/timber/share/timberc-1.0.3/include -I. -c rts.c