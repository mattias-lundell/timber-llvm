DESTDIR      = /usr/local/share/timberc
prefix       = -1.0.3
CC           = arm-eabi-gcc
CFLAGS       = -Os -g -I. -I../include -I../lib -I./asm
INSTALL	     = install

TIMBERC      = timberc
TIMBERLIBS   = Prelude.t $(filter-out Prelude.t,$(notdir $(wildcard ../lib/*.t)))
OBJS         = $(TIMBERLIBS:.t=.o)

DEST         = libTimber.a

all: 
	@printf "Run make install to install and build this RTS.\n\n"
	@printf "Run make $(DEST) to build this RTS in-place.\n"
	@printf "Notice that building in-place requires the RTS to reside in\n"
	@printf "an installed timberc tree.\n\n"

AOBJARM   = crt.o

$(DEST): crt.o rts.o ea_lcd/lcd_hw.o $(OBJS)
	rm -f $(DEST)
	arm-eabi-ar rc $(DEST) crt.o rts.o ea_lcd/lcd_hw.o $(OBJS)

$(OBJS): %.o: ../lib/%.t ../include/timber.h
	$(TIMBERC) --target ARM --api -c $<

rts.o: rts.h rts.c ../include/timber.h ../include/timber.c ../include/float.c
	$(CC) $(CFLAGS) -Wall -O2 -fno-strict-aliasing -g -I../include -I../lib -I. -c rts.c

# Assemble: create object files from assembler source files. ARM-only
crt.o : crt.S
	$(CC) -c $(CFLAGS) -D__ASSEMBLY__ crt.S -o crt.o

install: 
	$(INSTALL) -d $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) timberc.cfg $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) rts.h $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) rts.c $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) cyclic.c $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) gc.c $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) env.c $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) timer.c $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) main.c $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) -d $(DESTDIR)$(prefix)/rtsARM/ea_lcd
	$(INSTALL) ea_lcd/lcd_hw.c $(DESTDIR)$(prefix)/rtsARM/ea_lcd
	$(INSTALL) ea_lcd/lcd_hw.h $(DESTDIR)$(prefix)/rtsARM/ea_lcd
	$(INSTALL) lpc2468_registers.h $(DESTDIR)$(prefix)/rtsARM	
	$(INSTALL) lpc2478_lcd.h $(DESTDIR)$(prefix)/rtsARM	
	$(INSTALL) my9x10font.h $(DESTDIR)$(prefix)/rtsARM	
	$(INSTALL) crt.S $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) linkerscript.x $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) Makefile $(DESTDIR)$(prefix)/rtsARM
	$(INSTALL) ../lib/ARM.t $(DESTDIR)$(prefix)/lib
	cd $(DESTDIR)$(prefix)/rtsARM && make TIMBERC=$(TIMBERC) $(DEST) 


clean:
	rm *.o *.a
