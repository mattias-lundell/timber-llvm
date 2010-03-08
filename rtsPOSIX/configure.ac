#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([rtsPOSIX], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([main.c])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# Force building 32-bit applications since we're not 64-bit clean.
#
# GCC accepts -m32 on most architectures that might have a corresponding
# 64-bit architecture, for example sparc and MIPS. 

old_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -m32"
AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[]],
                [[return 1;]]
        )],
        [],
        [CFLAGS="$old_CFLAGS"]
        )

# Checks for libraries.

# Look for pthreads.
ACX_PTHREAD

if test "$THREAD_SUPPORT" = "no"; then
	AC_MSG_ERROR([Cannot enable threads on this platform.])
fi

CC="$PTHREAD_CC"
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
LIBS="$PTHREAD_LIBS $LIBS"

# Make sure that pthread realtime-threads extension is available. This is
# not the default behaviour on most GNU/Linux dists. If it turns out that
# the realtime-threads extension is not available we add -D_GNU_SOURCE=1
# as this should enable all extensions on most GNU/Linux dists. For more
# information please see <features.h>.
AC_MSG_CHECKING([pthread realtime-threads extension])
AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM([[#include <pthread.h>]],
		[[pthread_mutexattr_setprotocol(NULL, PTHREAD_PRIO_INHERIT);]]
	)],
	[need_gnu_source=no],
	[need_gnu_source=yes]
	)

if test "x$need_gnu_source" = "xyes"; then
	CFLAGS="$CFLAGS -D_GNU_SOURCE=1"
fi

# Finalize the checking for pthread realtime-threads extension. This may
# still fail if -D_GNU_SOURCE=1 does not enable the realtime-threads
# extension. I this is the case then we are either running this on a
# system without the realtime-threads extension or we need to perform some
# other kind of black magic to enable it. If you know of any such platform
# then please feel free to send a patch.
AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM([[#include <pthread.h>]],
		[[pthread_mutexattr_setprotocol(NULL, PTHREAD_PRIO_INHERIT);]]
	)],
	[AC_MSG_RESULT([yes])],
	[AC_MSG_FAILURE([no])]
	)

AC_ARG_WITH(timberc,
[AC_HELP_STRING([--with-timberc[=DIR]], [Path to timberc])],
[with_timberc=$withval]
)

if test "x$with_timberc" = "x"; then
	AC_CHECK_PROGS(TIMBERC, [timberc])
else
	TIMBERC="$with_timberc"
fi

if test "x$TIMBERC" = "x"; then
	AC_MSG_ERROR([Timberc not found.])
fi

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h sys/time.h termios.h])

AC_CHECK_HEADER([libkern/OSAtomic.h],
                [AC_DEFINE_UNQUOTED([HAVE_OSX_ATOMICS], 1, 
                [Define to 1 if OSX atomic operations are supported.])])

# Check the <sys/stat.h> header for XSI extensions. This should already be
# enable by -D_GNU_SOURCE=1, if it's supported.
# NOTE:
#   While it might be possible for the compiler to optimize away the int xsi
#   statement I'm not up for the challenge of writing something that is
#   _impossible_ for the compiler to optimize away.
AC_MSG_CHECKING([sys/stat.h header for XSI extensions])
AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM([[#include <sys/stat.h>]],
		[[mode_t mode_xsi = S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH;]]
	)],
	[AC_MSG_RESULT([yes])],
	[AC_MSG_FAILURE([no])]
	)

# Does this compiler have built-in functions for atomic memory access?
AC_MSG_CHECKING([if gcc supports __sync_bool_compare_and_swap])
 
AC_TRY_LINK(,
[
  int variable = 1;
  return (__sync_bool_compare_and_swap(&variable, 1, 2)
         && __sync_add_and_fetch(&variable, 1)) ? 1 : 0;
],
[
  AC_MSG_RESULT([yes])
  AC_DEFINE(HAVE_BUILTIN_ATOMIC, 1, [Define to 1 if gcc supports __sync_bool_compare_and_swap() a.o.])
],
[
  # Retry with -march set for x86.
  old_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -march=i686"
  AC_TRY_LINK(,
  [
    int variable = 1;
    return (__sync_bool_compare_and_swap(&variable, 1, 2)
           && __sync_add_and_fetch(&variable, 1)) ? 1 : 0;
  ],
  [
    AC_MSG_RESULT([yes])
    AC_DEFINE(HAVE_BUILTIN_ATOMIC, 1, [Define to 1 if gcc supports __sync_bool_compare_and_swap() a.o.])
  ],
  [
    AC_MSG_RESULT([no])
    CFLAGS="$old_CFLAGS"
  ])
])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_TIME

# Look for the timber compiler

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday sysconf])

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(SRC)
AC_SUBST(DEST)
AC_SUBST(TIMBERC)
AC_SUBST(prefix)

AC_CONFIG_FILES([Makefile timberc.cfg])
AC_OUTPUT
