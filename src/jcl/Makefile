# $Id: Makefile,v 1.2 1999/12/09 18:02:26 lord Exp $
#
# This software is subject to the terms of the IBM Jikes Compiler Open
# Source License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.

# Symbols used:
# O is suffix for object file. OBJECTS is list of objects file for Jikes.
#SYSOBJECTS is list of needed system files (typically setargv for non-unix system).
# EXECUTABLE names executable 
# CC is compiler. LD is linker. ERASE erases files.
# CCREQUIREDFLAGS gives CC flags that must be used; CCDEBUGFLAGS gives
# flags for debug, CCOPTFLAGS gives flags for optimized version
# LDFLAGS gives required LD flags; LDDEBUGFLAGS gives flags to link for debug

# Here are definitions for a number of OS's.
# Uncomment the clause for your system before running make.

# g++ definitions (SGI IRIX 5.X, 6.X)
# Use -mgas instead of -mmips-as to enable the gnu assembler.
# courtesy of Moses DeJong (dejong at cs.umn.edu), 25 Feb 99
#
#CC=g++
#CCREQUIREDFLAGS=-c -DUNIX -DBIGENDIAN -DTYPE_bool -ansi -mmips-as
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O3
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=g++
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -o $(EXECUTABLE)
#O=o
#SYSOBJECTS=
#
## Sun Solaris Sparc  definitions using IBM icc compiler
## NOTE: Only use this is using IBM's compiler 
#CC=xlC
#CCREQUIREDFLAGS=-DUNIX -DBIGENDIAN -langlvl=ansi -+ -c -qnotempinc
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O3 -qstrict
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=xlC
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -Bstatic -o$(EXECUTABLE)
#O=o
#SYSOBJECTS=
#
## Sun Solaris CC definitions (SunOS) (add +w for extra warnings)
## provided by mo dejong (dejong at cs.umn.edu), 2 Feb 1999
#CC=CC
#CCREQUIREDFLAGS=-c -DUNIX -DBIGENDIAN
#CCDEBUGFLAGS=-DTEST -g -xs
#CCOPTFLAGS=-fast
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=CC
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -o $(EXECUTABLE)
#O=o
#SYSOBJECTS=
#
## g++ definitions (Solaris), courtesy of Philip Brown (phil@bolthole.com)
#CC=g++
## use -W to see warnings (should review from time to time)
#CCREQUIREDFLAGS=-DBIGENDIAN -DUNIX -funsigned-char  -c -DTYPE_bool -ansi 
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=g++
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -o$(EXECUTABLE)
#O=o
#SYSOBJECTS=
#
#
## OS/2 definitions for IBM Cset++ (icc) compiler
#CC=icc
#CCREQUIREDFLAGS=-c -Tdp -G5 -DICC -Dcerr=cout -DUNIX_FILE_SYSTEM
#CCDEBUGFLAGS=-DTEST -Ti
#CCOPTFLAGS=-Tdp -O -DNDEBUG 
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes.exe
#LD=ilink
#LDDEBUGFLAGS=/Ti+
#LDFLAGS=/O:$(EXECUTABLE) /PM:VIO /STACK:400000 /NOE
#O=obj
#SYSOBJECTS=d:\ibmcpp\lib\setargv.obj
#
## gcc definitions
#CC=gcc
#CCREQUIREDFLAGS=-DUNIX -DBIGENDIAN -c -DTYPE bool -ansi -W
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=ld
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -o$(EXECUTABLE)
#O=o
#SYSOBJECTS=
#
## g++ definitions (RedHat 5.* - libc5)
#CC=g++
## use -W to see warnings (should review from time to time)
#CCREQUIREDFLAGS=-DGNU_LIBC5 -DUNIX -funsigned-char  -c -DTYPE_bool -ansi 
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=g++
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -o$(EXECUTABLE)
#O=o
#SYSOBJECTS=
#
## HP-UX/g++ changes
## courtesy of Kero van Gelder (gelderk@natlab.research.philips.com)
#CC=g++
## use -W to see warnings (should review from time to time)
#CCREQUIREDFLAGS=-D_INCLUDE_POSIX_SOURCE -DBIGENDIAN -DSTAT_POSIX_1 -DUNIX -funsigned-char  -c -DTYPE_bool -ansi
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
##CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=g++
#LDDEBUGFLAGS=-g
#LDFLAGS=$(LDDEBUGFLAGS) -L. -o$(EXECUTABLE) -lstdc++
#O=o
#SYSOBJECTS=
#
#
## Win95/NT definitions for IBM Cset++ (icc) compiler
## Note there are known problems with IEEE if Microsoft's compiler is used
#CC=g:\icc\bin\icc
#CCREQUIREDFLAGS=-c -Gx -Tdp -DWIN32_FILE_SYSTEM -DICC -Dcerr=cout 
#CCDEBUGFLAGS=-DTEST -Ti
#CCOPTFLAGS=-Tdp -O -DNDEBUG 
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes.exe
#LD=g:\icc\bin\ilink
#LDDEBUGFLAGS=/DEBUG
#LDFLAGS=/O:$(EXECUTABLE) /PM:VIO /STACK:400000 /NOE
#O=obj
#SYSOBJECTS=g:\icc\lib\setargv.obj
#
# Win95/NT definitions for Microsoft Visual C++ (cl) compiler
# There are known problems with IEEE if use this compiler
#CC=cl
#CCREQUIREDFLAGS=-DWIN32_FILE_SYSTEM -DMICROSOFT -DTYPE_bool 
#CCDEBUGFLAGS=-DTEST /Zi
#CCOPTFLAGS=-O -DNDEBUG 
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes.exe
#LD=cl
#LDDEBUGFLAGS=/Zi
LDFLAGS=
#O=obj
#SYSOBJECTS=c:\vc98\lib\setargv.obj
#
## AIX definitions
#CC=xlC
#CCREQUIREDFLAGS=-DUNIX -DBIGENDIAN -langlvl=ansi -qnotempinc -+ -qinlglue -c
#CCDEBUGFLAGS=-DTEST -g
#CCOPTFLAGS=-O3 -qstrict
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#ERASE=rm -f
#EXECUTABLE=jikes
#LD=xlC
#LDDEBUGFLAGS=-g
#LDFLAGS=-L. -o$(EXECUTABLE) $(LDDEBUGFLAGS)
#O=o
#SYSOBJECTS=
#
## g++ definitions (RedHat 5.* - glibc)
CC=g++
## use -W to see warnings (should review from time to time)
CCREQUIREDFLAGS=-DSTAT_POSIX -DUNIX -funsigned-char  -c -DTYPE_bool -ansi 
CCDEBUGFLAGS=-DTEST -g 
CCOPTFLAGS=-O
CCFLAGS=$(CCREQUIREDFLAGS) $(CCDEBUGFLAGS)
#CCFLAGS=$(CCREQUIREDFLAGS) $(CCOPTFLAGS)
ERASE=rm -f
EXECUTABLE=jcl
LD=g++
LDDEBUGFLAGS=-g
LDFLAGS=-L. -o$(EXECUTABLE)
O=o
SYSOBJECTS=

OBJECTS= jcl.$(O) jcl_iclass.$(O) jcl_long.$(O) jcl_class.$(O) jcl_bc_op.$(O) 



$(EXECUTABLE):	$(OBJECTS)
	$(LD) $(LDFLAGS)  $(OBJECTS) $(SYSOBJECTS)


jcl.$(O):  jcl.cpp jcl_iclass.h jcl_class.h jcl_access.h jcl_bool.h \
	jcl_int.h jcl_dynamic.h  jcl_unicode.h 
	$(CC) -c $(CCFLAGS) jcl.cpp 

jcl_iclass.$(O):  jcl_iclass.cpp jcl_iclass.h jcl_class.h jcl_access.h \
	jcl_bool.h jcl_int.h jcl_dynamic.h  jcl_unicode.h 
	$(CC) -c $(CCFLAGS) jcl_iclass.cpp 

jcl_long.$(O):  jcl_long.cpp jcl_long.h jcl_int.h 
	$(CC) -c $(CCFLAGS) jcl_long.cpp 

jcl_class.$(O):  jcl_class.cpp jcl_class.h jcl_access.h jcl_bool.h \
	jcl_int.h jcl_dynamic.h jcl_unicode.h  jcl_long.h 
	$(CC) -c $(CCFLAGS) jcl_class.cpp 

jcl_bc_op.$(O):  jcl_bc_op.cpp jcl_bc_op.h jcl_dynamic.h jcl_class.h \
	jcl_access.h jcl_bool.h jcl_int.h jcl_unicode.h 
	$(CC) -c $(CCFLAGS) jcl_bc_op.cpp 

clean:
	$(ERASE) $(EXECUTABLE)
	$(ERASE) $(OBJECTS)
