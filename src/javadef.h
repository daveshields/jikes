// $Id: javadef.h,v 1.15 2001/01/05 09:13:20 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef javadef_INCLUDED
#define javadef_INCLUDED

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

enum {
      ERROR_SYMBOL      = 107,
      MAX_DISTANCE      = 30,
      MIN_DISTANCE      = 3,
      MAX_NAME_LENGTH   = 38,
      MAX_TERM_LENGTH   = 38,
      NUM_STATES        = 560,

      NT_OFFSET         = 108,
      BUFF_UBOUND       = 31,
      BUFF_SIZE         = 32,
      STACK_UBOUND      = 127,
      STACK_SIZE        = 128,
      SCOPE_UBOUND      = 56,
      SCOPE_SIZE        = 57,
      LA_STATE_OFFSET   = 5532,
      MAX_LA            = 2,
      NUM_RULES         = 355,
      NUM_TERMINALS     = 108,
      NUM_NON_TERMINALS = 159,
      NUM_SYMBOLS       = 267,
      START_STATE       = 978,
      EOFT_SYMBOL       = 96,
      EOLT_SYMBOL       = 27,
      ACCEPT_ACTION     = 5176,
      ERROR_ACTION      = 5177
     };

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif /* javadef_INCLUDED */

