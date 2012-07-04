// $Id: javadef.h,v 1.31 2004/01/23 12:07:03 ericb Exp $
// DO NOT MODIFY THIS FILE - it is generated using jikespg on java.g.
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2003 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef javadef_INCLUDED
#define javadef_INCLUDED

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

enum {
      ERROR_SYMBOL      = 111,
      MAX_DISTANCE      = 30,
      MIN_DISTANCE      = 3,
      MAX_NAME_LENGTH   = 38,
      MAX_TERM_LENGTH   = 38,
      NUM_STATES        = 457,

      NT_OFFSET         = 111,
      BUFF_UBOUND       = 30,
      BUFF_SIZE         = 31,
      STACK_UBOUND      = 127,
      STACK_SIZE        = 128,
      SCOPE_UBOUND      = 56,
      SCOPE_SIZE        = 57,
      LA_STATE_OFFSET   = 5691,
      MAX_LA            = 1,
      NUM_RULES         = 410,
      NUM_TERMINALS     = 111,
      NUM_NON_TERMINALS = 169,
      NUM_SYMBOLS       = 280,
      START_STATE       = 706,
      EOFT_SYMBOL       = 94,
      EOLT_SYMBOL       = 94,
      ACCEPT_ACTION     = 5690,
      ERROR_ACTION      = 5691
     };


#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif /* javadef_INCLUDED */
