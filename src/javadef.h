// $Id: javadef.h,v 1.38 2004/03/24 04:01:16 ericb Exp $
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
      NUM_STATES        = 681,

      NT_OFFSET         = 111,
      BUFF_UBOUND       = 30,
      BUFF_SIZE         = 31,
      STACK_UBOUND      = 127,
      STACK_SIZE        = 128,
      SCOPE_UBOUND      = 104,
      SCOPE_SIZE        = 105,
      LA_STATE_OFFSET   = 8043,
      MAX_LA            = 1,
      NUM_RULES         = 566,
      NUM_TERMINALS     = 111,
      NUM_NON_TERMINALS = 235,
      NUM_SYMBOLS       = 346,
      START_STATE       = 6943,
      EOFT_SYMBOL       = 96,
      EOLT_SYMBOL       = 96,
      ACCEPT_ACTION     = 8042,
      ERROR_ACTION      = 8043
     };


#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif /* javadef_INCLUDED */
