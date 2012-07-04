// $Id: javadef.h,v 1.24 2002/05/22 06:56:45 ericb Exp $
// DO NOT MODIFY THIS FILE - it is generated using jikespg on java.g.
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1997, 1998, 1999, 2001, 2002 International
// Business Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef javadef_INCLUDED
#define javadef_INCLUDED

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

enum {
      ERROR_SYMBOL      = 108,
      MAX_DISTANCE      = 30,
      MIN_DISTANCE      = 3,
      MAX_NAME_LENGTH   = 38,
      MAX_TERM_LENGTH   = 38,
      NUM_STATES        = 575,

      NT_OFFSET         = 108,
      BUFF_UBOUND       = 31,
      BUFF_SIZE         = 32,
      STACK_UBOUND      = 127,
      STACK_SIZE        = 128,
      SCOPE_UBOUND      = 58,
      SCOPE_SIZE        = 59,
      LA_STATE_OFFSET   = 5558,
      MAX_LA            = 2,
      NUM_RULES         = 355,
      NUM_TERMINALS     = 108,
      NUM_NON_TERMINALS = 159,
      NUM_SYMBOLS       = 267,
      START_STATE       = 754,
      EOFT_SYMBOL       = 86,
      EOLT_SYMBOL       = 86,
      ACCEPT_ACTION     = 5202,
      ERROR_ACTION      = 5203
     };


#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif /* javadef_INCLUDED */
