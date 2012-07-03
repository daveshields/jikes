// $Id: javadef.h,v 1.11 1999/10/17 02:02:12 shields Exp $
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

enum {
      ERROR_CODE,
      BEFORE_CODE,
      INSERTION_CODE,
      INVALID_CODE,
      SUBSTITUTION_CODE,
      DELETION_CODE,
      MERGE_CODE,
      MISPLACED_CODE,
      SCOPE_CODE,
      MANUAL_CODE,
      SECONDARY_CODE,
      EOF_CODE,

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


#endif /* javadef_INCLUDED */
