// $Id: jcl_bool.h,v 1.2 1999/12/09 18:02:26 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_bool_INCLUDED
#define jcl_bool_INCLUDED

//==========================================================================================================================
// We define the type "bool" and the constants "false" and "true".   
// The type bool as well as the constants false and true are expected
// to become standard C++. When that happens, these declarations should 
// be removed.                                                       
//======================================================================
#ifndef TYPE_bool
typedef int bool;
enum {false = 0, true = 1};
#endif

#endif /* bool_INCLUDED */
