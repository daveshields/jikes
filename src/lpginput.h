// $Id: lpginput.h,v 1.10 2004/03/25 13:32:28 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef lpginput_INCLUDED
#define lpginput_INCLUDED

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

typedef TokenIndex TokenObject;
typedef TokenIndex Location;

inline Location Loc(TokenObject i) { return i; }

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#include "javasym.h" /* mapping of lexical symbols  */
#include "javadef.h" /* definition of parsing names */
#include "javaprs.h" /* parsing action functions    */

#endif // lpginput_INCLUDED
