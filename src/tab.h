// $Id: tab.h,v 1.9 2001/01/05 09:13:21 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef tab_INCLUDED
#define tab_INCLUDED

#include "platform.h"

/*
//FIXME: include stuff
#ifdef HAVE_WCHAR_H
# include <wchar.h>
#endif
#include <string.h>
*/

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

class Tab
{
public:
    enum { DEFAULT_TAB_SIZE = 8 };

    inline static int TabSize() { return tab_size; }
    inline static void SetTabSize(int value) { tab_size = value; }

    static int Wcslen(wchar_t *line, int start, int end);

private:
    static int tab_size;
};

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif

