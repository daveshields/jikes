// $Id: tab.cpp,v 1.2 1999/07/06 13:49:25 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "tab.h"

int Tab::tab_size = Tab::DEFAULT_TAB_SIZE;

//
// Compute the length of a wide character string segment
// after expanding tabs.
//
int Tab::Wcslen(wchar_t *line, int start, int end)
{
    for (int i = start--; i <= end; i++)
    {
        if (line[i] == U_HORIZONTAL_TAB)
        {
            int offset = (i - start) - 1;
            start -= ((tab_size - 1) - offset % tab_size);
        }
        else if (Coutput.ExpandWchar() && line[i] > 0xFF)
             start -= 5;
    }

    return (end - start);
}


