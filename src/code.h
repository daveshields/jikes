// $Id: code.h,v 1.18 2001/09/14 05:31:32 ericb Exp $ -*- c++ -*-
// DO NOT MODIFY THIS FILE - it is generated using gencode.java.
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1999, 2000, 2001, International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

#ifndef code_INCLUDED
#define code_INCLUDED

class Code
{
    //
    // To facilitate the scanning, the character set is partitioned into
    // 8 classes using the array CODE. The classes are described below
    // together with some self-explanatory functions defined on CODE.
    //
    enum {
             LOG_BASE_SIZE       = 9,
             LOG_COMPLEMENT_SIZE = 7,
             BASE_SIZE           = 512,
             SLOT_SIZE           = 128,
             SLOT_MASK           = 127,

             NEWLINE_CODE        = 1,
             SPACE_CODE          = 2,
             BAD_CODE            = 3,
             DIGIT_CODE          = 4,
             OTHER_DIGIT_CODE    = 5,
             LOWER_CODE          = 6,
             UPPER_CODE          = 7,
             OTHER_LETTER_CODE   = 8
         };

    static char code[7912];
    static char *base[512];


public:

    static inline void SetBadCode(wchar_t c)
    {
        base[c >> LOG_COMPLEMENT_SIZE][c] = BAD_CODE;
    }

    static inline void CodeCheck(wchar_t c)
    {
        assert(c >> LOG_COMPLEMENT_SIZE < BASE_SIZE);
        assert(base[c >> LOG_COMPLEMENT_SIZE] + c >= (&code[0]));
        assert(base[c >> LOG_COMPLEMENT_SIZE] + c < (&code[7912]));
    }

    static inline bool CodeCheck(void)
    {
        for (int i = 0; i <= 0xffff; i++)
            CodeCheck((wchar_t) i);
        return true;
    }

    static inline bool IsNewline(wchar_t c) // \r characters are replaced by \x0a in Stream::ProcessInput().
    {
        return c == '\x0a';
    }

    static inline bool IsSpaceButNotNewline(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] == SPACE_CODE;
    }

    static inline bool IsSpace(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] <= SPACE_CODE;
    }

    static inline bool IsDigit(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] == DIGIT_CODE;
    }

    static inline bool IsUpper(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] == UPPER_CODE;
    }

    static inline bool IsLower(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] == LOWER_CODE;
    }

    static inline bool IsAlpha(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] >= LOWER_CODE;
    }

    static inline bool IsAlnum(wchar_t c)
    {
        return base[c >> LOG_COMPLEMENT_SIZE][c] >= DIGIT_CODE;
    }


};

#endif // code_INCLUDED

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

