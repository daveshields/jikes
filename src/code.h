// $Id: code.h,v 1.20 2002/03/06 17:12:24 ericb Exp $ -*- c++ -*-
// DO NOT MODIFY THIS FILE - it is generated using gencode.java.
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1999, 2000, 2001, 2002, International Business
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
    // 8 categories using the array CODE. These are described below
    // together with some self-explanatory functions defined on CODE.
    //
    enum {
        SHIFT = 6,
        NEWLINE_CODE = 0,
        SPACE_CODE = 1,
        BAD_CODE = 2,
        DIGIT_CODE = 3,
        OTHER_DIGIT_CODE = 4,
        LOWER_CODE = 5,
        UPPER_CODE = 6,
        OTHER_LETTER_CODE = 7
    };

    static char codes[4485];
    static u2 blocks[1024];


public:

    static inline void SetBadCode(wchar_t c)
    {
        codes[(u2) (blocks[c >> SHIFT] + c)] = BAD_CODE;
    }

    static inline void CodeCheck(wchar_t c)
    {
        assert((u2) (blocks[c >> SHIFT] + c) < 4485);
    }

    static inline bool CodeCheck(void)
    {
        for (int i = 0; i <= 0xffff; i++)
            CodeCheck((wchar_t) i);
        return true;
    }

    //
    // \r characters are replaced by \x0a in Stream::ProcessInput().
    //
    static inline bool IsNewline(wchar_t c)
    {
        return c == '\x0a';
    }

    static inline bool IsSpaceButNotNewline(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] == SPACE_CODE;
    }

    static inline bool IsSpace(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] <= SPACE_CODE;
    }

    static inline bool IsDigit(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] == DIGIT_CODE;
    }

    static inline bool IsOctalDigit(wchar_t c)
    {
        return c >= U_0 && c <= U_7;
    }

    static inline bool IsHexDigit(wchar_t c)
    {
        return c <= U_f && (c >= U_a ||
                            (c >= U_A && c <= U_F) ||
                            (c >= U_0 && c <= U_9));
    }

    static inline bool IsUpper(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] == UPPER_CODE;
    }

    static inline bool IsLower(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] == LOWER_CODE;
    }

    static inline bool IsAlpha(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] >= LOWER_CODE;
    }

    static inline bool IsAlnum(wchar_t c)
    {
        return codes[(u2) (blocks[c >> SHIFT] + c)] >= DIGIT_CODE;
    }

};

#endif // code_INCLUDED

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

