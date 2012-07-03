// $Id: code.h,v 1.9 1999/10/11 23:19:05 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef code_INCLUDED
#define code_INCLUDED

#include "config.h"
#include <ctype.h>
#include <assert.h>

#ifdef HAVE_LIB_ICU_UC
# include <uchar.h>
#endif

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

    static char code[39424];
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
         assert(base[c >> LOG_COMPLEMENT_SIZE] + c < (&code[39424]));
    }

    static inline bool IsNewline(wchar_t c) // \r characters are replaced by \x0a in read_input.
    {
        return c == '\x0a';
    }

    static inline bool IsSpaceButNotNewline(wchar_t c)
    {
        // Since it is used only for Java Syntax parsing,
        // we use "WhiteSpace" definition from JLS (section 3.6),
        // not Unicode specific.
        // (lord)
        return c==' ' || c=='\t' || c=='\f';
    }

    // This method name is little mesleading.
    // In fact, this method checks for whitespace,
    // including newline
    // (lord)
    static inline bool IsSpace(wchar_t c)
    {
        // Since it is used only for Java Syntax parsing,
        // we use "WhiteSpace" definition from JLS (section 3.6),
        // not Unicode specific.
        // (lord)
        return c==' ' || c=='\t' || c=='\f' || IsNewline(c);
    }

    static inline bool IsDigit(wchar_t c)
    {
        // Since it is used only for Java Syntax parsing,
        // we use "Digit" definition from JLS (section 3.10.1),
        // not Unicode specific.
        // (lord)
        return c>='0' && c <='9';
    }

    static inline bool IsUpper(wchar_t c)
    {
#ifdef HAVE_LIB_ICU_UC
        return u_isupper(c);
#else
        return base[c >> LOG_COMPLEMENT_SIZE][c] == UPPER_CODE;
#endif        
    }

    static inline bool IsLower(wchar_t c)
    {
#ifdef HAVE_LIB_ICU_UC
        return u_islower(c);
#else
        return base[c >> LOG_COMPLEMENT_SIZE][c] == LOWER_CODE;
#endif        
    }

    static inline bool IsAlpha(wchar_t c)
    {
#ifdef HAVE_LIB_ICU_UC
        return u_isalpha(c) || c=='_' || c=='$';
#else
        return base[c >> LOG_COMPLEMENT_SIZE][c] >= LOWER_CODE;
#endif        
    }

    static inline bool IsAlnum(wchar_t c)
    {
#ifdef HAVE_LIB_ICU_UC
        return IsAlpha(c)|IsDigit(c);
#else
        return base[c >> LOG_COMPLEMENT_SIZE][c] >= DIGIT_CODE;
#endif        
    }

};

#endif
