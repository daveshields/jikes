// $Id: jcl_unicode.h,v 1.2 1999/12/09 18:02:26 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_unicode_INCLUDED
#define jcl_unicode_INCLUDED

#include <stdio.h>
#include <iostream.h>
#include <ctype.h>
#include <wchar.h>

//TODO: remove this
#define cerr cout

class Unicode
{
public:

    static inline int Getc(FILE *srcfile)
    {
        int ch = getc(srcfile);

        if (ch == L'\\')
        {
            int c = getc(srcfile);

            if (c == 'u')
            {
                while (c == 'u')
                    c = getc(srcfile);
                ch = 0;
                int i;
                for (i = 0; isxdigit(c) && i < 4; i++)
                {
                    int multiplier[4] = {4096, 256, 16, 1};

                    switch(c)
                    {
                        case 'a': case 'A':
                            ch += (10 * multiplier[i]);
                            break;
                        case 'b': case 'B':
                            ch += (11 * multiplier[i]);
                            break;
                        case 'c': case 'C':
                            ch += (12 * multiplier[i]);
                            break;
                        case 'd': case 'D':
                            ch += (13 * multiplier[i]);
                            break;
                        case 'e': case 'E':
                            ch += (14 * multiplier[i]);
                            break;
                        case 'f': case 'F':
                            ch += (15 * multiplier[i]);
                            break;
                        default:
                            ch += ((c - '0') * multiplier[i]);
                    }
                    c = getc(srcfile);
                }
                if (i != 4)
                    /* ISSUE AN ERROR ABOUT BAD UNICODE CHARACTER !!! */ ;
            }
    
            ungetc(c, srcfile);
        }
        else if (ch == L'\r')
        {
            ch = L'\n';
            int c = getc(srcfile);
            if (c != L'\n')
                ungetc(c, srcfile);
        }

        return ch;
    }

    //
    // Read max_size Ascii characters from srcfile, convert them to unicode
    // store them in block and return the number of unicode characters read.
    //
    static inline size_t read_file(FILE *srcfile, wchar_t *block)
    {
        wchar_t *p = block;
        int c;

        while((c = Getc(srcfile)) != EOF)
            *p++ = c;

        return p - block;
    }

    //
    // This function takes as arguments a pointer to a wchar_t string
    // buffer: block,  and an integer: max_size. It attempts to read the
    // next max_size Unicode characters from the input file into block (if
    // the input file has at least that many characters left). It returns
    // an integer which indicates how many Unicode characters were actually
    // read.
    //
    static size_t read_block(FILE *srcfile, wchar_t *block, size_t max_size)
    {
        int n;
        for (n = 0; n < max_size; n++)
        {
            int ch = Getc(srcfile);
            if (ch == EOF)
                break;
            *block++ = ch;
        }
    
        return n;
    }

    static inline void Cout(wchar_t ch)
    {
        cout << (ch > 0 && ch < 0x00C0 ? (char) ch : ' ');
    }

    static inline void Cout(wchar_t *str)
    {
        for (; *str; str++)
            Cout(*str);
    }

    static inline void Cerr(wchar_t ch)
    {
        cerr << (ch > 0 && ch < 0x00C0 ? (char) ch : ' ');
    }

    static inline void Cerr(wchar_t *str)
    {
        for (; *str; str++)
            Cerr(*str);
    }
};

#endif
