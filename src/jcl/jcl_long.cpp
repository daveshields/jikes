// $Id: jcl_long.cpp,v 1.2 1999/12/09 18:02:26 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include <iostream.h>
#include "jcl_long.h"

void BaseLong::divide(BaseLong dividend, BaseLong divisor, BaseLong &quotient, BaseLong &remainder)
{
    u4 high = dividend.high_word(),
       low  = dividend.low_word(),
       remainder_high = 0;

    int i;
    for (i = 0; i < 32; i++)
    {
        remainder_high = (remainder_high << 1) | (high >> 31);
        high <<= 1;
        if ((ULong) divisor <= remainder_high)
        {
            high++;
            remainder_high -= divisor.low_word();
        }
    }

    remainder = BaseLong(0, remainder_high);

    for (i = 0; i < 32; i++)
    {
        remainder <<= 1;
        remainder.low_word() |= (low >> 31);
        low <<= 1;
        if ((ULong) divisor <= remainder)
        {
            low++;
            remainder -= divisor;
        }
    }

    quotient = BaseLong(high, low);

    return;
}


void ULong::String(char *result)
{
    ULong value = *this;
    char *ptr = result;

    do
    {
        *ptr++ = '0' + (value % 10).low_word();
        value /= 10;
    } while (value != 0);

    *ptr = '\0';

    for (char *tail = ptr - 1; tail > result; tail--, result++)
    {
        char c = *tail;
        *tail = *result;
        *result = c;
    }

    return;
}


void Long::String(char *result)
{
    ULong value;

    if (high_word() & 0x80000000)
    {
        *result++ = '-';
        value = -(*this);
    }
    else value = *this;

    value.String(result);

    return;
}
