// $Id: long.cpp,v 1.8 1999/10/15 02:30:41 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include <iostream.h>
#include "long.h"
#include "double.h"
//
// Note that the minimum long value, (0x80000000, 0x00000000), can be represented
// exactly in a double field. However, the maximum long value, (0x7FFFFFFF, 0xFFFFFFFF)
// cannot. To test if the double quantity "a" fits in a long range we will test whether
// or not:
//         (a >= min_long) && (-a > min_long)
//
BaseLong::operator LongInt()
{
    return LongInt(HighWord(), LowWord());
}

BaseLong::operator ULongInt()
{
    return ULongInt(HighWord(), LowWord());
}

bool BaseLong::operator== (BaseLong op)
{
    return ((HighWord() == op.HighWord()) && (LowWord() == op.LowWord()));
}

bool BaseLong::operator!= (BaseLong op)
{
    return ((HighWord() != op.HighWord()) || (LowWord() != op.LowWord()));
}

bool BaseLong::operator!()
{
    return (*this == 0);
}

BaseLong BaseLong::operator~()
{
    return BaseLong(~HighWord(), ~LowWord());
}

BaseLong BaseLong::operator^ (BaseLong op)
{
    return BaseLong(HighWord() ^ op.HighWord(), LowWord() ^ op.LowWord());
}

BaseLong& BaseLong::operator^= (BaseLong op)
{
    *this = *this ^ op;
    return *this;
}

BaseLong BaseLong::operator| (BaseLong op)
{
    return BaseLong(HighWord() | op.HighWord(), LowWord() | op.LowWord());
}

BaseLong& BaseLong::operator|= (BaseLong op)
{
    *this = *this | op;
    return *this;
}

BaseLong BaseLong::operator& (BaseLong op)
{
    return BaseLong(HighWord() & op.HighWord(), LowWord() & op.LowWord());
}

BaseLong& BaseLong::operator&= (BaseLong op)
{
    *this = *this & op;
    return *this;
}

bool BaseLong::operator&& (BaseLong op)
{
    return (*this != 0) && (op != 0);
}

bool BaseLong::operator|| (BaseLong op)
{
    return (*this != 0) || (op != 0);
}

BaseLong BaseLong::operator<< (BaseLong op)
{
    u4 n = op.LowWord(); // Always treat this value as positive, since negative values are not allowed

    //
    // Note that this function assumes that for two 32-bit integers
    // x << y, where y = 0, is well-defined and that the result is
    // the value x. This is true in Ansi-C and C++ but not true in
    // old versions of C (See Kernighan and Ritchie).
    // Note also that in shifting a 32-bit word, if y >= 32 then the
    // result is unpredictable. On Aix, xlC will produce the result 0(good!)
    // whereas on windows the Microsoft compiler produces the value of x(very bad !).
    // That is the reason why we have the initial special check for (n == 0).
    //

    // gcc-2.95.1 compiler bug prevents use of this implementation.
    // return (n == 0 ? *this
    // 	       : n < 32
    // 		   ? BaseLong((HighWord() << n) | (LowWord() >> (32 - n)), LowWord() << n)
    // 		   : BaseLong(LowWord() << (n - 32), 0));
    
    if (n == 0)
	return *this;
    else if (n < 32)
	return BaseLong((HighWord() << n) | (LowWord() >> (32 - n)), LowWord() << n);
    else
	return BaseLong(LowWord() << (n - 32), 0);
}

BaseLong& BaseLong::operator<<= (BaseLong op)
{
    *this = *this << op;
    return *this;
}

BaseLong BaseLong::operator+ (BaseLong op)
{
    u4 ushort1 = (LowWord() & 0xFFFF) + (op.LowWord() & 0xFFFF),
       ushort2 = (ushort1 >> 16) + (LowWord() >> 16) + (op.LowWord() >> 16),
       ushort3 = (ushort2 >> 16) + (HighWord() & 0xFFFF) + (op.HighWord() & 0xFFFF),
       ushort4 = (ushort3 >> 16) + (HighWord() >> 16) + (op.HighWord() >> 16);

    return BaseLong((ushort3 & 0xFFFF) | (ushort4 << 16), (ushort1 & 0xFFFF) | (ushort2 << 16));
}

BaseLong& BaseLong::operator+= (BaseLong op)
{
    *this = *this + op;
    return *this;
}

BaseLong BaseLong::operator++ (int dummy)
{
    BaseLong temp = *this;
    *this += 1;
    return temp;
}

BaseLong BaseLong::operator++ ()
{
    *this += 1;
    return *this;
}

BaseLong BaseLong::operator- ()
{
    return ~(*this) + 1;
}

BaseLong BaseLong::operator- (BaseLong op)
{
    return *this + (-op);
}

BaseLong& BaseLong::operator-= (BaseLong op)
{
    *this = *this - op;
    return *this;
}

BaseLong BaseLong::operator-- (int dummy)
{
    BaseLong temp = *this;
    *this -= 1;
    return temp;
}

BaseLong BaseLong::operator-- ()
{
    *this -= 1;
    return *this;
}

BaseLong BaseLong::operator* (BaseLong op)
{
    u4 x0 = this -> LowWord()   & 0xFFFF,
       x1 = this -> LowWord()  >> 16,
       x2 = this -> HighWord()  & 0xFFFF,
       x3 = this -> HighWord() >> 16,

       y0 = op.LowWord()   & 0xFFFF,
       y1 = op.LowWord()  >> 16,
       y2 = op.HighWord()  & 0xFFFF,
       y3 = op.HighWord() >> 16;

    BaseLong result = BaseLong(0, x0 * y0),
             part1  = BaseLong(0, x0 * y1);
    part1  <<= (1 << 4);
    result += part1;
    BaseLong part2  = BaseLong(0, x0 * y2);
    part2  <<= (2 << 4);
    result += part2;
    BaseLong part3  = BaseLong(0, x0 * y3);
    part3  <<= (3 << 4);
    result += part3;

    BaseLong part4  = BaseLong(0, x1 * y0);
    part4  <<= (1 << 4);
    result += part4;
    BaseLong part5  = BaseLong(0, x1 * y1);
    part5  <<= (2 << 4);
    result += part5;
    BaseLong part6  = BaseLong(0, x1 * y2);
    part6  <<= (3 << 4);
    result += part6;
    BaseLong part7  = BaseLong(0, x1 * y3);
    part7  <<= (4 << 4);
    result += part7;

    BaseLong part8  = BaseLong(0, x2 * y0);
    part8  <<= (2 << 4);
    result += part8;
    BaseLong part9  = BaseLong(0, x2 * y1);
    part9  <<= (3 << 4);
    result += part9;
    BaseLong part10 = BaseLong(0, x2 * y2);
    part10 <<= (4 << 4);
    result += part10;
    BaseLong part11 = BaseLong(0, x2 * y3);
    part11 <<= (5 << 4);
    result += part11;

    BaseLong part12 = BaseLong(0, x3 * y0);
    part12 <<= (3 << 4);
    result += part12;
    BaseLong part13 = BaseLong(0, x3 * y1);
    part13 <<= (4 << 4);
    result += part13;
    BaseLong part14 = BaseLong(0, x3 * y2);
    part14 <<= (5 << 4);
    result += part14;
    BaseLong part15 = BaseLong(0, x3 * y3);
    part15 <<= (6 << 4);
    result += part15;

    return result;
}

BaseLong& BaseLong::operator*= (BaseLong op)
{
    *this = *this * op;
    return *this;
}


BaseLong::BaseLong(u4 a, u4 b)
{
    HighWord() = a;
    LowWord() = b;
}

BaseLong::BaseLong(u4 a)
{
    HighWord() = 0;
    LowWord() = a;
}

BaseLong::BaseLong(i4 a)
{
    LowWord() = a;
    //
    // Since the carry is not guaranteed to ripple, we cannot use this code.
    //
    //        HighWord() = a >> 31;
    //
    HighWord() = (a < 0 ? 0xFFFFFFFF : 0x00000000);
}


void BaseLong::Divide(BaseLong dividend, BaseLong divisor, BaseLong &quotient, BaseLong &remainder)
{
    u4 high = dividend.HighWord(),
       low  = dividend.LowWord(),
       remainder_high = 0;

    for (int i = 0; i < 32; i++)
    {
        remainder_high = (remainder_high << 1) | (high >> 31);
        high <<= 1;
        if ((ULongInt) divisor <= remainder_high)
        {
            high++;
            remainder_high -= divisor.LowWord();
        }
    }

    remainder = BaseLong(0, remainder_high);

    for (int j = 0; j < 32; j++)
    {
        remainder <<= 1;
        remainder.LowWord() |= (low >> 31);
        low <<= 1;
        if ((ULongInt) divisor <= remainder)
        {
            low++;
            remainder -= divisor;
        }
    }

    quotient = BaseLong(high, low);

    return;
}


void ULongInt::OctString(char *result, bool show_base)
{
    ULongInt val = *this;
    char *ptr = result;

    do
    {
        *ptr++ = '0' + (val % 8).LowWord();
        val /= 8;
    } while (val != 0);

    if (show_base)
        *ptr++ = '0';

    *ptr = U_NULL;

    for (char *tail = ptr - 1; tail > result; tail--, result++)
    {
        char c = *tail;
        *tail = *result;
        *result = c;
    }

    return;
}


void ULongInt::DecString(char *result)
{
    ULongInt val = *this;
    char *ptr = result;

    do
    {
        *ptr++ = '0' + (val % 10).LowWord();
        val /= 10;
    } while (val != 0);

    *ptr = U_NULL;

    for (char *tail = ptr - 1; tail > result; tail--, result++)
    {
        char c = *tail;
        *tail = *result;
        *result = c;
    }

    return;
}


void ULongInt::HexString(char *result, bool show_base)
{
    ULongInt val = *this;
    char *ptr = result;

    do
    {
        *ptr++ = '0' + (val % 16).LowWord();
        val /= 16;
    } while (val != 0);

    if (show_base)
    {
        *ptr++ = 'x';
        *ptr++ = '0';
    }

    *ptr = U_NULL;

    for (char *tail = ptr - 1; tail > result; tail--, result++)
    {
        char c = *tail;
        *tail = *result;
        *result = c;
    }

    return;
}


void LongInt::OctString(char *result, bool show_base)
{
    ULongInt val;

    if (HighWord() & 0x80000000)
    {
        *result++ = '-';
        val = -(*this);
    }
    else val = *this;

    val.OctString(result, show_base);

    return;
}

void LongInt::DecString(char *result)
{
    ULongInt val;

    if (HighWord() & 0x80000000)
    {
        *result++ = '-';
        val = -(*this);
    }
    else val = *this;

    val.DecString(result);

    return;
}

void LongInt::HexString(char *result, bool show_base)
{
    ULongInt val;

    if (HighWord() & 0x80000000)
    {
        *result++ = '-';
        val = -(*this);
    }
    else val = *this;

    val.HexString(result, show_base);

    return;
}

ULongInt& ULongInt::operator/= (ULongInt op)
{
    *this = *this / op;
    return *this;
}


ULongInt ULongInt::operator/ (ULongInt op)
{
    BaseLong quotient,
             remainder;

    Divide(*this, op, quotient, remainder);

    return quotient;
}

ULongInt ULongInt::operator% (ULongInt op)
{
    BaseLong quotient,
             remainder;

    Divide(*this, op, quotient, remainder);

    return remainder;
}

ULongInt& ULongInt::operator%= (ULongInt op)
{
    *this = *this % op;
    return *this;
}


ULongInt ULongInt::operator>> (ULongInt op)
{
    u4 n = op.LowWord(); // Always treat this value as positive, since negative values are not allowed

    //
    // Note that this function assumes that for two 32-bit integers
    // x >> y, where y = 0, is well-defined and that the result is
    // the value x. This is true in Ansi-C and C++ but not true in
    // old versions of C (See Kernighan and Ritchie).
    // Note also that in shifting a 32-bit word, if y >= 32 then the
    // result is unpredictable. On Aix, xlC will produce the result 0(good!)
    // whereas on windows the Microsoft compiler produces the value of x(very bad !).
    // That is the reason why we have the initial special check for (n == 0).
    //

    // gcc-2.95.1 compiler bug prevents this implementation
    // return (n == 0 ? *this
    // 	       : n < 32
    // 		   ? ULongInt(HighWord() >> n, (HighWord() << (32 - n)) | (LowWord() >> n))
    // 		   : ULongInt(0, HighWord() >> (n - 32)));

    if (n == 0)
	return *this;
    else if (n < 32)
	return ULongInt(HighWord() >> n, (HighWord() << (32 - n)) | (LowWord() >> n));
    else
	return ULongInt(0, HighWord() >> (n - 32));

}

ULongInt& ULongInt::operator>>= (ULongInt op)
{
    *this = *this >> op;
    return *this;
}

bool ULongInt::operator< (ULongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() < op.LowWord() : HighWord() < op.HighWord());
}

bool ULongInt::operator<= (ULongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() <= op.LowWord() : HighWord() <= op.HighWord());
}

bool ULongInt::operator> (ULongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() > op.LowWord() : HighWord() > op.HighWord());
}

bool ULongInt::operator>= (ULongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() >= op.LowWord() : HighWord() >= op.HighWord());
}

// This conversion from double to LongInt is performed according to the rules
// specified in the Java Language Specification.
//
LongInt::LongInt(IEEEfloat a) : BaseLong(0,0)
{
    IEEEdouble value = IEEEdouble(a);
    LongInt lvalue = LongInt(value);
    HighWord() = lvalue.HighWord();
    LowWord() = lvalue.LowWord();
    return;
}

LongInt::LongInt(IEEEdouble a) : BaseLong (0,0)
{
    if (a.HighWord() == 0x7fffffff && a.LowWord() == 0xffffffff) // if NaN
        ; // *this is already initialized to 0
    else if (a.HighWord() == 0xfff00000 && a.LowWord() == 0x00000000) // if NEGATIVE_INFINITY())
        HighWord() = 0x80000000;
    else if (a.HighWord() == 0x7ff00000 && a.LowWord() == 0x00000000) // if POSITIVE_INFINITY())
    {
        HighWord() = 0x7FFFFFFF;
        LowWord()  = 0xFFFFFFFF;
    }
    else
    {
        double b = floor(a.DoubleValue() < 0.0 ? -a.DoubleValue() : a.DoubleValue()); // DSDouble

        if (b < IEEEdouble::min_long.DoubleValue())
            HighWord() = 0x80000000;
        else if (-b <= IEEEdouble::min_long.DoubleValue())
        {
            HighWord() = 0x7FFFFFFF;
            LowWord()  = 0xFFFFFFFF;
        }
        else
        {
            LongInt multiplier = 1;

            while (b > 0.0)
            {
                *this += (multiplier * (int) fmod(b, 10));
                b /=  10.0;
                multiplier *= 10;
            }

            if (a < 0.0)
                *this = -(*this);
        }
    }

    return;
}

LongInt LongInt::operator/ (LongInt op)
{
    bool negative_dividend = ((HighWord() & 0x80000000) != 0),
         negative_divisor  = ((op.HighWord() & 0x80000000) != 0);

    BaseLong a = (negative_dividend ? -(*this) : (BaseLong) *this),
             b = (negative_divisor  ? -(op)    : (BaseLong) op),
             quotient,
             remainder;

    Divide(a, b, quotient, remainder);

    return (negative_dividend ^ negative_divisor ? -quotient : quotient);
}

LongInt& LongInt::operator/= (LongInt op)
{
    *this = *this / op;
    return *this;
}

LongInt LongInt::operator% (LongInt op)
{
    bool negative_dividend = ((HighWord() & 0x80000000) != 0),
    negative_divisor  = ((op.HighWord() & 0x80000000) != 0);

    BaseLong a = (negative_dividend ? -(*this) : (BaseLong) *this),
             b = (negative_divisor  ? -(op)    : (BaseLong) op),
             quotient,
             remainder;

    Divide(a, b, quotient, remainder);

    return (negative_dividend ? -remainder : remainder);
}

LongInt& LongInt::operator%= (LongInt op)
{
    *this = *this % op;
    return *this;
}

LongInt LongInt::operator>> (LongInt op)
{
    u4 n = op.LowWord(); // Always treat this value as positive, since negative values are not allowed

    //
    // Note that this function assumes that for two 32-bit integers
    // x >> y, where y = 0, is well-defined and that the result is
    // the value x. This is true in Ansi-C and C++ but not true in
    // old versions of C (See Kernighan and Ritchie).
    //
    // Note also that in shifting a 32-bit word, if y >= 32 then the
    // result is unpredictable. On Aix, xlC will produce the result 0(good!)
    // whereas on windows the Microsoft compiler produces the value of x(very bad !).
    // That is the reason why we have the initial special check for (n == 0).
    //
    // Finally, note that the right-shitfting of the HighWord is not guaranteed
    // to ripple the carry bit. Whether or not the carry-bit is rippled is
    // implementation-dependent. Therefore, this implementation is designed to
    // shift the "long" quantity in a similar manner as the system (compiler + environement)
    // used to compile it would shift a 32-bit signed integer.
    //

    // gcc-2.95.1 compiler bug prevents use of this implementation
    // return (n == 0 ? *this
    // 	       : n < 32
    // 		   ? LongInt(((i4) HighWord()) >> n, (HighWord() << (32 - n)) | (LowWord() >> n))
    // 		   : LongInt(((i4) HighWord()) >> 31, ((i4) HighWord()) >> (n - 32)));

    if (n == 0)
	return *this;
    else if (n < 32)
	return LongInt(((i4) HighWord()) >> n, (HighWord() << (32 - n)) | (LowWord() >> n));
    else 
	return LongInt(((i4) HighWord()) >> 31, ((i4) HighWord()) >> (n - 32));
}

LongInt& LongInt::operator>>= (LongInt op)
{
    *this = *this >> op;
    return *this;
}

bool LongInt::operator< (LongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() < op.LowWord() : (i4) HighWord() < (i4) op.HighWord());
}

bool LongInt::operator<= (LongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() <= op.LowWord() : (i4) HighWord() <= (i4) op.HighWord());
}

bool LongInt::operator> (LongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() > op.LowWord() : (i4) HighWord() > (i4) op.HighWord());
}

bool LongInt::operator>= (LongInt op)
{
    return (HighWord() == op.HighWord() ? LowWord() >= op.LowWord() : (i4) HighWord() >= (i4) op.HighWord());
}

double ULongInt::Double()
{
    double val = 0.0,
           multiplier = 1.0;
    ULongInt num = *this;

    while (num > 0)
    {
        val += (multiplier * (num % 10).LowWord());
        num /= 10;
        multiplier *= 10.0;
    }

    return val;
}

double LongInt::Double()
{
    double val;
    ULongInt num;

    if (*this < 0)
    {
        num = -(*this);
        val = -num.Double();
    }
    else
    {
        num = *this;
        val = num.Double();
    }

    return val;
}

