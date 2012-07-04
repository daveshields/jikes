// $Id: double.cpp,v 1.17 2001/02/14 21:25:11 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
//
// NOTE: The IEEE 754 emulation code in double.h and double.cpp within
// Jikes are adapted from code written by Alan M. Webb of IBM's Hursley
// lab in porting the Sun JDK to System/390.
//
// In addition, the code for emulating the remainder operator, %, is
// adapted from e_fmod.c, part of fdlibm, the Freely Distributable Math
// Library mentioned in the documentation of java.lang.StrictMath.  The
// original library is available at http://netlib2.cs.utk.edu/fdlibm.
//
// The code from fdlibm is copyrighted, as follows:
// ====================================================
// Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
//
// Developed at SunSoft, a Sun Microsystems, Inc. business.
// Permission to use, copy, modify, and distribute this
// software is freely granted, provided that this notice 
// is preserved.
// ====================================================
//
//

#include "double.h"
#include "long.h"

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

#ifndef HAVE_MEMBER_CONSTANTS
// VC++ can't cope with constant class members
u4 IEEEfloat::MAX_FRACT   = 0x01000000;
u4 IEEEfloat::MAX_FRACT2  = 0x00800000;
u4 IEEEfloat::MIN_INT_F   = 0xCF000000;
i4 IEEEfloat::MIN_INT     = 0x80000000;
i4 IEEEfloat::MAX_INT     = 0x7FFFFFFF;

u4 IEEEdouble::MAX_FRACT  = 0x00200000;
u4 IEEEdouble::MAX_FRACT2 = 0x00100000;
i4 IEEEdouble::MIN_INT    = 0x80000000;
i4 IEEEdouble::MAX_INT    = 0x7FFFFFFF;
#else
// gcc bug 1877 can cause linker errors if
// the following external decls are not used.
const u4 IEEEfloat::MAX_FRACT;
const u4 IEEEfloat::MAX_FRACT2;
const u4 IEEEfloat::MIN_INT_F;
const i4 IEEEfloat::MIN_INT;
const i4 IEEEfloat::MAX_INT;

const u4 IEEEdouble::MAX_FRACT;
const u4 IEEEdouble::MAX_FRACT2;
const i4 IEEEdouble::MIN_INT;
const i4 IEEEdouble::MAX_INT;
#endif


IEEEfloat::IEEEfloat(i4 a)
{
#ifdef HAVE_IEEE754
    value.float_value = (float) a;
#else
    int	sign = 0;

    if (a < 0)
    {
        a  = -a; // even works for MIN_INT!
        sign = 1;
    }
    if (a == 0)
        *this = POSITIVE_ZERO();
    else
        *this = Normalize(sign, FRACT_BITS, (u4) a);
#endif // HAVE_IEEE754
}

IEEEfloat::IEEEfloat(LongInt a)
{
#ifdef HAVE_IEEE754
# ifdef HAVE_UNSIGNED_LONG_LONG
    value.float_value = (float) a.Words();
# else
    value.float_value = ((float) a.HighWord() * (float) 0x40000000 * 4.0f) +
        (float) a.LowWord();
# endif // HAVE_UNSIGNED_LONG_LONG
#else
    //
    // Unfortunately, we cannot recycle the LongInt.DoubleValue() method, since
    // in rare cases the double rounding puts us off by one bit.
    //
    int	sign = 0;

    if (a < 0)
    {
        a  = -a;
        sign = 1;
        if (a < 0) // special case MIN_LONG
        {
            value.word = 0xDF000000;
            return;
        }
    }
    if (a == 0)
        *this = POSITIVE_ZERO();
    else if (a.HighWord() == 0)
        *this = Normalize(sign, FRACT_BITS, a.LowWord());
    else
    {
        int exponent = FRACT_BITS, round = 0;
        while (a.HighWord())
        {
            round |= (a.LowWord() & 0x000000ff) ? 1 : 0;
            a >>= 8;
            exponent += 8;
        }
        *this = Normalize(sign, exponent, a.LowWord() | round);
    }
#endif // HAVE_IEEE754
}

IEEEfloat::IEEEfloat(IEEEdouble d)
{
#ifdef HAVE_IEEE754
    value.float_value = (float) d.DoubleView();
#else
    // Either true zero, denormalized, or too small
    if (d.Exponent() < -BIAS - 30)
        *this = (d.IsPositive() ? POSITIVE_ZERO() : NEGATIVE_ZERO());
    else
    {
        if (d.IsPositiveInfinity())
            *this = POSITIVE_INFINITY();
        else if (d.IsNegativeInfinity())
            *this = NEGATIVE_INFINITY();
        else if (d.IsNaN())
            *this = NaN();
        else 
	{
            //
            // A regular, normalized number - do work on the parts
            // Shift to 26th position, add implicit msb, rounding bits
            //
            LongInt fract = d.Fraction() << 5;
	    u4 fraction = fract.HighWord() | ((fract.LowWord()) ? 0x02000001
                                                                : 0x02000000);

	    *this = Normalize(d.Sign(), d.Exponent() - 2, fraction);
	}
    }
#endif // HAVE_IEEE754
}

IEEEfloat::IEEEfloat(char *str, bool check_invalid)
{
    //
    // This assumes that name already meets the format of JLS 3.10.2 (ie. no
    // extra whitespace or invalid characters)
    //
    // TODO: This conversion is a temporary patch. Need volunteer to implement
    //       Clinger algorithm from PLDI 1990.
    //
    value.float_value = (float) atof(str);

    //
    // When parsing a literal in Java, a number that rounds to infinity or
    // zero is invalid.  A true check_invalid sets any invalid number to NaN,
    // to make the upstream processing easier.  Leave check_invalid false to
    // allow parsing infinity or rounded zero from a string.
    //
    if (check_invalid)
    {
        if (IsInfinite())
            *this = NaN();
        if (IsZero())
        {
            for (char *p = str; *p; p++) {
                switch (*p) {
                case '-': case '+': case '0': case '.':
                    break; // keep checking
                case 'e': case 'E': case 'd': case 'D': case 'f': case 'F':
                    return; // got this far, str is 0
                default:
                    *this = NaN(); // encountered non-zero digit. oops.
                    return;
                }
            }
        }
    }
}

i4 IEEEfloat::IntValue()
{
    if (IsNaN())
        return 0;
            
    int	sign = Sign(),
        exponent = Exponent();

    if (IsInfinite())
        return sign ? MIN_INT : MAX_INT;

    // This covers true zero and denorms.
    if (exponent < 0)
        return 0;

    i4 result = Fraction();

    if (exponent > FRACT_BITS)
	result <<= (exponent - FRACT_BITS);
    else if (exponent < FRACT_BITS)
	result >>= (FRACT_BITS - exponent);

    return sign ? -result : result;
}

LongInt IEEEfloat::LongValue()
{
    if (IsNaN())
        return LongInt(0);
            
    int	sign = Sign(),
        exponent = Exponent();

    if (IsInfinite())
        return sign ? LongInt::MIN_LONG() : LongInt::MAX_LONG();

    // This covers true zero and denorms.
    if (exponent < 0)
        return LongInt(0);

    LongInt result(Fraction());

    if (exponent > FRACT_BITS)
	result <<= (exponent - FRACT_BITS);
    else if (exponent < FRACT_BITS)
	result >>= (FRACT_BITS - exponent);

    return sign ? (LongInt) -result : result;
}

IEEEfloat IEEEfloat::Normalize(int sign, int exponent, u4 fraction)
{
    bool round = false, sticky = false;

    assert(fraction);

    //
    // Normalize right. MAX_FRACT is (FLT_MAX<<1)-FLT_MAX.
    // So we need to shift on equal.
    //
    if (fraction >= MAX_FRACT)
    {
	while (fraction >= MAX_FRACT)
	{
	    sticky |= round;
	    round = (fraction & 0x00000001) != 0;
	    fraction >>= 1;
	    exponent++;
	}
	if (round && (sticky || (fraction & 0x00000001)) && exponent > -BIAS)
            //
            // Capture any overflow caused by rounding. No other checks are
            // required because if overflow occurred, the the low order bit
            // was guaranteed to be zero. Do not round denorms yet.
            //
	    if (++fraction >= MAX_FRACT)
	    {
		fraction >>= 1;
		exponent++;
	    }
    }

    // Normalize left. MAX_FRACT2 is MAX_FRACT >> 1.
    else
	while (fraction < MAX_FRACT2)
	{
	    fraction <<= 1;
	    exponent--;
	}

    //
    // Check and respond to overflow
    //
    if (exponent > BIAS)
        return sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    //
    // Check and respond to underflow 
    //
    if (exponent <= -BIAS)
    {
	while (exponent <= -BIAS)
	{
	    sticky |= round;
	    round = (fraction & 0x00000001) != 0;
	    fraction >>= 1;
	    exponent++;
	}

	if (round && (sticky || (fraction & 0x00000001)))
	    fraction++;

	exponent = -BIAS;

	if (fraction == 0)
            return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
    }

    fraction &= 0x007FFFFF;
    fraction |= ((exponent + BIAS) << FRACT_BITS);
    if (sign)
        fraction |= 0x80000000;
    return IEEEfloat(fraction);
}

int IEEEfloat::SplitInto(u4 &fraction)
{
    int exponent = Exponent();
    fraction = Fraction();

    if (exponent == -BIAS)
    {
	exponent++;
	while (fraction < MAX_FRACT2)
	{
	    fraction <<= 1;
	    exponent--;
	}
    }

    return exponent;
}

bool IEEEfloat::operator== (IEEEfloat op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return value.float_value == op.value.float_value;
#else
    return IsNaN() || op.IsNaN() ? false
                                 : IsZero() && op.IsZero() ? true
                                                           : value.word == op.value.word;
#endif
}

bool IEEEfloat::operator!= (IEEEfloat op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return value.float_value != op.value.float_value;
#else
    return !(*this == op);
#endif
}

bool IEEEfloat::operator< (IEEEfloat op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.float_value < op.value.float_value);
#else
    if (IsNaN() || op.IsNaN())
        return false; // NaNs are unordered
    if (IsZero())
        return op.IsZero() ? false : op.IsPositive();
    if (op.IsZero())
        return IsNegative();
    // Exploit fact that remaining IEEE floating point sort as signed ints
    return value.iword < op.value.iword;
#endif
}

bool IEEEfloat::operator<= (IEEEfloat op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.float_value <= op.value.float_value);
#else
    return *this < op || *this == op;
#endif
}

bool IEEEfloat::operator> (IEEEfloat op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.float_value > op.value.float_value);
#else
    if (IsNaN() || op.IsNaN())
        return false; // NaNs are unordered.
    if (IsZero())
        return op.IsZero() ? false : op.IsNegative();
    if (op.IsZero())
        return IsPositive();
    // Exploit fact that remaining IEEE floating point sort as signed ints
    return value.iword > op.value.iword;
#endif
}

bool IEEEfloat::operator>= (IEEEfloat op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.float_value >= op.value.float_value);
#else
    return *this > op || *this == op;
#endif
}

IEEEfloat IEEEfloat::operator+ (IEEEfloat op)
{
#ifdef HAVE_IEEE754
    // FIXME: This could be sped up by inlining
    return IEEEfloat(value.float_value + op.value.float_value);
#else
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    //
    // Adding unlike infinities not allowed
    // Adding infinities of same sign is infinity of that sign
    // Adding finite and infinity produces infinity
    //
    if (IsInfinite())
        return op.IsInfinite() && (Sign() != op.Sign()) ? NaN() : *this;
    if (op.IsInfinite())
        return op;

    //
    // Adding zero is easy
    //
    if (IsZero())
        return (op.IsZero()) ? (Sign() != op.Sign()) ? POSITIVE_ZERO()
                                                     : *this
                             : op;
    if (op.IsZero())
        return *this;

    //
    // Now for the real work - do manipulations on copies
    //
    i4 x, y, round = 0;
    int expx, expy, signx, signy;
    
    expx = SplitInto((u4 &) x);
    expy = op.SplitInto((u4 &) y);
    
    signx = Sign();
    signy = op.Sign();

    // If the exponents are far enough apart, the answer is easy
    if (expx > expy + 25)
        return *this;
    if (expy > expx + 25)
        return op;

    //
    // Denormalize the fractions, so that the exponents are
    // the same and then set the exponent for the result.
    // Leave enough space for overflow and INT_MIN avoidance!
    // 
    if (signx)
        x = -x;
    if (signy)
        y = -y;

    x <<= 6;
    y <<= 6;

    if (expx > expy) {
        round = y << (32 + expy - expx);
        y >>= expx - expy;
    }
    else if (expy > expx)
    {
        round = x << (32 + expx - expy);
        x >>= expy - expx;
        expx = expy;
    }

    //
    // Do the arithmetic. The excess magnitude of 32-bit arithmetic means
    // overflow is impossible (we only need 1 spare bit!). We ensure that
    // pre-alignment avoids any question of INT_MIN negation problems.
    //
    x += y;
    if (round)
        x |= 1;

    //
    // If the result is negative, then make the fraction positive again
    // and remember the sign.
    //
    if (x < 0)
    {
        x = -x;
        signx = 1;
    }
    else
        signx = 0;

    if (x == 0)
        return signx ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    //
    // Time to normalize again! If we need to shift left (the addition was
    // effectively a subtraction), then there cannot be any reason to round.
    // If the number fits exactly we don't have anything to do either.
    //
    return Normalize(signx, expx - 6, (u4) x);
#endif
}

IEEEfloat IEEEfloat::operator- ()
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return IEEEfloat(-value.float_value);
#else
    if (IsNaN())
        return *this;
    return IEEEfloat(value.word ^ 0x80000000);
#endif
}

IEEEfloat IEEEfloat::operator* (IEEEfloat op)
{
#ifdef HAVE_IEEE754
    return IEEEfloat(value.float_value * op.value.float_value);
#else
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    int sign = Sign() ^ op.Sign();

    //
    // If either operand is zero or infinite, then the answer is easy.
    //
    if (IsZero())
        return op.IsInfinite() ? NaN()
                               : sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (op.IsZero())
        return IsInfinite() ? NaN()
                            : sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (IsInfinite() || op.IsInfinite())
        return sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    //
    // Now for the real work - do manipulations on copies
    //
    u4 x, y;
    int exponent;

    exponent = SplitInto(x) + op.SplitInto(y);

    //
    // The numbers to be multiplied are 24 bits in length (stored in 32 bit
    // integers). Using ULongInt to perform the multiplication, the result
    // will be 46-48 bits (unsigned); shift it back to 28 bits for Normalize,
    // while folding the low 20 bits into the lsb for rounding purposes.
    //

    ULongInt a = x,
             b = y;
    a *= b;
    b = a & 0x000FFFFF;
    a >>= 20;
    x = a.LowWord() | ((b > 0) ? 1 : 0);
    
    return Normalize(sign, exponent - 3, x);
#endif // HAVE_IEEE754
}

IEEEfloat IEEEfloat::operator/ (IEEEfloat op)
{
#ifdef HAVE_IEEE754
    return op.IsZero() ? ((IsNaN() || IsZero()) ? NaN()
                               : (IsPositive() ^ op.IsPositive())
                                       ? NEGATIVE_INFINITY()
                                       : POSITIVE_INFINITY())
                       : IEEEfloat(value.float_value / op.value.float_value);
#else // HAVE_IEEE754
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    int sign = Sign() ^ op.Sign();

    //
    // Infinities and zeroes are special.
    //

    if (IsInfinite())
        return op.IsInfinite() ? NaN()
                               : sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    if (op.IsInfinite())
        return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (IsZero())
        return op.IsZero() ? NaN()
                           : sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (op.IsZero())
        return sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    //
    // Now for the real work - do manipulations on copies
    //
    u4 x, y;
    int exponent;

    exponent = SplitInto(x) - op.SplitInto(y);

    u4  mask   = 0x80000000,
        result = 0;

    // Because both values are normalised, a single shift guarantees results.

    if (x < y)
    {
	x <<= 1;
	exponent--;
    }

    //
    // If the numerator is larger, then it is divisible.
    // Reflect this in the result, and do the subtraction.
    // Magnify the numerator again and reduce the mask.
    //
    while (mask)
    {
	if (x >= y)
	{
	    result += mask;
	    x -= y;
	    if (x == 0)
		break;
	}
        x <<= 1;
        mask >>= 1;
    }

    return Normalize(sign, exponent - 8, result);
#endif // HAVE_IEEE754
}

IEEEfloat IEEEfloat::operator% (IEEEfloat op)
{
#ifdef HAVE_IEEE754
    return IEEEfloat((op.IsZero() ? NaN().value.float_value
                                  : (float) fmod((double) value.float_value, (double) op.value.float_value)));
#else // HAVE_IEEE754
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    //
    // Infinities and zeroes are special.
    //

    if (IsInfinite() || op.IsZero())
        return NaN();

    if (IsZero() || op.IsInfinite())
        return *this;

    //
    // Now for the real work - do manipulations on copies
    // This algorithm is from fdlibm.c - see above notice
    //

    int sign = Sign();
    u4 x, y;
    int expy, exponent;
    i4 z;

    expy = op.SplitInto(y);
    exponent = SplitInto(x) - expy;

    if (exponent < 0 || (exponent == 0 && x < y))
        return *this;

    while (exponent--)
    {
        z = x - y;
        if (z < 0)
            x <<= 1;
        else if (z == 0)
            return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
        else
            x = z + z;
    }
    z = x - y;
    if (z >= 0)
        x = z;

    if (x == 0)
        return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
    return Normalize(sign, expy, x);
#endif // HAVE_IEEE754
}



IEEEdouble::IEEEdouble(IEEEfloat f)
{
#ifdef HAVE_IEEE754
    value.double_value = (double) f.FloatView();
#else
    int sign = f.Sign();
    int exponent = f.Exponent();

    if (exponent == -127)
    {
	if (f.IsZero())
            *this = sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
	else
        {
            //
            // This is a denormalized number, with exponent -126
            // (1 - IEEEfloat::BIAS); shift it to fit double format
            //
	    *this = Normalize(sign, -126, ULongInt(f.Fraction()) << 29);
	}
    }
    else
    {
        if (f.IsInfinite())
            *this = sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();
        else if (f.IsNaN())
            *this = NaN();
        else
	{
            // Regular, noramlized number.  Shift it to fit double format
            *this = Normalize(sign, exponent, ULongInt(f.Fraction()) << 29);
	}
    }
#endif // HAVE_IEEE754
}

IEEEdouble::IEEEdouble(i4 a)
{
#ifdef HAVE_IEEE754
    value.double_value = (double) a;
#else
    int	sign = 0;

    if (a < 0)
    {
        a  = -a; // even works for MIN_INT!
        sign = 1;
    }
    if (a == 0)
        *this = POSITIVE_ZERO();
    else
        *this = Normalize(sign, FRACT_BITS, ULongInt((u4) a));
#endif // HAVE_IEEE754
}

IEEEdouble::IEEEdouble(LongInt a)
{
#ifdef HAVE_IEEE754
# ifdef HAVE_UNSIGNED_LONG_LONG
    value.double_value = (double) a.Words();
# else
    value.double_value = ((double) a.HighWord() * (double) 0x40000000 * 4.0) +
        (double) a.LowWord();
# endif // HAVE_UNSIGNED_LONG_LONG
#else
    int	sign = 0;

    if (a < 0)
    {
        a  = -a; // even works for MIN_LONG!
        sign = 1;
    }
    if (a == 0)
        *this = POSITIVE_ZERO();
    else
        *this = Normalize(sign, FRACT_BITS, ULongInt(a));
#endif // HAVE_IEEE754
}

IEEEdouble::IEEEdouble(char *str, bool check_invalid)
{
    //
    // TODO: This conversion is a temporary patch. Need volunteer to implement
    //       Clinger algorithm from PLDI 1990.
    //
    value.double_value = atof(str);

    //
    // When parsing a literal in Java, a number that rounds to infinity or
    // zero is invalid.  A true check_invalid sets any invalid number to NaN,
    // to make the upstream processing easier.  Leave check_invalid false to
    // allow parsing infinity or rounded zero from a string.
    //
    if (check_invalid)
    {
        if (IsInfinite())
            *this = NaN();
        if (IsZero())
        {
            for (char *p = str; *p; p++) {
                switch (*p) {
                case '-': case '+': case '0': case '.':
                    break; // keep checking
                case 'e': case 'E': case 'd': case 'D': case 'f': case 'F':
                    return; // got this far, str is 0
                default:
                    *this = NaN(); // encountered non-zero digit. oops.
                    return;
                }
            }
        }
    }
}

i4 IEEEdouble::IntValue()
{
    if (IsNaN())
        return 0;                                                             
            
#ifdef HAVE_IEEE754
    if (value.double_value < MIN_INT)
        return MIN_INT;
    else if (value.double_value > MAX_INT)
        return MAX_INT;
    return (i4) value.double_value;
#else
    int	sign = Sign(),
        exponent = Exponent();

    if (IsInfinite() || exponent > 30)
        return sign ? MIN_INT : MAX_INT;

    // This includes true zero and denorms.
    if (exponent < 0)
        return 0;

    i4 result = (i4) (Fraction() >> (FRACT_BITS - exponent)).LowWord();

    return sign ? -result : result;
#endif // HAVE_IEEE754
}

LongInt IEEEdouble::LongValue()
{
    if (IsNaN())
        return LongInt(0);
            
    int	sign = Sign(),
        exponent = Exponent();

    if (IsInfinite() || exponent > 62)
        return sign ? LongInt::MIN_LONG() : LongInt::MAX_LONG();

    // This covers true zero and denorms.
    if (exponent < 0)
        return LongInt(0);

    LongInt result = Fraction();

    if (exponent > FRACT_BITS)
	result <<= (exponent - FRACT_BITS);
    else if (exponent < FRACT_BITS)
	result >>= (FRACT_BITS - exponent);

    return sign ? (LongInt) -result : result;
}

IEEEdouble IEEEdouble::Normalize(int sign, int exponent, ULongInt fraction)
{
    bool round = false, sticky = false;

    assert(fraction != 0);

    //
    // Normalize right. Shift until value < MAX_FRACT.
    //
    if (fraction.HighWord() >= MAX_FRACT)
    {
	while (fraction.HighWord() >= MAX_FRACT)
	{
	    sticky |= round;
	    round = (fraction.LowWord() & 0x00000001) != 0;
	    fraction >>= 1;
	    exponent++;
	}
	if (round && (sticky || (fraction.LowWord() & 0x00000001)) && exponent > -BIAS)
            //
            // Capture any overflow caused by rounding. No other checks are
            // required because if overflow occurred, the the low order bit
            // was guaranteed to be zero. Do not round denorms yet.
            //
	    if ((++fraction).HighWord() >= MAX_FRACT)
	    {
		fraction >>= 1;
		exponent++;
	    }
    }

    // Normalize left. MAX_FRACT2 is MAX_FRACT >> 1.
    else
	while (fraction.HighWord() < MAX_FRACT2)
	{
	    fraction <<= 1;
	    exponent--;
	}

    //
    // Check and respond to overflow
    //
    if (exponent > BIAS)
        return sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    //
    // Check and respond to underflow 
    //
    if (exponent <= -BIAS)
    {
	while (exponent <= -BIAS)
	{
	    sticky |= round;
	    round = (fraction.LowWord() & 0x00000001) != 0;
	    fraction >>= 1;
	    exponent++;
	}

	if (round && (sticky || (fraction.LowWord() & 0x00000001)))
	    fraction++;

	exponent = -BIAS;

	if (fraction == 0)
            return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
    }

    return IEEEdouble((sign << 31) | ((exponent + BIAS) << 20)
                                   | (fraction.HighWord() & 0x000FFFFF),
                      fraction.LowWord());
}

int IEEEdouble::SplitInto(BaseLong &fraction)
{
    int exponent = Exponent();
    fraction = Fraction();

    if (exponent == -BIAS)
    {
	exponent++;
	while (fraction.HighWord() < MAX_FRACT2)
	{
	    fraction <<= 1;
	    exponent--;
	}
    }

    return exponent;
}

bool IEEEdouble::operator== (IEEEdouble op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    // TODO: Microsoft VC++ botches this, mixing 12.0 and NaN
    return value.double_value == op.value.double_value;
#else
    return IsNaN() || op.IsNaN() ? false
                                 : IsZero() && op.IsZero() ? true
                                                           : (BaseLong) *this == (BaseLong) op;
#endif
}

bool IEEEdouble::operator!= (IEEEdouble op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return value.double_value != op.value.double_value;
#else
    return !(*this == op);
#endif
}

IEEEdouble IEEEdouble::operator+ (IEEEdouble op)
{
#ifdef HAVE_IEEE754
    // FIXME: This could be sped up by inlining
    return IEEEdouble(value.double_value + op.value.double_value);
#else
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    //
    // Adding unlike infinities not allowed
    // Adding infinities of same sign is infinity of that sign
    // Adding finite and infinity produces infinity
    //
    if (IsInfinite())
        return op.IsInfinite() && (Sign() != op.Sign()) ? NaN() : *this;
    if (op.IsInfinite())
        return op;

    //
    // Adding zero is easy
    //
    if (IsZero())
        return (op.IsZero()) ? (Sign() != op.Sign()) ? POSITIVE_ZERO()
                                                     : *this
                             : op;
    if (op.IsZero())
        return *this;

    //
    // Now for the real work - do manipulations on copies
    //
    LongInt x, y, round = 0;
    int expx, expy, signx, signy;
    
    expx = SplitInto(x);
    expy = op.SplitInto(y);
    
    signx = Sign();
    signy = op.Sign();

    // If the exponents are far enough apart, the answer is easy
    if (expx > expy + 54)
        return *this;
    if (expy > expx + 54)
        return op;

    //
    // Denormalize the fractions, so that the exponents are
    // the same and then set the exponent for the result.
    // Leave enough space for overflow and LONG_MIN avoidance!
    // 
    if (signx)
        x = -x;
    if (signy)
        y = -y;

    x <<= 8;
    y <<= 8;

    if (expx > expy)
    {
        round = y << (64 + expy - expx);
        y >>= expx - expy;
    }
    else if (expy > expx)
    {
        round = x << (64 + expx - expy);
        x >>= expy - expx;
        expx = expy;
    }

    //
    // Do the arithmetic. The excess magnitude of 64-bit arithmetic means
    // overflow is impossible (we only need 1 spare bit!). We ensure that
    // pre-alignment avoids any question of LONG_MIN negation problems.
    //
    x += y;
    if (round != 0)
        x |= 1;

    //
    // If the result is negative, then make the fraction positive again
    // and remember the sign.
    //
    if (x < 0)
    {
        x = -x;
        signx = 1;
    }
    else
        signx = 0;

    if (x == 0)
        return signx ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    //
    // Time to normalize again! If we need to shift left (the addition was
    // effectively a subtraction), then there cannot be any reason to round.
    // If the number fits exactly we don't have anything to do either.
    //
    return Normalize(signx, expx - 8, (ULongInt) x);
#endif
}

IEEEdouble IEEEdouble::operator- ()
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return IEEEdouble(-value.double_value);
#else
    if (IsNaN())
        return *this;
    return IEEEdouble(HighWord() ^ 0x80000000, LowWord());
#endif // HAVE_IEEE754
}

IEEEdouble IEEEdouble::operator* (IEEEdouble op)
{
#ifdef HAVE_IEEE754
    return IEEEdouble(value.double_value * op.value.double_value);
#else
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    int sign = Sign() ^ op.Sign();

    //
    // If either operand is zero or infinite, then the answer is easy.
    //
    if (IsZero())
        return op.IsInfinite() ? NaN()
                               : sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (op.IsZero())
        return IsInfinite() ? NaN()
                            : sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (IsInfinite() || op.IsInfinite())
        return sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    //
    // Now for the real work - do manipulations on copies
    //
    ULongInt x, y, z, w, pr1, pr2;
    u4 round;
    int exponent;

    exponent = SplitInto(x) + op.SplitInto(y);

    //
    // The numbers to be multiplied are 53 bits in length (stored in 64 bit
    // integers). Split them in 32 bit parts, then shift result into place.
    // Fold the low order bits into the lsb for rounding purposes.
    //
    z = ULongInt(x.LowWord());
    w = ULongInt(y.LowWord());
    x >>= 32;
    y >>= 32;

    pr1 = z * w; // low*low
    pr2 = z * y + x * w + pr1.HighWord(); // low*high + high*low + adjusted pr1
    round = pr1.LowWord();
    x = (x * y) << 20; // high*high, adjusted

    round |= pr2.LowWord() & 0xFFF;
    pr2 >>= 12;

    x += pr2 | (round ? 1 : 0);

    return Normalize(sign, exponent - 8, x);
#endif // HAVE_IEEE754
}

IEEEdouble IEEEdouble::operator/ (IEEEdouble op)
{
#ifdef HAVE_IEEE754
    return op.IsZero() ? ((IsNaN() || IsZero()) ? NaN()
                               : (IsPositive() ^ op.IsPositive())
                                       ? NEGATIVE_INFINITY()
                                       : POSITIVE_INFINITY())
                       : IEEEdouble(value.double_value / op.value.double_value);
#else // HAVE_IEEE754
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    int sign = Sign() ^ op.Sign();

    //
    // Infinities and zeroes are special.
    //

    if (IsInfinite())
        return op.IsInfinite() ? NaN()
                               : sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    if (op.IsInfinite())
        return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (IsZero())
        return op.IsZero() ? NaN()
                           : sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();

    if (op.IsZero())
        return sign ? NEGATIVE_INFINITY() : POSITIVE_INFINITY();

    //
    // Now for the real work - do manipulations on copies
    //
    ULongInt x, y;
    int exponent;

    exponent = SplitInto(x) - op.SplitInto(y);

    ULongInt mask  (0x80000000, 0x00000000),
             result(0x00000000, 0x00000000);

    // Because both values are normalised, a single shift guarantees results.

    if (x < y)
    {
	x <<= 1;
	exponent--;
    }

    //
    // If the numerator is larger, then it is divisible.
    // Reflect this in the result, and do the subtraction.
    // Magnify the numerator again and reduce the mask.
    //
    while (mask != 0)
    {
	if (x >= y)
	{
	    result += mask;
	    x -= y;
	    if (x == 0)
		break;
	}
        x <<= 1;
        mask >>= 1;
    }

    return Normalize(sign, exponent - 11, result);
#endif // HAVE_IEEE754
}

IEEEdouble IEEEdouble::operator% (IEEEdouble op)
{
#ifdef HAVE_IEEE754
    return IEEEdouble((op.IsZero() ? NaN().value.double_value
                                   : fmod(value.double_value, op.value.double_value)));
#else // HAVE_IEEE754
    if (IsNaN() || op.IsNaN())
        return NaN(); // arithmetic on NaNs not allowed

    //
    // Infinities and zeroes are special.
    //

    if (IsInfinite() || op.IsZero())
        return NaN();

    if (IsZero() || op.IsInfinite())
        return *this;

    //
    // Now for the real work - do manipulations on copies
    // This algorithm is from fdlibm.c - see above notice
    //

    int sign = Sign();
    ULongInt x, y;
    int expy, exponent;
    LongInt z;

    expy = op.SplitInto(y);
    exponent = SplitInto(x) - expy;

    if (exponent < 0 || (exponent == 0 && x < y))
        return *this;

    while (exponent--)
    {
        z = x - y;
        if (z < 0)
            x <<= 1;
        else if (z == 0)
            return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
        else
            x = z + z;
    }
    z = x - y;
    if (z >= 0)
        x = z;

    if (x == 0)
        return sign ? NEGATIVE_ZERO() : POSITIVE_ZERO();
    return Normalize(sign, expy, x);
#endif // HAVE_IEEE754
}

bool IEEEdouble::operator< (IEEEdouble op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.double_value < op.value.double_value);
#else
    if (IsNaN() || op.IsNaN())
        return false; // NaNs are unordered
    if (IsZero())
        return op.IsZero() ? false : op.IsPositive();
    if (op.IsZero())
        return IsNegative();
    // Exploit fact that remaining IEEE floating point sort as signed ints
    u4 a = HighWord(), b = op.HighWord();
    return (a < b) || ((a == b) && LowWord() < op.LowWord());
#endif
}

bool IEEEdouble::operator<= (IEEEdouble op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.double_value <= op.value.double_value);
#else
    return *this < op || *this == op;
#endif
}

bool IEEEdouble::operator> (IEEEdouble op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.double_value > op.value.double_value);
#else
    if (IsNaN() || op.IsNaN())
        return false; // NaNs are unordered.
    if (IsZero())
        return op.IsZero() ? false : op.IsNegative();
    if (op.IsZero())
        return IsPositive();
    // Exploit fact that remaining IEEE floating point sort as signed ints
    u4 a = HighWord(), b = op.HighWord();
    return (a > b) || ((a == b) && LowWord() > op.LowWord());
#endif
}

bool IEEEdouble::operator>= (IEEEdouble op)
{
    // FIXME: This could be sped up by inlining
#ifdef HAVE_IEEE754
    return (value.double_value >= op.value.double_value);
#else
    return *this > op || *this == op;
#endif
}

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

