// $Id: double.cpp,v 1.7 1999/10/27 18:07:09 shields Exp $
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
#include "double.h"
#include "long.h"

IEEEfloat::IEEEfloat(float d)
{
    value.float_value = d;
}

IEEEfloat::IEEEfloat(u4 a)
{
    value.word = a;
}

IEEEfloat::IEEEfloat(i4 a)
{
    value.float_value = a;
}

IEEEfloat::IEEEfloat(char *name)
{
    //
    // TODO: This conversion is a temporary patch. Need volunteer to implement
    //       Clinger algorithm from PLDI 1990.
    //
    value.float_value = atof(name);
}

float IEEEfloat::FloatRoundedValue()
{
    //
    // TODO: Implement this properly !
    //
    return value.float_value;
}

bool IEEEfloat::operator== (IEEEfloat op)
{
    return value.float_value == op.value.float_value;
}

bool IEEEfloat::operator!= (IEEEfloat op)
{
    return value.float_value != op.value.float_value;
}


bool IEEEfloat::operator< (IEEEfloat op)
{
    return (value.float_value < op.value.float_value ? true : false);
}

bool IEEEfloat::operator<= (IEEEfloat op)
{
    return (value.float_value <= op.value.float_value ? true : false);
}

bool IEEEfloat::operator> (IEEEfloat op)
{
    return (value.float_value > op.value.float_value ? true : false);
}

bool IEEEfloat::operator>= (IEEEfloat op)
{
    return (value.float_value >= op.value.float_value ? true : false);
}

IEEEfloat IEEEfloat::operator+ (IEEEfloat op)
{
    return IEEEfloat(value.float_value + op.value.float_value);
}

IEEEfloat& IEEEfloat::operator+= (IEEEfloat op)
{
    *this = *this + op;
    return *this;
}


IEEEfloat IEEEfloat::operator- ()
{
    return IEEEfloat((0x80000000 & value.word) == 0x80000000 ? 0x7fffffff & value.word : 0x80000000 | value.word);
}

IEEEfloat IEEEfloat::operator- (IEEEfloat op)
{
    return *this + (-op);
}

IEEEfloat& IEEEfloat::operator-= (IEEEfloat op)
{
    *this = *this - op;
    return *this;
}

IEEEfloat IEEEfloat::operator* (IEEEfloat op)
{
    return IEEEfloat(value.float_value * op.value.float_value);
}

IEEEfloat& IEEEfloat::operator*= (IEEEfloat op)
{
    *this = *this * op;
    return *this;
}

IEEEfloat IEEEfloat::operator/ (IEEEfloat op)
{
#ifndef IEEE_DIV_0
    return IEEEfloat(value.float_value / op.value.float_value);
#else /* IEEE_DIV_0 */
    return (op.value.float_value == 0.0
                                  ? (value.float_value < 0.0
                                                       ? NEGATIVE_INFINITY()
                                                       : value.float_value == 0.0
                                                                            ? NaN()
                                                                            : POSITIVE_INFINITY()))
                                  : IEEEfloat(value.float_value / op.value.float_value));
#endif /* IEEE_DIV_0 */
}

IEEEfloat& IEEEfloat::operator/= (IEEEfloat op)
{
#ifndef IEEE_DIV_0
    *this = *this / op;
#else /* IEEE_DIV_0 */
    *this = (op.value.float_value == 0.0
                                   ? (*this < (float) 0.0
                                            ? NEGATIVE_INFINITY()
                                            : *this == (float) 0.0
                                                     ? NaN()
                                                     : POSITIVE_INFINITY())
                                   : *this / op);
#endif /* IEEE_DIV_0 */

    return *this;
}

IEEEfloat::IEEEfloat(IEEEdouble a)
{
    value.float_value = a.DoubleValue();
}

void IEEEfloat::Fmodulus(IEEEfloat a, IEEEfloat b, IEEEfloat& result)
{
#ifndef IEEE_DIV_0
    result.value.float_value = (float) fmod((double) a.value.float_value, (double) b.value.float_value);
#else /* IEEE_DIV_0 */
    result.value.float_value = (b.value.float_value == 0.0 ? NaN().FloatValue()
                                                           : fmod((double) a.value.float_value, (double) b.value.float_value));
#endif /* IEEE_DIV_0 */

    return;
}

IEEEdouble IEEEdouble::min_long = IEEEdouble(0xc3e00000, 0x00000000);

double IEEEdouble::DoubleRoundedValue()
{
    //
    // TODO: Implement this properly !
    //
    return DoubleValue();
}

IEEEdouble::IEEEdouble(double d)
{
    value.double_value = d;
}

IEEEdouble::IEEEdouble(u4 a, u4 b)
{
    High() = a;
    Low() = b;
}

IEEEdouble::IEEEdouble(IEEEfloat a)
{
    value.double_value = a.FloatValue();
}

IEEEdouble::IEEEdouble(i4 a)
{
    value.double_value = a;
}

IEEEdouble::IEEEdouble(u4 a)
{
    High() = 0;
    Low() = a;
}

IEEEdouble::IEEEdouble(char *name)
{
    //
    // TODO: This conversion is a temporary patch. Need volunteer to implement
    //       Clinger algorithm from PLDI 1990.
    //
    value.double_value = atof(name);
}

bool IEEEdouble::operator== (IEEEdouble op)
{
    return value.double_value == op.value.double_value;
}

bool IEEEdouble::operator!= (IEEEdouble op)
{
    return value.double_value != op.value.double_value;
}

IEEEdouble IEEEdouble::operator+ (IEEEdouble op)
{
    return IEEEdouble(value.double_value + op.value.double_value);
}

IEEEdouble& IEEEdouble::operator+= (IEEEdouble op)
{
    *this = *this + op;
    return *this;
}

IEEEdouble IEEEdouble::operator- ()
{
    u4 high = this -> HighWord();
    return IEEEdouble(((0x80000000 & high) == 0x80000000 ? 0x7fffffff & high : 0x80000000 | high), this -> LowWord());
}

IEEEdouble IEEEdouble::operator- (IEEEdouble op)
{
    return IEEEdouble(value.double_value + (-op.value.double_value));
}

IEEEdouble& IEEEdouble::operator-= (IEEEdouble op)
{
    *this = *this - op;
    return *this;
}


IEEEdouble IEEEdouble::operator* (IEEEdouble op)
{
    return IEEEdouble(value.double_value * op.value.double_value);
}

IEEEdouble& IEEEdouble::operator*= (IEEEdouble op)
{
    *this = *this * op;
    return *this;
}

IEEEdouble IEEEdouble::operator/ (IEEEdouble op)
{
#ifndef IEEE_DIV_0
    return IEEEdouble(value.double_value / op.value.double_value);
#else /* IEEE_DIV_0 */
    return (op.value.double_value == 0.0
                                   ? (value.double_value < 0.0
                                                         ? NEGATIVE_INFINITY()
                                                         : value.double_value == 0.0
                                                                               ? NaN()
                                                                               : POSITIVE_INFINITY())
                                   : IEEEdouble(value.double_value / op.value.double_value));
#endif /* IEEE_DIV_0 */
}

IEEEdouble& IEEEdouble::operator/= (IEEEdouble op)
{
#ifndef IEEE_DIV_0
    *this = *this / op;
#else /* IEEE_DIV_0 */
    *this = (op.value.double_value == 0.0
                                    ? (*this < 0.0
                                             ? NEGATIVE_INFINITY()
                                             : *this == 0.0
                                                      ? NaN()
                                                      : POSITIVE_INFINITY())
                                    : *this / op);
#endif /* IEEE_DIV_0 */

    return *this;
}

bool IEEEdouble::operator< (IEEEdouble op)
{
    return (value.double_value < op.value.double_value ? true : false);
}

bool IEEEdouble::operator<= (IEEEdouble op)
{
    return (value.double_value <= op.value.double_value ? true : false);
}

bool IEEEdouble::operator> (IEEEdouble op)
{
    return (value.double_value > op.value.double_value ? true : false);
}

bool IEEEdouble::operator>= (IEEEdouble op)
{
    return (value.double_value >= op.value.double_value ? true : false);
}

void IEEEdouble::Fmodulus(IEEEdouble a, IEEEdouble b, IEEEdouble& result)
{
#ifndef IEEE_DIV_0
     result.value.double_value = fmod(a.value.double_value, b.value.double_value);
#else /* IEEE_DIV_0 */
    result.value.double_value = (b.value.double_value == 0 ? NaN().value.double_value
                                                 : fmod(a.value.double_value, b.value.double_value));
#endif /* IEEE_DIV_0 */
}

void IEEEdouble::Divide(IEEEdouble dividend, IEEEdouble divisor, IEEEdouble &quotient)
{
#ifndef IEEE_DIV_0
    quotient = dividend.value.double_value / divisor.value.double_value;
#else /* IEEE_DIV_0 */
    quotient = (divisor.value.double_value == 0
                                            ? (dividend.value.double_value < 0.0
                                                                           ? NEGATIVE_INFINITY()
                                                                           : dividend.value.double_value == 0.0
                                                                                                          ? NaN()
                                                                                                          : POSITIVE_INFINITY())
                                            : dividend.value.double_value / divisor.value.double_value);
#endif /* IEEE_DIV_0 */

    return;
}

IEEEdouble::IEEEdouble(LongInt& a)
{
    value.double_value = a.Double();
}
