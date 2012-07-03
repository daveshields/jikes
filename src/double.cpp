// $Id: double.cpp,v 1.6 1999/08/20 14:44:21 shields Exp $
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

IEEEdouble IEEEdouble::min_long = IEEEdouble(0xc3e00000, 0x00000000);

IEEEfloat::IEEEfloat(float d)
{
    FloatValue() = d;
}

IEEEfloat::IEEEfloat(u4 a)
{
    Word() = a;
}

IEEEfloat::IEEEfloat(i4 a)
{
    FloatValue() = a;
}

IEEEfloat::IEEEfloat(char * name)
{
    FloatValue() = atof(name);
}

int IEEEfloat::IntValue()
{
    return (int) FloatValue();
}
int IEEEfloat::LongValue()
{
    return (long) FloatValue();
}

IEEEdouble::IEEEdouble(double d)
{
    DoubleValue() = d;
}

IEEEdouble::IEEEdouble(u4 a, u4 b)
{
    HighWord() = a;
    LowWord() = b;
}

IEEEdouble::IEEEdouble(IEEEfloat a)
{
    DoubleValue() = a.FloatValue();
}

IEEEdouble::IEEEdouble(i4 a)
{
    DoubleValue() = a;
}


IEEEdouble::IEEEdouble(u4 a)
{
    HighWord() = 0;
    LowWord() = a;
}

IEEEdouble::IEEEdouble(char * name)
{
    DoubleValue()  = atof(name);
}

bool IEEEdouble::operator== (IEEEdouble op)
{
    return DoubleValue() == op.DoubleValue();
}

bool IEEEdouble::operator!= (IEEEdouble op)
{
    return DoubleValue() != op.DoubleValue();
}

IEEEdouble IEEEdouble::operator+ (IEEEdouble op)
{
    return IEEEdouble(DoubleValue() + op.DoubleValue());
}

IEEEdouble& IEEEdouble::operator+= (IEEEdouble op)
{
    *this = *this + op;
    return *this;
}

IEEEdouble IEEEdouble::operator- ()
{
    return IEEEdouble(- this -> DoubleValue());
}

IEEEdouble IEEEdouble::operator- (IEEEdouble op)
{
    return IEEEdouble(DoubleValue() + (-op.DoubleValue()));
}

IEEEdouble& IEEEdouble::operator-= (IEEEdouble op)
{
    *this = *this - op;
    return *this;
}


IEEEdouble IEEEdouble::operator* (IEEEdouble op)
{
    return IEEEdouble(DoubleValue() * op.DoubleValue());
}

IEEEdouble& IEEEdouble::operator*= (IEEEdouble op)
{
    *this = *this * op;
    return *this;
}

IEEEdouble IEEEdouble::operator/ (IEEEdouble op)
{
#ifndef IEEE_DIV_0
    return IEEEdouble(DoubleValue() / op.DoubleValue());
#else /* IEEE_DIV_0 */
    return (op.DoubleValue() == 0.0
                              ? (DoubleValue() < 0.0
                                               ? NEGATIVE_INFINITY()
                                               : DoubleValue() == 0.0
                                                                ? NaN()
                                                                : POSITIVE_INFINITY())
                              : IEEEdouble(DoubleValue() / op.DoubleValue()));
#endif /* IEEE_DIV_0 */
}

IEEEdouble& IEEEdouble::operator/= (IEEEdouble op)
{
#ifndef IEEE_DIV_0
    *this = *this / op;
#else /* IEEE_DIV_0 */
    *this = (op.DoubleValue() == 0.0
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
    return (DoubleValue() < op.DoubleValue() ? 1 : 0);
}

bool IEEEdouble::operator<= (IEEEdouble op)
{
    return (DoubleValue() <= op.DoubleValue() ? 1 : 0);
}

bool IEEEdouble::operator> (IEEEdouble op)
{
    return (DoubleValue() > op.DoubleValue() ? 1 : 0);
}

bool IEEEdouble::operator>= (IEEEdouble op)
{
    return (DoubleValue() >= op.DoubleValue() ? 1 : 0);
}

bool IEEEfloat::operator== (IEEEfloat op)
{
    return FloatValue() == op.FloatValue();
}

bool IEEEfloat::operator!= (IEEEfloat op)
{
    return FloatValue() != op.FloatValue();
}


bool IEEEfloat::operator< (IEEEfloat op)
{
    return (FloatValue() < op.FloatValue() ? 1 : 0);
}

bool IEEEfloat::operator<= (IEEEfloat op)
{
    return (FloatValue() <= op.FloatValue() ? 1 : 0);
}

bool IEEEfloat::operator> (IEEEfloat op)
{
    return (FloatValue() > op.FloatValue() ? 1 : 0);
}

bool IEEEfloat::operator>= (IEEEfloat op)
{
    return (FloatValue() >= op.FloatValue() ? 1 : 0);
}


IEEEfloat IEEEfloat::operator+ (IEEEfloat op)
{
    return IEEEfloat(FloatValue() + op.FloatValue());
}

IEEEfloat& IEEEfloat::operator+= (IEEEfloat op)
{
    *this = *this + op;
    return *this;
}


IEEEfloat IEEEfloat::operator- ()
{
    return IEEEfloat( - this -> FloatValue());

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
    return IEEEfloat(FloatValue() * op.FloatValue());
}

IEEEfloat& IEEEfloat::operator*= (IEEEfloat op)
{
    *this = *this * op;
    return *this;
}

IEEEfloat IEEEfloat::operator/ (IEEEfloat op)
{
#ifndef IEEE_DIV_0
    return IEEEfloat(FloatValue() / op.FloatValue());
#else /* IEEE_DIV_0 */
    return (op.FloatValue() == 0.0
                             ? (FloatValue() < 0.0
                                             ? IEEEfloat(IEEEdouble::NEGATIVE_INFINITY())
                                             : FloatValue() == 0.0
                                                             ? IEEEfloat(IEEEdouble::NaN())
                                                             : IEEEfloat(IEEEdouble::POSITIVE_INFINITY()))
                             : IEEEfloat(FloatValue() / op.FloatValue()));
#endif /* IEEE_DIV_0 */
}

IEEEfloat& IEEEfloat::operator/= (IEEEfloat op)
{
#ifndef IEEE_DIV_0
    *this = *this / op;
#else /* IEEE_DIV_0 */
    *this = (op.FloatValue() == 0.0
                              ? (*this < (float) 0.0
                                       ? IEEEdouble::NEGATIVE_INFINITY()
                                       : *this == (float) 0.0
                                                ? IEEEdouble::NaN()
                                                : IEEEdouble::POSITIVE_INFINITY())
                              : *this / op);
#endif /* IEEE_DIV_0 */

    return *this;
}


void IEEEdouble::Fmodulus(IEEEdouble a, IEEEdouble b, IEEEdouble& result)
{
#ifndef IEEE_DIV_0
     result.DoubleValue() = fmod(a.DoubleValue(), b.DoubleValue());
#else /* IEEE_DIV_0 */
    result.DoubleValue() = (b.DoubleValue() == 0 ? NaN().DoubleValue()
                                                 : fmod(a.DoubleValue(), b.DoubleValue()));
#endif /* IEEE_DIV_0 */
}

void IEEEdouble::Divide(IEEEdouble dividend, IEEEdouble divisor, IEEEdouble &quotient)
{
#ifndef IEEE_DIV_0
    quotient = dividend.DoubleValue() / divisor.DoubleValue();
#else /* IEEE_DIV_0 */
    quotient = (divisor.DoubleValue() == 0
                                       ? (dividend.DoubleValue() < 0.0
                                                                 ? NEGATIVE_INFINITY()
                                                                 : dividend.DoubleValue() == 0.0
                                                                                           ? NaN()
                                                                                           : POSITIVE_INFINITY())
                                       : dividend.DoubleValue() / divisor.DoubleValue());
#endif /* IEEE_DIV_0 */

    return;
}

IEEEfloat::IEEEfloat(IEEEdouble a)
{
    FloatValue() = a.DoubleValue();
}

IEEEdouble::IEEEdouble(LongInt& a)
{
    DoubleValue() = a.Double();
}

void IEEEfloat::Fmodulus(IEEEfloat a, IEEEfloat b, IEEEfloat& result)
{
#ifndef IEEE_DIV_0
    result.FloatValue() = (float) fmod((double) a.FloatValue(), (double) b.FloatValue());
#else /* IEEE_DIV_0 */
    result.FloatValue() = (b.FloatValue() == 0.0 ? (IEEEdouble::NaN()).DoubleValue()
                                                 : fmod((double) a.FloatValue(), (double) b.FloatValue()));
#endif /* IEEE_DIV_0 */

    return;
}

void IEEEfloat::String(char * str)
{
    // format value into character string
    sprintf(str, "%E", FloatValue());
}
void IEEEdouble::String(char * str)
{
    // format value into character string
    sprintf(str, "%E", DoubleValue());
}
