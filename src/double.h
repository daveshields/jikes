// $Id: double.h,v 1.6 2000/01/06 06:46:47 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef Double_INCLUDED
#define Double_INCLUDED

#include "config.h"
#include <math.h>

class LongInt;
class IEEEdouble;
class IEEEfloat
{
private:
    union
    {
        float float_value;
        u4 word;
    } value;

    enum { BIAS = 127 };

public:

    u4 Word() { return value.word; }

    float FloatValue() { return value.float_value; }
    float FloatRoundedValue();

    int Exponent()
    {
        return ((value.word >> 23) & 0x000000FF) - BIAS;
    }

    bool IsNaN()
    {
        return (value.word & 0xFF800000) == 0x7F800000 && (value.word & 0x007FFFFF) > 0;
    }

    bool IsNegative()
    {
        return (value.word & 0x80000000) == 0x80000000;
    }

    bool IsNegativeZero()
    {
        return value.word == 0x80000000;
    }

    bool IsPositiveZero()
    {
        return value.word == 0x00000000;
    }

    bool IsNegativeInfinity()
    {
        return value.word == 0xFF800000;
    }

    bool IsPositiveInfinity()
    {
        return value.word == 0x7F800000;
    }

    static inline IEEEfloat NaN()               { return  IEEEfloat(0x7FC00000); }
    static inline IEEEfloat POSITIVE_INFINITY() { return  IEEEfloat(0x7F800000); }
    static inline IEEEfloat NEGATIVE_INFINITY() { return  IEEEfloat(0xFF800000); }

    IEEEfloat(float);
    IEEEfloat(u4);
    IEEEfloat(i4);
    IEEEfloat(char *);
    IEEEfloat(IEEEdouble a);
    IEEEfloat() {}

    inline int IntValue() { return (int) FloatValue(); }

    IEEEfloat  operator+  (IEEEfloat); // binary addition
    IEEEfloat  operator+  ();         // unary plus
    IEEEfloat& operator+= (IEEEfloat); // add and assign

    IEEEfloat  operator-  (IEEEfloat); // binary subtraction
    IEEEfloat  operator-  ();         // unary minus
    IEEEfloat& operator-= (IEEEfloat); // subtract and assign

    IEEEfloat  operator* (IEEEfloat);  // multiplication
    IEEEfloat& operator*=(IEEEfloat);  // multiply and assign

    IEEEfloat  operator/ (IEEEfloat);  // divide
    IEEEfloat& operator/=(IEEEfloat);  // divide and assign

    bool      operator== (IEEEfloat); // equal
    bool      operator!= (IEEEfloat); // not equal

    bool  operator<  (IEEEfloat); // less-than
    bool  operator>  (IEEEfloat); // greater-than
    bool  operator<= (IEEEfloat); // less-than or equal
    bool  operator>= (IEEEfloat); // greater-than or equal

    static void Fmodulus(IEEEfloat, IEEEfloat, IEEEfloat&);
    static void Divide(IEEEfloat, IEEEfloat, IEEEfloat &, IEEEfloat &);
};


class IEEEdouble
{
private:
    union
    {
        double double_value;
        u4 word[2];
    } value;

    enum { BIAS = 1023 };

#ifdef WORDS_BIGENDIAN
    u4 &High() { return value.word[0]; }
    u4 &Low()  { return value.word[1]; }
#else
    u4 &Low()  { return value.word[0]; }
    u4 &High() { return value.word[1]; }
#endif

public:

    u4 HighWord() { return High(); }
    u4 LowWord()  { return Low(); }

    double DoubleValue() { return value.double_value; }
    double DoubleRoundedValue();

    int Exponent() // Notice that the value returned here is unadjusted
    {
        return ((HighWord() >> 20) & 0x000007FF) - BIAS;
    }

    bool IsNaN()
    {
        u4 high = HighWord(),
           low  = LowWord();
        return (high & 0xFFF00000) == 0x7FF00000 && ((high & 0x000FFFFF) > 0 || low > 0);
    }

    bool IsNegative()
    {
        return (HighWord() & 0x80000000) == 0x80000000;
    }

    bool IsNegativeZero()
    {
        return HighWord() == 0x80000000 && LowWord() == 0x00000000;
    }

    bool IsPositiveZero()
    {
        return HighWord() == 0x00000000 && LowWord() == 0x00000000;
    }

    bool IsNegativeInfinity()
    {
        return HighWord() == 0xFFF00000 && LowWord() == 0x00000000;
    }

    bool IsPositiveInfinity()
    {
        return HighWord() == 0x7FF00000 && LowWord() == 0x00000000;
    }

    static IEEEdouble min_long;

    static inline IEEEdouble NaN()               { return  IEEEdouble(0x7FF80000, 0x00000000); }
    static inline IEEEdouble POSITIVE_INFINITY() { return  IEEEdouble(0x7FF00000, 0x00000000); }
    static inline IEEEdouble NEGATIVE_INFINITY() { return  IEEEdouble(0xFFF00000, 0x00000000); }

    IEEEdouble(LongInt&);
    IEEEdouble(double);
    IEEEdouble(u4, u4);
    IEEEdouble(u4);
    IEEEdouble(IEEEfloat);
    IEEEdouble(i4);
    IEEEdouble(char *);
    IEEEdouble() {}

    inline int IntValue() { return (int) DoubleValue(); }

    IEEEdouble  operator+  (IEEEdouble); // binary addition
    IEEEdouble  operator+  ();         // unary plus
    IEEEdouble& operator+= (IEEEdouble); // add and assign

    IEEEdouble  operator-  (IEEEdouble); // binary subtraction
    IEEEdouble  operator-  ();         // unary minus
    IEEEdouble& operator-= (IEEEdouble); // subtract and assign

    IEEEdouble  operator* (IEEEdouble);  // multiplication
    IEEEdouble& operator*=(IEEEdouble);  // multiply and assign

    IEEEdouble  operator/ (IEEEdouble);  // divide
    IEEEdouble& operator/=(IEEEdouble);  // divide and assign

    bool  operator== (IEEEdouble); // equal
    bool  operator!= (IEEEdouble); // not equal
    bool  operator<  (IEEEdouble); // less-than
    bool  operator>  (IEEEdouble); // greater-than
    bool  operator<= (IEEEdouble); // less-than or equal
    bool  operator>= (IEEEdouble); // greater-than or equal

    static void Divide(IEEEdouble, IEEEdouble, IEEEdouble &);
    static void Fmodulus(IEEEdouble, IEEEdouble, IEEEdouble &);
};

#endif
