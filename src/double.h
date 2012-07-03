// $Id: double.h,v 1.4 1999/07/06 13:49:19 shields Exp $
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
    protected:
        union
        {
            float float_value;
            u4 word;
        } value;

    public:

        u4 &Word() { return value.word; }
        float &FloatValue() { return value.float_value; }

        IEEEfloat(float);
        IEEEfloat(u4);

        IEEEfloat(i4);
        IEEEfloat(char *);
        IEEEfloat(IEEEdouble a);

        inline IEEEfloat (void) {}

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

        int IntValue();
        int LongValue();

        static void Fmodulus(IEEEfloat, IEEEfloat, IEEEfloat&);
        static void Divide(IEEEfloat, IEEEfloat, IEEEfloat &, IEEEfloat &);
        void String(char *);
};


class IEEEdouble
{
protected:
    union
    {
        double double_value;
        u4 word[2];
    } value;

public:

#ifdef BIGENDIAN
    u4 &HighWord() { return value.word[0]; }
    u4 &LowWord()  { return value.word[1]; }
#else
    u4 &LowWord()  { return value.word[0]; }
    u4 &HighWord() { return value.word[1]; }
#endif

    double &DoubleValue() { return value.double_value; }

    static IEEEdouble min_long;

    //
    //     static inline IEEEdouble NaN()               { return  zero / zero; }
    //     static inline IEEEdouble POSITIVE_INFINITY() { return  1.0  / zero; }
    //     static inline IEEEdouble NEGATIVE_INFINITY() { return -1.0  / zero; }
    //
    static inline IEEEdouble NaN()               { return  IEEEdouble(0x7fffffff, 0xffffffff); }
    static inline IEEEdouble POSITIVE_INFINITY() { return  IEEEdouble(0x7ff00000, 0x00000000); }
    static inline IEEEdouble NEGATIVE_INFINITY() { return  IEEEdouble(0xfff00000, 0x00000000); }

    IEEEdouble(LongInt&);
    IEEEdouble(double);
    IEEEdouble(u4, u4);
    IEEEdouble(u4);
    IEEEdouble(IEEEfloat);
    IEEEdouble(i4);
    IEEEdouble(char *);
    inline IEEEdouble (void) {}

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

    inline int IntValue() { return (int) DoubleValue(); }
    void String(char *);

    static void Divide(IEEEdouble, IEEEdouble, IEEEdouble &);
    static void Fmodulus(IEEEdouble, IEEEdouble, IEEEdouble &);
};

#endif
