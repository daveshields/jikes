// $Id: double.h,v 1.11 2000/07/25 11:32:33 mdejong Exp $
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

#include "platform.h"

#ifdef	HAVE_NAMESPACES
namespace Jikes {	// Open namespace Jikes block
#endif

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
    
    inline int IntValue()
        {
            if(IsNaN())                                                             
                return 0;                                                             
            
            if(value.float_value < INT_MIN)
                return INT_MIN;
            else if (value.float_value > INT_MAX)
                return INT_MAX;
            else
                return (int)value.float_value;
        }

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
    enum { BIAS = 1023 };

    union
    {
        double double_value;
#ifndef HAVE_UNSIGNED_LONG_LONG
        u4 word[2];
#else
        u8 words;
#endif // HAVE_UNSIGNED_LONG_LONG
    } value;

    // Set the high word only. Does not modify the low word!
    inline void setHighWord(u4 high) {
#ifndef HAVE_UNSIGNED_LONG_LONG
# ifndef WORDS_BIGENDIAN
        value.word[1] = high;
# else
        value.word[0] = high;
# endif
#else
        setHighAndLowWords(high, LowWord());
#endif // HAVE_UNSIGNED_LONG_LONG
    }

    // Set the low word only. Does not modify the high word!
    inline void setLowWord(u4 low) {
#ifndef HAVE_UNSIGNED_LONG_LONG
# ifndef WORDS_BIGENDIAN
        value.word[0] = low;
# else
        value.word[1] = low;
# endif
#else
        setHighAndLowWords(HighWord(), low);
#endif // HAVE_UNSIGNED_LONG_LONG
    }
    
    // Set the value for both words.
    inline void setHighAndLowWords(u4 high, u4 low) {
#ifndef HAVE_UNSIGNED_LONG_LONG
# ifndef WORDS_BIGENDIAN
        value.word[1] = high;
        value.word[0] = low;
# else
        value.word[0] = high;
        value.word[1] = low;
# endif
#else
        value.words = (((u8) high) << 32) | low;
#endif // HAVE_UNSIGNED_LONG_LONG
    }

public:

#ifndef HAVE_UNSIGNED_LONG_LONG
# ifndef WORDS_BIGENDIAN
    inline u4 HighWord() { return value.word[1]; }
    inline u4 LowWord()  { return value.word[0]; }
# else
    inline u4 HighWord() { return value.word[0]; }
    inline u4 LowWord()  { return value.word[1]; }
# endif
#else
    inline u4 HighWord() { return ((u4) (value.words >> 32)); }
    inline u4 LowWord()  { return ((u4) value.words); }
#endif // HAVE_UNSIGNED_LONG_LONG

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

    inline int IntValue()
        {
            if(IsNaN())                                                             
                return 0;                                                             
            
            if(value.double_value < INT_MIN)
                return INT_MIN;
            else if (value.double_value > INT_MAX)
                return INT_MAX;
            else
                return (int)value.double_value;
        }

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

#ifdef	HAVE_NAMESPACES
}			// Close namespace Jikes block
#endif

#endif // Double_INCLUDED

