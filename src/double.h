// $Id: double.h,v 1.15 2001/02/17 06:26:55 mdejong Exp $
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

#ifndef Double_INCLUDED
#define Double_INCLUDED

#include "platform.h"
#include "long.h"

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

class LongInt;
class IEEEdouble;

class IEEEfloat
{
    //
    // If HAVE_IEEE754 is defined, this class is simply a wrapper for
    // compiler-supported IEEE operations.  If not, then this class
    // emulates IEEE 754 behavior, in acccordance with the Java Language,
    // using only integer arithmetic.
    //
private:
    union
    {
        float float_value;
        u4 word; // unsigned
        i4 iword; // signed
    } value;

    enum { FRACT_BITS = 23, BIAS = 127 };

    inline u4 SignBit(void)  { return value.word & 0x80000000; }
    inline u4 ExpBits(void)  { return value.word & 0x7F800000; }

    // takes the (possibly) unnormalized fraction with its corresponding
    // exponent and sign, and creates the normalized float
    static IEEEfloat Normalize(int, int, u4);
    // takes the float and splits it into a normalized fraction and exponent;
    // the exponent is returned, the parameter fraction is modified
    int SplitInto(u4 &fraction);

#ifndef HAVE_MEMBER_CONSTANTS
    // VC++ can't cope with constant class members
    static u4 MAX_FRACT;
    static u4 MAX_FRACT2;
    static u4 MIN_INT_F;
    static i4 MIN_INT;
    static i4 MAX_INT;
#else
    static const u4 MAX_FRACT   = 0x01000000;
    static const u4 MAX_FRACT2  = 0x00800000;
    static const u4 MIN_INT_F   = 0xCF000000;
    static const i4 MIN_INT     = 0x80000000;
    static const i4 MAX_INT     = 0x7FFFFFFF;
#endif

public:
    //
    // Information methods, for evaluating components of the float
    //
    inline u4 Word(void) { return value.word; }

    inline int  Sign(void)       { return value.word >> 31; }
    inline int  Exponent(void)   { return ((value.iword >> 23) & 0x000000FF) - BIAS; }
    inline u4   Fraction(void)   { return (value.word & 0x007FFFFF)
                                          | (ExpBits() ? 0x00800000 : 0); }

    inline bool IsNaN(void)      { return (value.word & 0x7FFFFFFF) > 0x7F800000; }

    inline bool IsNegative(void) { return (value.iword < 0) && !IsNaN(); }
    inline bool IsPositive(void) { return (value.iword >= 0) && !IsNaN(); }

    inline bool IsNegativeZero(void)     { return value.word == 0x80000000; }
    inline bool IsPositiveZero(void)     { return value.word == 0x00000000; }
    inline bool IsZero(void)             { return (value.word & 0x7FFFFFFF) == 0x00000000; }

    inline bool IsNegativeInfinity(void) { return value.word == 0xFF800000; }
    inline bool IsPositiveInfinity(void) { return value.word == 0x7F800000; }
    inline bool IsInfinite(void)         { return (value.word & 0x7FFFFFFF) == 0x7F800000; }


    //
    // Generation methods, for creating common constants
    //
    static inline IEEEfloat NaN(void)               { return IEEEfloat(0x7FC00000U); }
    static inline IEEEfloat POSITIVE_INFINITY(void) { return IEEEfloat(0x7F800000U); }
    static inline IEEEfloat NEGATIVE_INFINITY(void) { return IEEEfloat(0xFF800000U); }
    static inline IEEEfloat POSITIVE_ZERO(void)     { return IEEEfloat(0x00000000U); }
    static inline IEEEfloat NEGATIVE_ZERO(void)     { return IEEEfloat(0x80000000U); }


    //
    // Constructors
    //
    // Create a float from the given value
    inline   IEEEfloat(float f) { value.float_value = f; }

    // Convert string to float, the float is NaN if check_invalid is true and input is invalid by JLS
             IEEEfloat(char *, bool check_invalid = false);
    // Widening conversion of int to float, may lose precision
             IEEEfloat(i4);
    // Widening conversion of long to float, may lose precision
             IEEEfloat(LongInt);
    // Narrowing conversion of double to float, may lose precision
#ifdef HAVE_EXPLICIT
    explicit
#endif
             IEEEfloat(IEEEdouble);
    // Create a float without initializing it
    inline   IEEEfloat(void) {}

    // Load a specified bit pattern (contrast to IEEEfloat(i4))
    inline   IEEEfloat(u4 bits) { value.word = bits; }


    //
    // Conversion routines
    //
           i4         IntValue(void);
           LongInt    LongValue(void);
    inline float      FloatView(void) { return value.float_value; }
    inline IEEEdouble DoubleValue(void);


    //
    // Floating-point operations
    // TODO: add const reference versions
    //
           IEEEfloat  operator+  (IEEEfloat);   // binary addition
    inline IEEEfloat  operator+  (void)         { return *this; } // unary plus
    inline IEEEfloat &operator+= (IEEEfloat op) { return *this = *this + op; } // add and assign
    inline IEEEfloat  operator++ (void)         { return *this += 1; } // pre-increment
    inline IEEEfloat  operator++ (int dummy)
    {
        IEEEfloat result = *this;
        *this += 1;
        return result;
    } // post-increment

           IEEEfloat  operator-  (void);        // unary minus
    inline IEEEfloat  operator-  (IEEEfloat op) { return *this + (-op); } // binary subtraction
    inline IEEEfloat &operator-= (IEEEfloat op) { return *this = *this - op; } // subtract and assign
    inline IEEEfloat  operator-- (void)         { return *this -= 1; } // pre-decrement
    inline IEEEfloat  operator-- (int dummy)
    {
        IEEEfloat result = *this;
        *this -= 1;
        return result;
    } // post-decrement

           IEEEfloat  operator*  (IEEEfloat);   // multiplication
    inline IEEEfloat &operator*= (IEEEfloat op) { return *this = *this * op; } // multiply and assign

           IEEEfloat  operator/  (IEEEfloat);   // divide
    inline IEEEfloat &operator/= (IEEEfloat op) { return *this = *this / op; } // divide and assign

           IEEEfloat  operator%  (IEEEfloat);   // modulus
    inline IEEEfloat &operator%= (IEEEfloat op) { return *this = *this % op; } // modulus and assign

    //
    // Comparison operators.  Recall that NaN does not compare, and 0.0 == -0.0
    //
    bool operator== (IEEEfloat); // equal
    bool operator!= (IEEEfloat); // not equal
    bool operator<  (IEEEfloat); // less-than
    bool operator>  (IEEEfloat); // greater-than
    bool operator<= (IEEEfloat); // less-than or equal
    bool operator>= (IEEEfloat); // greater-than or equal

    //
    // Methods for hashing floats, behave like java.lang.Float counterparts:
    //  * -0.0f and 0.0f are different, with positive 0 comparing as greater
    //    than negative 0.
    //  * identical bit patterns of NaN are the same in equals, but not
    //    unique patterns
    //  * all bit patterns of NaN compare as greater than any other float,
    //    including positive infinity, but the same as any other NaN pattern
    //
    inline bool equals(IEEEfloat op) { return value.word == op.value.word; }
    inline i4 hashCode(void) { return value.iword; }
    inline int compareTo(IEEEfloat op)
    {
        return IsNaN() ? 1
                       : (IsZero() && op.IsZero()) ? op.Sign() - Sign()
                                                   : (*this < op) ? -1
                                                                  : *this > op;
    }        
};


class IEEEdouble : public BaseLong
    //
    // If HAVE_IEEE754 is defined, this class is simply a wrapper for
    // compiler-supported IEEE operations.  If not, then this class
    // emulates IEEE 754 behavior, in acccordance with the Java Language,
    // using only integer arithmetic.
    //
{
private:
    enum { FRACT_BITS = 52, BIAS = 1023 };

    inline u4 SignBit(void) { return HighWord() & 0x80000000; }
    inline u4 ExpBits(void) { return HighWord() & 0x7FF00000; }

    // takes the (possibly) unnormalized fraction with its corresponding
    // exponent and sign, and creates the normalized double
    static IEEEdouble Normalize(int, int, ULongInt);
    // takes the double and splits it into a normalized fraction and exponent;
    // the exponent is returned, the parameter fraction is modified
    int SplitInto(BaseLong &fraction);



#ifndef HAVE_MEMBER_CONSTANTS
    // VC++ can't cope with constant class members
    static u4 MAX_FRACT;
    static u4 MAX_FRACT2;
    static i4 MIN_INT;
    static i4 MAX_INT;
#else
    static const u4 MAX_FRACT  = 0x00200000;
    static const u4 MAX_FRACT2 = 0x00100000;
    static const i4 MIN_INT    = 0x80000000;
    static const i4 MAX_INT    = 0x7FFFFFFF;
#endif

public:
    //
    // Information methods, for evaluating components of the float
    //
    inline int     Sign(void)     { return HighWord() >> 31; }
    inline int     Exponent(void) { return (int) ((HighWord() >> 20) & 0x000007FF) - BIAS; }
    inline LongInt Fraction(void)
    {
        return LongInt((HighWord() & 0x000FFFFF) | (ExpBits() ? 0x00100000 : 0),
                       LowWord()); 
    }

    inline bool IsNaN(void)
    {
        // optimized for no branching, idea from fdlibm.c
        u4 high = HighWord(),
            low = LowWord();
        return ((high & 0x7FFFFFFF) | ((low | -(i4) low) >> 31)) > 0x7FF00000;
    }

    inline bool IsNegative(void) { return ((i4) HighWord() < 0) && !IsNaN(); }
    inline bool IsPositive(void) { return ((i4) HighWord() >= 0) && !IsNaN(); }

    inline bool IsNegativeZero(void)
    {
        return (HighWord() == 0x80000000) && (LowWord() == 0x00000000);
    }
    inline bool IsPositiveZero(void)
    {
        return (HighWord() == 0x00000000) && (LowWord() == 0x00000000);
    }
    inline bool IsZero(void)
    {
        return ((HighWord() & 0x7FFFFFFF) == 0x00000000) && (LowWord() == 0x00000000);
    }

    inline bool IsNegativeInfinity(void)
    {
        return (HighWord() == 0xFFF00000) && (LowWord() == 0x00000000);
    }
    inline bool IsPositiveInfinity(void)
    {
        return (HighWord() == 0x7FF00000) && (LowWord() == 0x00000000);
    }
    inline bool IsInfinite(void)
    {
        return ((HighWord() & 0x7FFFFFFF) == 0x7FF00000) && (LowWord() == 0x00000000);
    }


    //
    // Generation methods, for creating common constants
    //
    static inline IEEEdouble NaN(void)               { return IEEEdouble(0x7FF80000U, 0x00000000U); }
    static inline IEEEdouble POSITIVE_INFINITY(void) { return IEEEdouble(0x7FF00000U, 0x00000000U); }
    static inline IEEEdouble NEGATIVE_INFINITY(void) { return IEEEdouble(0xFFF00000U, 0x00000000U); }
    static inline IEEEdouble POSITIVE_ZERO(void)     { return IEEEdouble(0x00000000U, 0x00000000U); }
    static inline IEEEdouble NEGATIVE_ZERO(void)     { return IEEEdouble(0x80000000U, 0x00000000U); }


    //
    // Constructors
    //
    // Create a double from the given value
    inline IEEEdouble(double d) { value.double_value = d; }
    // Convert string to double, the double is NaN if check_invalid is true and input is invalid by JLS
           IEEEdouble(char *, bool check_invalid = false);

    // widening conversion of int to double, no information lost
           IEEEdouble(i4);
    // Widening conversion of long to double, may lose precision
           IEEEdouble(LongInt);
    // Widening conversion of float to double, no information lost
           IEEEdouble(IEEEfloat);
    // Create a double without initializing it
    inline IEEEdouble(void) {}
    // Load a specified bit pattern (contrast to IEEEfloat(LongInt))
    inline IEEEdouble(u4 hi, u4 lo) { setHighAndLowWords(hi, lo); }

    //
    // Conversion routines
    //
           i4        IntValue(void);
           LongInt   LongValue(void);
    inline IEEEfloat FloatValue(void) { return IEEEfloat(*this); }


    //
    // Floating-point operations
    // TODO: add const reference versions
    //
           IEEEdouble  operator+  (IEEEdouble);   // binary addition
    inline IEEEdouble  operator+  (void)          { return *this; } // unary plus
    inline IEEEdouble &operator+= (IEEEdouble op) { return *this = *this + op; } // add and assign
    inline IEEEdouble  operator++ (void)          { return *this += 1; } // pre-increment
    inline IEEEdouble  operator++ (int dummy)
    {
        IEEEdouble result = *this;
        *this += 1;
        return result;
    } // post-increment

           IEEEdouble  operator-  (void);         // unary minus
    inline IEEEdouble  operator-  (IEEEdouble op) { return *this + (-op); } // binary subtraction
    inline IEEEdouble &operator-= (IEEEdouble op) { return *this = *this - op; } // subtract and assign
    inline IEEEdouble  operator-- (void)          { return *this -= 1; } // pre-decrement
    inline IEEEdouble  operator-- (int dummy)
    {
        IEEEdouble result = *this;
        *this -= 1;
        return result;
    } // post-decrement

           IEEEdouble  operator*  (IEEEdouble);   // multiplication
    inline IEEEdouble &operator*= (IEEEdouble op) { return *this = *this * op; } // multiply and assign

           IEEEdouble  operator/  (IEEEdouble);   // divide
    inline IEEEdouble &operator/= (IEEEdouble op) { return *this = *this / op; } // divide and assign

           IEEEdouble  operator%  (IEEEdouble);   // modulus
    inline IEEEdouble &operator%= (IEEEdouble op) { return *this = *this % op; } // modulus and assign

    //
    // Comparison operators.  Recall that NaN does not compare, and 0.0 == -0.0
    //
    bool operator== (IEEEdouble); // equal
    bool operator!= (IEEEdouble); // not equal
    bool operator<  (IEEEdouble); // less-than
    bool operator>  (IEEEdouble); // greater-than
    bool operator<= (IEEEdouble); // less-than or equal
    bool operator>= (IEEEdouble); // greater-than or equal

    //
    // Methods for hashing doubles, behave like java.lang.Double counterparts:
    //  * -0.0 and 0.0 are different, with positive 0 comparing as greater
    //    than negative 0.
    //  * identical bit patterns of NaN are the same in equals, but not
    //    unique patterns
    //  * all bit patterns of NaN compare as greater than any other float,
    //    including positive infinity, but the same as any other NaN pattern
    //
    inline bool equals(IEEEdouble op) { return (BaseLong) *this == (BaseLong) op; }
    inline i4 hashCode(void) { return ((BaseLong) *this).hashCode(); }
    inline int compareTo(IEEEdouble op)
    {
        return IsNaN() ? 1
                       : (IsZero() && op.IsZero()) ? op.Sign() - Sign()
                                                   : (*this < op) ? -1
                                                                  : *this > op;
    }        
};


inline IEEEdouble IEEEfloat::DoubleValue()
{
    return IEEEdouble(*this);
}

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif // Double_INCLUDED

