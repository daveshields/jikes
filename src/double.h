// $Id: double.h,v 1.17 2001/04/19 13:52:32 cabbey Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
//
// NOTES: The IEEE 754 emulation code in double.h and double.cpp within
// Jikes are adapted from code written by Alan M. Webb of IBM's Hursley
// lab in porting the Sun JDK to System/390.
//
//
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
//
// Likewise, the code for accurate conversions between floating point
// and decimal strings, in double.h, double.cpp, platform.h, and
// platform.cpp, is adapted from dtoa.c.  The original code can be
// found at http://netlib2.cs.utk.edu/fp/dtoa.c.
//
// The code in dtoa.c is copyrighted as follows:
//****************************************************************
//*
//* The author of this software is David M. Gay.
//*
//* Copyright (c) 1991, 2000, 2001 by Lucent Technologies.
//*
//* Permission to use, copy, modify, and distribute this software for any
//* purpose without fee is hereby granted, provided that this entire notice
//* is included in all copies of any software which is or includes a copy
//* or modification of this software and in all copies of the supporting
//* documentation for such software.
//*
//* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
//* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR LUCENT MAKES ANY
//* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
//* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
//*
//***************************************************************/
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
class BigInt;

class IEEEfloat
{
    //
    // If HAVE_IEEE754 is defined, this class is simply a wrapper for
    // compiler-supported IEEE operations.  If not, then this class
    // emulates IEEE 754 behavior, in acccordance with the Java Language,
    // using only integer arithmetic.
    //
private:
    friend class FloatToString;

    union
    {
        float float_value;
        u4 word; // unsigned
        i4 iword; // signed
    } value;

    //
    // two enums, so that first is signed, and second may be unsigned
    //
    enum
    {
        FRACT_SIZE = 23,         // number of bits in mantissa
        BIAS       = 127,        // exponent bias
        MAX_DEC_EXP = 38,        // maximum decimal exponent
        MAX_DIGITS = BIAS + FRACT_SIZE + 1 - MAX_DEC_EXP // maximum digits in precise decimal
    };
    enum
    {
        SIGN_BIT   = 0x80000000, // sign bit mask
        EXP_BITS   = 0x7F800000, // exponent bit mask
        FRACT_BITS = 0x007FFFFF, // mantissa bit mask
        ABS_BITS   = EXP_BITS | FRACT_BITS, // absolute value
        MAX_FRACT  = 0x01000000, // upper limit of mantissa
        MIN_FRACT  = 0x00800000, // lower limit of normalized mantissa
        NEG_ZERO   = 0x80000000, // -0.0
        POS_ZERO   = 0x00000000, // 0.0
        NEG_INF    = 0xFF800000, // -Inf
        POS_INF    = 0x7F800000, // +Inf
        NAN        = 0x7FC00000, // canonical NaN
        BYTE_MASK  = 0x000000FF, // mask off least significant byte
        MAX_INT    = 0x7FFFFFFF, // maximum integer
        MIN_INT    = 0x80000000, // minimum integer
        MIN_LONG_F = 0xDF000000, // bit pattern of (float)MIN_LONG
        MIN_INT_F  = 0xCF000000 // bit pattern of (float)MIN_INT
    };

    inline u4 SignBit(void) const   { return value.word & SIGN_BIT; }
    inline u4 ExpBits(void) const   { return value.word & EXP_BITS; }
    inline u4 FractBits(void) const { return value.word & FRACT_BITS; }

    // takes the (possibly) unnormalized fraction with its corresponding
    // exponent and sign, and creates the normalized float
    static IEEEfloat Normalize(int, int, u4);
    // takes the float and splits it into a normalized fraction and exponent;
    // the exponent is returned, the parameter fraction is modified
    int SplitInto(u4 &fraction) const;

    // returns the value of 1 ulp (unit in last place) for this float
    IEEEfloat Ulp(void) const;
    // returns the ratio between two floats represented in BigInts
    static IEEEfloat Ratio(const BigInt &, const BigInt &);
    // adjusts the value of this based on the ratio of the BigInts, and
    // in direction determined by the boolean
    bool Adjust(const BigInt &, const BigInt &, const bool);

#ifndef HAVE_MEMBER_CONSTANTS
    // VC++ can't cope with constant class members
    static IEEEfloat tens[]; // powers of 10 exactly represented in float
    static IEEEfloat bigtens[]; // powers of 10 by powers of 2
#else
    static const IEEEfloat tens[]; // powers of 10 exactly represented in float
    static const IEEEfloat bigtens[]; // powers of 10 by powers of 2
#endif

public:
    //
    // Information methods, for evaluating components of the float
    //
    inline u4 Word(void) const { return value.word; }

    inline int  Sign(void) const     { return value.word >> 31; }
    inline int  Exponent(void) const { return ((value.word & EXP_BITS) >> FRACT_SIZE) - BIAS; }
    inline u4   Fraction(void) const { return (value.word & FRACT_BITS)
                                          | (ExpBits() ? MIN_FRACT : 0); }
    static inline int Bias(void)      { return BIAS; }
    static inline int FractSize(void) { return FRACT_SIZE; }

    inline bool IsNaN(void) const      { return (value.word & ABS_BITS) > EXP_BITS; }

    inline bool IsNegative(void) const { return (value.iword < 0) && !IsNaN(); }
    inline bool IsPositive(void) const { return (value.iword >= 0) && !IsNaN(); }

    inline bool IsNegativeZero(void) const { return value.word == NEG_ZERO; }
    inline bool IsPositiveZero(void) const { return value.word == POS_ZERO; }
    inline bool IsZero(void) const         { return (value.word & ABS_BITS) == POS_ZERO; }

    inline bool IsNegativeInfinity(void) const { return value.word == NEG_INF; }
    inline bool IsPositiveInfinity(void) const { return value.word == POS_INF; }
    inline bool IsInfinite(void) const         { return (value.word & ABS_BITS) == POS_INF; }


    //
    // Generation methods, for creating common constants.
    // TODO: These methods should be converted to return static const
    // fields, rather than generating a non-const every time.  However,
    // adding const support is a big undertaking.
    //
    static inline const IEEEfloat NaN(void)               { return IEEEfloat(NAN); }
    static inline const IEEEfloat POSITIVE_INFINITY(void) { return IEEEfloat(POS_INF); }
    static inline const IEEEfloat NEGATIVE_INFINITY(void) { return IEEEfloat(NEG_INF); }
    static inline const IEEEfloat POSITIVE_ZERO(void)     { return IEEEfloat(POS_ZERO); }
    static inline const IEEEfloat NEGATIVE_ZERO(void)     { return IEEEfloat(NEG_ZERO); }


    //
    // Constructors
    //
    // Create a float from the given value
    inline   IEEEfloat(float f) { value.float_value = f; }

    // Convert string to float, the float is NaN if check_invalid is true and input is invalid by JLS
             IEEEfloat(const char *, bool check_invalid = false);
    // Widening conversion of int to float, may lose precision
             IEEEfloat(i4);
    // Widening conversion of long to float, may lose precision
             IEEEfloat(const LongInt &);
    // Narrowing conversion of double to float, may lose precision
#ifdef HAVE_EXPLICIT
    explicit
#endif
             IEEEfloat(const IEEEdouble &);
    // Create a float without initializing it
    inline   IEEEfloat(void) {}

    // Load a specified bit pattern (contrast to IEEEfloat(i4))
    inline   IEEEfloat(u4 bits) { value.word = bits; }


    //
    // Conversion routines
    //
           i4         IntValue(void) const;
           LongInt    LongValue(void) const;
    inline float      FloatView(void) const { return value.float_value; }
    inline IEEEdouble DoubleValue(void) const;


    //
    // Floating-point operations
    // TODO: add const reference versions
    //
           IEEEfloat  operator+  (const IEEEfloat) const;   // binary addition
    inline IEEEfloat  operator+  (void)         { return *this; } // unary plus
    inline IEEEfloat &operator+= (const IEEEfloat op) { return *this = *this + op; } // add and assign
    inline IEEEfloat  operator++ (void)         { return *this += 1; } // pre-increment
    inline IEEEfloat  operator++ (int dummy)
    {
        IEEEfloat result = *this;
        *this += 1;
        return result;
    } // post-increment

           IEEEfloat  operator-  (void) const;        // unary minus
    inline IEEEfloat  operator-  (const IEEEfloat op) const { return *this + (-op); } // binary subtraction
    inline IEEEfloat &operator-= (const IEEEfloat op) { return *this = *this - op; } // subtract and assign
    inline IEEEfloat  operator-- (void)         { return *this -= 1; } // pre-decrement
    inline IEEEfloat  operator-- (int dummy)
    {
        IEEEfloat result = *this;
        *this -= 1;
        return result;
    } // post-decrement

           IEEEfloat  operator*  (const IEEEfloat) const;   // multiplication
    inline IEEEfloat &operator*= (const IEEEfloat op) { return *this = *this * op; } // multiply and assign

           IEEEfloat  operator/  (const IEEEfloat) const;   // divide
    inline IEEEfloat &operator/= (const IEEEfloat op) { return *this = *this / op; } // divide and assign

           IEEEfloat  operator%  (const IEEEfloat) const;   // modulus
    inline IEEEfloat &operator%= (const IEEEfloat op) { return *this = *this % op; } // modulus and assign

    //
    // Comparison operators.  Recall that NaN does not compare, and 0.0 == -0.0
    //
    bool operator== (const IEEEfloat) const; // equal
    bool operator!= (const IEEEfloat) const; // not equal
    bool operator<  (const IEEEfloat) const; // less-than
    bool operator>  (const IEEEfloat) const; // greater-than
    bool operator<= (const IEEEfloat) const; // less-than or equal
    bool operator>= (const IEEEfloat) const; // greater-than or equal

    //
    // Methods for hashing floats, behave like java.lang.Float counterparts:
    //  * -0.0f and 0.0f are different, with positive 0 comparing as greater
    //    than negative 0.
    //  * identical bit patterns of NaN are the same in equals, but not
    //    unique patterns
    //  * all bit patterns of NaN compare as greater than any other float,
    //    including positive infinity, but the same as any other NaN pattern
    //
    inline bool equals(const IEEEfloat op) const { return value.word == op.value.word; }
    inline i4 hashCode(void) const { return value.iword; }
    inline int compareTo(const IEEEfloat op) const
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
    friend class DoubleToString;

    //
    // two enums, so that first is signed, and second may be unsigned
    //
    enum
    {
        FRACT_SIZE    = 52,         // number of bits in mantissa
        FRACT_SIZE_HI = FRACT_SIZE - 32, // mantissa bits in high word
        BIAS          = 1023,       // exponent bias
        MAX_DEC_EXP   = 308,        // maximum decimal exponent
        MAX_DIGITS    = BIAS + FRACT_SIZE + 1 - MAX_DEC_EXP // maximum digits in precise decimal
    };
    enum
    {
        EXP_BITS      = 0x7FF00000, // exponent bit mask
        FRACT_BITS    = 0x000FFFFF, // mantissa bit mask
        ABS_BITS      = EXP_BITS | FRACT_BITS, // absolute value
        MAX_FRACT     = 0x00200000, // upper limit of mantissa
        MIN_FRACT     = 0x00100000, // lower limit of mantissa
        NEG_ZERO_HI   = 0x80000000, // -0.0
        POS_ZERO_HI   = 0x00000000, // 0.0
        NEG_INF_HI    = 0xFFF00000, // -Inf
        POS_INF_HI    = 0x7FF00000, // +Inf
        NAN_HI        = 0x7FF80000, // canonical NaN
        ZERO_LO       = 0x00000000 // low half of special values above
    };

    inline u4 SignBit(void) const   { return HighWord() & SIGN_BIT; }
    inline u4 ExpBits(void) const   { return HighWord() & EXP_BITS; }
    inline u4 FractBits(void) const { return HighWord() & FRACT_BITS; }

    // takes the (possibly) unnormalized fraction with its corresponding
    // exponent and sign, and creates the normalized double
    static IEEEdouble Normalize(int, int, ULongInt);
    // takes the double and splits it into a normalized fraction and exponent;
    // the exponent is returned, the parameter fraction is modified
    int SplitInto(BaseLong &fraction) const;

    // returns the value of 1 ulp (unit in last place) for this double
    IEEEdouble Ulp(void) const;
    // returns the ratio between two doubles represented in BigInts
    static IEEEdouble Ratio(const BigInt &a, const BigInt &b);


#ifndef HAVE_MEMBER_CONSTANTS
    // VC++ can't cope with constant class members
    static IEEEdouble tens[]; // powers of 10 exactly represented in float
    static IEEEdouble bigtens[]; // powers of 10 by powers of 2
#else
    static const IEEEdouble tens[]; // powers of 10 exactly represented in float
    static const IEEEdouble bigtens[]; // powers of 10 by powers of 2
#endif

public:
    //
    // Information methods, for evaluating components of the float
    //
    inline int     Sign(void) const     { return HighWord() >> 31; }
    inline int     Exponent(void) const { return ((HighWord() & EXP_BITS) >> (FRACT_SIZE_HI)) - BIAS; }
    inline LongInt Fraction(void) const
    {
        return LongInt((HighWord() & FRACT_BITS) | (ExpBits() ? MIN_FRACT : 0),
                       LowWord()); 
    }
    static inline int Bias(void)      { return BIAS; }
    static inline int FractSize(void) { return FRACT_SIZE; }

    inline bool IsNaN(void) const
    {
        // optimized for no branching, idea from fdlibm.c
        u4 high = HighWord(),
            low = LowWord();
        return ((high & ABS_BITS) | ((low | -(i4) low) >> 31)) > EXP_BITS;
    }

    inline bool IsNegative(void) const { return ((i4) HighWord() < 0) && !IsNaN(); }
    inline bool IsPositive(void) const { return ((i4) HighWord() >= 0) && !IsNaN(); }

    inline bool IsNegativeZero(void) const
    {
        return (HighWord() == NEG_ZERO_HI) && (LowWord() == ZERO_LO);
    }
    inline bool IsPositiveZero(void) const
    {
        return (HighWord() == POS_ZERO_HI) && (LowWord() == ZERO_LO);
    }
    inline bool IsZero(void) const
    {
        return ((HighWord() & ABS_BITS) == POS_ZERO_HI) && (LowWord() == ZERO_LO);
    }

    inline bool IsNegativeInfinity(void) const
    {
        return (HighWord() == NEG_INF_HI) && (LowWord() == ZERO_LO);
    }
    inline bool IsPositiveInfinity(void) const
    {
        return (HighWord() == POS_INF_HI) && (LowWord() == ZERO_LO);
    }
    inline bool IsInfinite(void) const
    {
        return ((HighWord() & ABS_BITS) == POS_INF_HI) && (LowWord() == ZERO_LO);
    }


    //
    // Generation methods, for creating common constants
    // TODO: These methods should be converted to return static const
    // fields, rather than generating a non-const every time, as in
    // BaseLong (long.h).  However, adding const support is a big undertaking.
    //
    static inline const IEEEdouble NaN(void)               { return IEEEdouble(NAN_HI, ZERO_LO); }
    static inline const IEEEdouble POSITIVE_INFINITY(void) { return IEEEdouble(POS_INF_HI, ZERO_LO); }
    static inline const IEEEdouble NEGATIVE_INFINITY(void) { return IEEEdouble(NEG_INF_HI, ZERO_LO); }
    static inline const IEEEdouble POSITIVE_ZERO(void)     { return IEEEdouble(POS_ZERO_HI, ZERO_LO); }
    static inline const IEEEdouble NEGATIVE_ZERO(void)     { return IEEEdouble(NEG_ZERO_HI, ZERO_LO); }


    //
    // Constructors
    //
    // Create a double from the given value
    inline IEEEdouble(double d) { value.double_value = d; }
    // Convert string to double, the double is NaN if check_invalid is true and input is invalid by JLS
           IEEEdouble(const char *, bool check_invalid = false);

    // widening conversion of int to double, no information lost
           IEEEdouble(i4);
    // Widening conversion of long to double, may lose precision
           IEEEdouble(const LongInt &);
    // Widening conversion of float to double, no information lost
           IEEEdouble(const IEEEfloat &);
    // Create a double without initializing it
    inline IEEEdouble(void) {}
    // Load a specified bit pattern (contrast to IEEEfloat(LongInt))
    inline IEEEdouble(u4 hi, u4 lo) { setHighAndLowWords(hi, lo); }

    //
    // Conversion routines
    //
           i4        IntValue(void) const;
           LongInt   LongValue(void) const;
    inline IEEEfloat FloatValue(void) const { return IEEEfloat(*this); }


    //
    // Floating-point operations
    // TODO: add const reference versions
    //
           IEEEdouble  operator+  (const IEEEdouble) const;   // binary addition
    inline IEEEdouble  operator+  (void) const          { return *this; } // unary plus
    inline IEEEdouble &operator+= (const IEEEdouble op) { return *this = *this + op; } // add and assign
    inline IEEEdouble  operator++ (void)          { return *this += 1; } // pre-increment
    inline IEEEdouble  operator++ (int dummy)
    {
        IEEEdouble result = *this;
        *this += 1;
        return result;
    } // post-increment

           IEEEdouble  operator-  (void) const;         // unary minus
    inline IEEEdouble  operator-  (const IEEEdouble op) const { return *this + (-op); } // binary subtraction
    inline IEEEdouble &operator-= (const IEEEdouble op) { return *this = *this - op; } // subtract and assign
    inline IEEEdouble  operator-- (void)          { return *this -= 1; } // pre-decrement
    inline IEEEdouble  operator-- (int dummy)
    {
        IEEEdouble result = *this;
        *this -= 1;
        return result;
    } // post-decrement

           IEEEdouble  operator*  (const IEEEdouble) const;   // multiplication
    inline IEEEdouble &operator*= (const IEEEdouble op) { return *this = *this * op; } // multiply and assign

           IEEEdouble  operator/  (const IEEEdouble) const;   // divide
    inline IEEEdouble &operator/= (const IEEEdouble op) { return *this = *this / op; } // divide and assign

           IEEEdouble  operator%  (const IEEEdouble) const;   // modulus
    inline IEEEdouble &operator%= (const IEEEdouble op) { return *this = *this % op; } // modulus and assign

    //
    // Comparison operators.  Recall that NaN does not compare, and 0.0 == -0.0
    //
    bool operator== (const IEEEdouble) const; // equal
    bool operator!= (const IEEEdouble) const; // not equal
    bool operator<  (const IEEEdouble) const; // less-than
    bool operator>  (const IEEEdouble) const; // greater-than
    bool operator<= (const IEEEdouble) const; // less-than or equal
    bool operator>= (const IEEEdouble) const; // greater-than or equal

    //
    // Methods for hashing doubles, behave like java.lang.Double counterparts:
    //  * -0.0 and 0.0 are different, with positive 0 comparing as greater
    //    than negative 0.
    //  * identical bit patterns of NaN are the same in equals, but not
    //    unique patterns
    //  * all bit patterns of NaN compare as greater than any other float,
    //    including positive infinity, but the same as any other NaN pattern
    //
    inline bool equals(const IEEEdouble op) const { return (BaseLong) *this == (BaseLong) op; }
    inline i4 hashCode(void) const { return ((BaseLong) *this).hashCode(); }
    inline int compareTo(const IEEEdouble op) const
    {
        return IsNaN() ? 1
                       : (IsZero() && op.IsZero()) ? op.Sign() - Sign()
                                                   : (*this < op) ? -1
                                                                  : *this > op;
    }        
};


//
// Helper class for storing precise decimal values of floating point
// when converting to or from decimal strings.
//
class BigInt
{
public:
    // init with value, set exponent and bit count
    BigInt(const IEEEfloat &, int &exp, int &bitcnt);
    BigInt(const IEEEdouble &, int &exp, int &bitcnt);
    // init with int
    inline BigInt(int);
    // init with value of string; where before is the number of digits
    // before the '.', total is the number of digits to parse, start
    // is the previously parsed value of the first few digits, and
    // startsize gives the number of digits previously parsed
    BigInt(const char *, int before, int total, u4 start, int startsize);
    // copy constructor and assignment operator
    BigInt &operator =(const BigInt &);
    BigInt(const BigInt &op) { *this = op; }
    // destructor
    ~BigInt()
    {
        delete data;
    }

private:
    // resize data[] to be 1<<k elements
    inline void resize(int k);

    // return count of high-order 0's in x
    static int hi0bits(u4 x);
    // return count of low-order 0's in y, shift y so that
    // least significant 1 is at right
    static int lo0bits(u4 &y);

public:
    // return number of leading 0's in most significant word of data
    inline int hi0bits() const { assert(data); return hi0bits(data[wds - 1]); }

    // return true if this is 0
    inline bool IsZero() const { assert(data); return ! (data[0] || wds > 1); }

    // return true if this is negative
    inline bool IsNegative() const { return neg; }
    // set whether this is negative
    inline void IsNegative(bool value) { neg = value; }

    // operators
           BigInt &operator +  (const unsigned op) const;
    inline BigInt &operator += (const unsigned op) { return *this = *this + op; }
    inline BigInt &operator ++ (void) { return *this += 1; } // pre-increment
           BigInt &operator -  (const BigInt &op) const;
           BigInt &operator *  (const BigInt &op) const;
           BigInt &operator *  (unsigned op) const;
    inline BigInt &operator *= (const BigInt &op) { return *this = *this * op; }
    inline BigInt &operator *= (unsigned op) { return *this = *this * op; }
           BigInt &operator << (unsigned op) const;
    inline BigInt &operator <<=(unsigned op) { return *this = *this << op; }

    // equivalent to *this = *this * m + a, with less work
    BigInt &multadd(unsigned m, unsigned a);
    // return *this *= pow(5, k)
    BigInt &pow5mult(unsigned k);
    // return *this == b ? 0 : *this < b ? negative : positive;
    int compareTo(const BigInt &b) const;
    // tmp = *this % S; *this /= S; return tmp;
    int quorem(const BigInt &S);

    // converts to scaled native value, between 1 and 2.
    IEEEfloat FloatValue() const;
    IEEEdouble DoubleValue() const;

private:
    friend IEEEfloat IEEEfloat::Ratio(const BigInt &, const BigInt &);
    friend IEEEdouble IEEEdouble::Ratio(const BigInt &, const BigInt &);

    int k;      // log2 maxwds
    int maxwds; // size of data[]
    bool neg;   // true for negative
    int wds;    // current memory use of data[]
    u4 *data;   // bit storage

#ifndef HAVE_MEMBER_CONSTANTS
    static u4 fives[];          // powers of 5 that fit in int
    static BigInt *bigfives[];  // bigger powers of 5, by powers of 2
#else /* HAVE_MEMBER_CONSTANTS */
    static const u4 fives[];
    static const BigInt *bigfives[];
#endif /* HAVE_MEMBER_CONSTANTS */

};


inline IEEEdouble IEEEfloat::DoubleValue() const
{
    return IEEEdouble(*this);
}

inline BigInt::BigInt(int i) : data(NULL)
{
    resize(0);
    data[0] = i;
    wds = 1;
    neg = i < 0;
}

inline void BigInt::resize(int k)
{
    maxwds = 1 << k;
    delete data;
    data = new u4[maxwds];
    this -> k = k;
    wds = 0;
    neg = false;
}

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif // Double_INCLUDED

