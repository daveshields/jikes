// $Id: long.h,v 1.21 2001/09/21 05:06:07 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef long_INCLUDED
#define long_INCLUDED

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class IEEEdouble;
class IEEEfloat;

class LongInt;
class ULongInt;

class BaseLong
{
protected:
    enum 
    {
        MAX_INT    = 0x7FFFFFFF, // max value of i4
        MIN_INT    = 0x80000000, // min value of i4
        SHORT_MASK = 0xFFFF,     // mask for lower half of i4
        SIGN_BIT   = 0x80000000 // sign bit
    };

    union
    {
        double double_value;
#ifndef HAVE_64BIT_TYPES
        u4 word[2];
#else
        u8 words;
#endif // HAVE_64BIT_TYPES
    } value;

    // Set the high word only. Does not modify the low word!
    inline void setHighWord(u4 high) {
#ifndef HAVE_64BIT_TYPES
# ifndef WORDS_BIGENDIAN
        value.word[1] = high;
# else
        value.word[0] = high;
# endif
#else
        setHighAndLowWords(high, LowWord());
#endif // HAVE_64BIT_TYPES
    }

    // Set the low word only. Does not modify the high word!
    inline void setLowWord(u4 low) {
#ifndef HAVE_64BIT_TYPES
# ifndef WORDS_BIGENDIAN
        value.word[0] = low;
# else
        value.word[1] = low;
# endif
#else
        setHighAndLowWords(HighWord(), low);
#endif // HAVE_64BIT_TYPES
    }
    
    // Set the value for both words.
    inline void setHighAndLowWords(u4 high, u4 low) {
#ifndef HAVE_64BIT_TYPES
# ifndef WORDS_BIGENDIAN
        value.word[1] = high;
        value.word[0] = low;
# else
        value.word[0] = high;
        value.word[1] = low;
# endif
#else
        value.words = (((u8) high) << 32) | low;
#endif // HAVE_64BIT_TYPES
    }

    // Set the value for both words.
    inline void setHighAndLowWords(const BaseLong &op)
    {
#ifndef HAVE_64BIT_TYPES
        value.word[0] = op.value.word[0];
        value.word[1] = op.value.word[1];
#else
        value.words = op.value.words;
#endif // HAVE_64BIT_TYPES
    }

public:

#ifndef HAVE_64BIT_TYPES
# ifndef WORDS_BIGENDIAN
    inline u4 HighWord(void) const { return value.word[1]; }
    inline u4 LowWord(void) const  { return value.word[0]; }
# else
    inline u4 HighWord(void) const { return value.word[0]; }
    inline u4 LowWord(void) const  { return value.word[1]; }
# endif
#else
    inline u4 HighWord(void) const { return ((u4) (value.words >> 32)); }
    inline u4 LowWord(void) const  { return ((u4) value.words); }
    inline u8 Words(void) const    { return value.words; }
#endif // HAVE_64BIT_TYPES

    double DoubleView(void) const { return value.double_value; }

    BaseLong(u4 high, u4 low);
    BaseLong(u4 a); // no sign extension
    BaseLong(i4 a); // sign extends
    inline BaseLong (void) {} // construct without initializing
#ifdef HAVE_64BIT_TYPES
    inline BaseLong(u8 a) { value.words = a; } // construct in one step
#endif // HAVE_64BIT_TYPES

    BaseLong  operator+  (const BaseLong) const; // binary addition
    BaseLong  operator+  (void) const;     // unary plus
    BaseLong &operator+= (const BaseLong); // add and assign
    BaseLong  operator++ (int);      // postfix increment
    BaseLong  operator++ (void);     // prefix increment

    BaseLong  operator-  (const BaseLong) const; // binary subtraction
    BaseLong  operator-  (void) const;     // unary minus
    BaseLong &operator-= (const BaseLong); // subtract and assign
    BaseLong  operator-- (int);      // postfix decrement
    BaseLong  operator-- (void);     // prefix decrement

    BaseLong  operator*  (const BaseLong) const; // multiplication
    BaseLong &operator*= (const BaseLong); // multiply and assign

    //
    // NOTE: To match the JLS, mask the argument with
    // Semantic::LONG_SHIFT_MASK (0x3f)
    //
    BaseLong  operator<< (int) const;      // left shift
    BaseLong &operator<<=(int);      // left shift and assign

    bool      operator== (const BaseLong) const; // equal
    bool      operator!= (const BaseLong) const; // not equal
    bool      operator!  (void) const;     // logical complement

    BaseLong  operator~  (void) const;     // bitwise complement
    BaseLong  operator^  (const BaseLong) const; // bitwise XOR
    BaseLong &operator^= (const BaseLong); // bitwise XOR and assign
    BaseLong  operator|  (const BaseLong) const; // bitwise OR
    BaseLong &operator|= (const BaseLong); // bitwise OR and assign
    BaseLong  operator&  (const BaseLong) const; // bitwise AND
    BaseLong &operator&= (const BaseLong); // bitwise AND and assign

    bool      operator&& (const BaseLong) const; // logical AND (not short-circuit)
    bool      operator|| (const BaseLong) const; // logical OR (not short circuit)

    static void Divide(const BaseLong &, const BaseLong &, BaseLong &, BaseLong &);

    operator LongInt(void) const;          // Cast to LongInt
    operator ULongInt(void) const;         // Cast to ULongInt

    // mirrors java.lang.Long, useful in hashing
    inline i4 hashCode(void) const { return (HighWord() ^ LowWord()); }
};


class LongInt : public BaseLong
{
public:

    inline LongInt (u4 a, u4 b) : BaseLong (a, b) {}
    inline LongInt (u4 a) : BaseLong (a) {} // no sign extension
    inline LongInt (i4 a) : BaseLong (a) {} // sign extends
    inline LongInt (void) : BaseLong () {} // uninitialized
#ifdef HAVE_EXPLICIT
    explicit
#endif
           LongInt (const IEEEdouble &); // narrowing conversion of double to long

#ifdef HAVE_EXPLICIT
    explicit 
#endif
           LongInt (const IEEEfloat &); // narrowing conversion of float to long
#ifdef HAVE_64BIT_TYPES
    inline LongInt(u8 a) : BaseLong (a) {} // construct in one step
#endif // HAVE_64BIT_TYPES

    //
    // These constants are generated when first used.  The memory they
    // use can be reclaimed with ConstantCleanup().
    //
    static inline const LongInt MAX_LONG(void)
    {
        return max_long_const ? *max_long_const
            : *(max_long_const = new LongInt(0x7FFFFFFF, 0xFFFFFFFF));
    }
    static inline const LongInt MIN_LONG(void)
    {
        return min_long_const ? *min_long_const
            : *(min_long_const = new LongInt(0x80000000, 0x00000000));
    }
    static void ConstantCleanup(void)
    {
        // The explicit casts are there to workaround a MSVC
        // behaviour which doesn't allow an implicit cast
        // from const X* to void* for the delete operator.
        if (max_long_const)
            delete (LongInt*)max_long_const;
        if (min_long_const)
            delete (LongInt*)min_long_const;
    }


private:
    static const LongInt *max_long_const, *min_long_const;

public:
    LongInt  operator/  (const LongInt) const; // divide
    LongInt &operator/= (const LongInt); // divide and assign

    LongInt  operator%  (const LongInt) const; // modulus
    LongInt &operator%= (const LongInt); // modulus and assign

    //
    // NOTE: To match the JLS, mask the argument with
    // Semantic::LONG_SHIFT_MASK (0x3f)
    //
    LongInt  operator>> (int) const;     // right shift
    LongInt &operator>>=(int);     // right shift and assign

    bool     operator<  (const LongInt) const; // less-than
    bool     operator>  (const LongInt) const; // greater-than
    bool     operator<= (const LongInt) const; // less-than or equal
    bool     operator>= (const LongInt) const; // greater-than or equal
};


class ULongInt : public BaseLong
{
public:

    inline ULongInt (u4 a, u4 b) : BaseLong (a, b) {}
    inline ULongInt (u4 a) : BaseLong (a) {} // no sign extension
    inline ULongInt (i4 a) : BaseLong (a) {} // sign extended
    inline ULongInt (void) : BaseLong () {} // uninitialized
#ifdef HAVE_64BIT_TYPES
    inline ULongInt(u8 a) : BaseLong (a) {} // construct in one step
#endif // HAVE_64BIT_TYPES

    ULongInt  operator/  (const ULongInt) const; // divide
    ULongInt &operator/= (const ULongInt); // divide and assign

    ULongInt  operator%  (const ULongInt) const; // modulus
    ULongInt &operator%= (const ULongInt); // modulus and assign

    //
    // NOTE: To match the JLS, mask the argument with
    // Semantic::LONG_SHIFT_MASK (0x3f)
    //
    ULongInt  operator>> (int) const;      // right shift
    ULongInt &operator>>=(int);      // right shift and assign

    bool      operator<  (const ULongInt) const; // less-than
    bool      operator>  (const ULongInt) const; // greater-than
    bool      operator<= (const ULongInt) const; // less-than or equal
    bool      operator>= (const ULongInt) const; // greater-than or equal
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // long_INCLUDED
