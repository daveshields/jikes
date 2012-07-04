// $Id: long.h,v 1.15 2001/02/17 06:26:55 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef Long_INCLUDED
#define Long_INCLUDED

#include "platform.h"

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

class IEEEdouble;
class IEEEfloat;

class LongInt;
class ULongInt;

class BaseLong
{
protected:
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

    // Set the value for both words.
    inline void setHighAndLowWords(const BaseLong &op)
    {
#ifndef HAVE_UNSIGNED_LONG_LONG
        value.word[0] = op.value.word[0];
        value.word[1] = op.value.word[1];
#else
        value.words = op.value.words;
#endif // HAVE_UNSIGNED_LONG_LONG
    }

public:

#ifndef HAVE_UNSIGNED_LONG_LONG
# ifndef WORDS_BIGENDIAN
    inline u4 HighWord(void) { return value.word[1]; }
    inline u4 LowWord(void)  { return value.word[0]; }
# else
    inline u4 HighWord(void) { return value.word[0]; }
    inline u4 LowWord(void)  { return value.word[1]; }
# endif
#else
    inline u4 HighWord(void) { return ((u4) (value.words >> 32)); }
    inline u4 LowWord(void)  { return ((u4) value.words); }
    inline u8 Words(void)    { return value.words; }
#endif // HAVE_UNSIGNED_LONG_LONG

    double DoubleView(void) { return value.double_value; }

    BaseLong(u4 high, u4 low);
    BaseLong(u4 a); // no sign extension
    BaseLong(i4 a); // sign extends
    inline BaseLong (void) {} // construct without initializing
#ifdef HAVE_UNSIGNED_LONG_LONG
    inline BaseLong(u8 a) { value.words = a; } // construct in one step
#endif // HAVE_UNSIGNED_LONG_LONG

    BaseLong  operator+  (BaseLong); // binary addition
    BaseLong  operator+  (void);     // unary plus
    BaseLong &operator+= (BaseLong); // add and assign
    BaseLong  operator++ (int);      // postfix increment
    BaseLong  operator++ (void);     // prefix increment

    BaseLong  operator-  (BaseLong); // binary subtraction
    BaseLong  operator-  (void);     // unary minus
    BaseLong &operator-= (BaseLong); // subtract and assign
    BaseLong  operator-- (int);      // postfix decrement
    BaseLong  operator-- (void);     // prefix decrement

    BaseLong  operator*  (BaseLong); // multiplication
    BaseLong &operator*= (BaseLong); // multiply and assign

    //
    // NOTE: To match the JLS, mask the argument with 0x3f
    //
    BaseLong  operator<< (int);      // left shift
    BaseLong &operator<<=(int);      // left shift and assign

    bool      operator== (BaseLong); // equal
    bool      operator!= (BaseLong); // not equal
    bool      operator!  (void);     // logical complement

    BaseLong  operator~  (void);     // bitwise complement
    BaseLong  operator^  (BaseLong); // bitwise XOR
    BaseLong &operator^= (BaseLong); // bitwise XOR and assign
    BaseLong  operator|  (BaseLong); // bitwise OR
    BaseLong &operator|= (BaseLong); // bitwise OR and assign
    BaseLong  operator&  (BaseLong); // bitwise AND
    BaseLong &operator&= (BaseLong); // bitwise AND and assign

    bool      operator&& (BaseLong); // logical AND (not short-circuit)
    bool      operator|| (BaseLong); // logical OR (not short circuit)

    static void Divide(BaseLong &, BaseLong &, BaseLong &, BaseLong &);

    operator LongInt(void);          // Cast to LongInt
    operator ULongInt(void);         // Cast to ULongInt

    // mirrors java.lang.Long, useful in hashing
    inline i4 hashCode(void) { return (HighWord() ^ LowWord()); }
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
           LongInt (IEEEdouble); // narrowing conversion of double to long

#ifdef HAVE_EXPLICIT
    explicit 
#endif
    LongInt (IEEEfloat); // narrowing conversion of float to long
#ifdef HAVE_UNSIGNED_LONG_LONG
    inline LongInt(u8 a) : BaseLong (a) {} // construct in one step
#endif // HAVE_UNSIGNED_LONG_LONG

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
        if (max_long_const)
            delete max_long_const;
        if (min_long_const)
            delete min_long_const;
    }

private:
    static LongInt *max_long_const, *min_long_const;

public:
    LongInt  operator/  (LongInt); // divide
    LongInt &operator/= (LongInt); // divide and assign

    LongInt  operator%  (LongInt); // modulus
    LongInt &operator%= (LongInt); // modulus and assign

    //
    // NOTE: To match the JLS, mask the argument with 0x3f
    //
    LongInt  operator>> (int);     // right shift
    LongInt &operator>>=(int);     // right shift and assign

    bool     operator<  (LongInt); // less-than
    bool     operator>  (LongInt); // greater-than
    bool     operator<= (LongInt); // less-than or equal
    bool     operator>= (LongInt); // greater-than or equal
};


class ULongInt : public BaseLong
{
public:

    inline ULongInt (u4 a, u4 b) : BaseLong (a, b) {}
    inline ULongInt (u4 a) : BaseLong (a) {} // no sign extension
    inline ULongInt (i4 a) : BaseLong (a) {} // sign extended
    inline ULongInt (void) : BaseLong () {} // uninitialized
#ifdef HAVE_UNSIGNED_LONG_LONG
    inline ULongInt(u8 a) : BaseLong (a) {} // construct in one step
#endif // HAVE_UNSIGNED_LONG_LONG

    ULongInt  operator/  (ULongInt); // divide
    ULongInt &operator/= (ULongInt); // divide and assign

    ULongInt  operator%  (ULongInt); // modulus
    ULongInt &operator%= (ULongInt); // modulus and assign

    //
    // NOTE: To match the JLS, mask the argument with 0x3f
    //
    ULongInt  operator>> (int);      // right shift
    ULongInt &operator>>=(int);      // right shift and assign

    bool      operator<  (ULongInt); // less-than
    bool      operator>  (ULongInt); // greater-than
    bool      operator<= (ULongInt); // less-than or equal
    bool      operator>= (ULongInt); // greater-than or equal
};

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif // Long_INCLUDED
