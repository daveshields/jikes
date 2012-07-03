// $Id: long.h,v 1.11 2000/07/25 11:32:33 mdejong Exp $
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

#ifdef	HAVE_NAMESPACES
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
        double double_words;
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

    double DoubleView() { return value.double_words; }

    BaseLong(u4 high, u4 low);
    BaseLong(u4 a);
    BaseLong(i4 a);
    inline BaseLong (void) {}

    BaseLong  operator+  (BaseLong); // binary addition
    BaseLong  operator+  ();         // unary plus
    BaseLong& operator+= (BaseLong); // add and assign
    BaseLong  operator++ (int);      // postfix increment
    BaseLong  operator++ ();         // prefix increment

    BaseLong  operator-  (BaseLong); // binary subtraction
    BaseLong  operator-  ();         // unary minus
    BaseLong& operator-= (BaseLong); // subtract and assign
    BaseLong  operator-- (int);      // postfix decrement
    BaseLong  operator-- ();         // prefix decrement

    BaseLong  operator* (BaseLong);  // multiplication
    BaseLong& operator*=(BaseLong);  // multiply and assign

    BaseLong  operator<< (BaseLong); // left shift
    BaseLong& operator<<=(BaseLong); // left shift and assign

    bool      operator== (BaseLong); // equal
    bool      operator!= (BaseLong); // not equal
    bool      operator!  ();         // logical complement

    BaseLong  operator~  ();         // bitwise complement
    BaseLong  operator^  (BaseLong); // bitwise XOR
    BaseLong& operator^= (BaseLong); // bitwise XOR and assign
    BaseLong  operator|  (BaseLong); // bitwise OR
    BaseLong& operator|= (BaseLong); // bitwise OR and assign
    BaseLong  operator&  (BaseLong); // bitwise AND
    BaseLong& operator&= (BaseLong); // bitwise AND and assign

    bool      operator&& (BaseLong); // logical AND (not short-circuit)
    bool      operator|| (BaseLong); // logical OR (not short circuit)

    static void Divide(BaseLong, BaseLong, BaseLong &, BaseLong &);

    operator LongInt();                 // Cast to LongInt
    operator ULongInt();                // Cast to ULongInt
};


class LongInt : public BaseLong
{
public:

    inline LongInt (u4 a, u4 b) : BaseLong (a, b) {}
    inline LongInt (u4 a) : BaseLong (a) {}
    inline LongInt (i4 a) : BaseLong (a) {}
    inline LongInt (void) : BaseLong () {}
    LongInt (IEEEdouble) ; //BaseLong (a) {};
    LongInt (IEEEfloat) ; //BaseLong (a) {};

    LongInt  operator/  (LongInt); // divide
    LongInt& operator/= (LongInt); // divide and assign

    LongInt  operator%  (LongInt); // modulus
    LongInt& operator%= (LongInt); // modulus and assign

    LongInt  operator>> (LongInt); // right shift
    LongInt& operator>>=(LongInt); // right shift and assign

    bool  operator<  (LongInt); // less-than
    bool  operator>  (LongInt); // greater-than
    bool  operator<= (LongInt); // less-than or equal
    bool  operator>= (LongInt); // greater-than or equal

    double Double();           // convert ULongInt value to a double value
};


class ULongInt : public BaseLong
{
public:

    inline ULongInt (u4 a, u4 b) : BaseLong (a, b) {}
    inline ULongInt (u4 a) : BaseLong (a) {}
    inline ULongInt (i4 a) : BaseLong (a) {}
    inline ULongInt (void) : BaseLong () {}

    ULongInt  operator/  (ULongInt); // divide
    ULongInt& operator/= (ULongInt); // divide and assign

    ULongInt  operator%  (ULongInt); // modulus
    ULongInt& operator%= (ULongInt); // modulus and assign

    ULongInt  operator>> (ULongInt); // right shift
    ULongInt& operator>>=(ULongInt); // right shift and assign

    bool   operator<  (ULongInt); // less-than
    bool   operator>  (ULongInt); // greater-than
    bool   operator<= (ULongInt); // less-than or equal
    bool   operator>= (ULongInt); // greater-than or equal

    double Double();      // convert LongInt value to a double value
};

#ifdef	HAVE_NAMESPACES
}			// Close namespace Jikes block
#endif

#endif // Long_INCLUDED

