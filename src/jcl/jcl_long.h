// $Id: jcl_long.h,v 1.1 1999/11/04 18:48:04 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_long_INCLUDED
#define jcl_ong_INCLUDED

#include <math.h>
#include "jcl_bool.h"
#include "jcl_int.h"

class Long;
class ULong;

class BaseLong
{
protected:
    union
    {
        double double_words;
        u4 word[2];
    } value;

public:

#ifdef BIGENDIAN
    u4 &high_word() { return value.word[0]; }
    u4 &low_word()  { return value.word[1]; }
#else
    u4 &low_word()  { return value.word[0]; }
    u4 &high_word() { return value.word[1]; }
#endif

    double &DoubleView() { return value.double_words; }

    inline BaseLong(u4 a, u4 b)
    {
        high_word() = a;
        low_word() = b;
    }

    inline BaseLong(u4 a)
    {
        high_word() = 0;
        low_word() = a;
    }

    inline BaseLong(i4 a)
    {
        low_word() = a;
        //
        // Since the carry is not guaranteed to ripple, we cannot use this code.
        //
        //        high_word() = a >> 31;
        //
        high_word() = (a < 0 ? 0xFFFFFFFF : 0x00000000);
    }

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

    static void divide(BaseLong, BaseLong, BaseLong &, BaseLong &);

    operator Long();                 // Cast to Long
    operator ULong();                // Cast to ULong
};


class Long : public BaseLong
{
public:

    inline Long (u4 a, u4 b) : BaseLong (a, b) {}
    inline Long (u4 a) : BaseLong (a) {}
    inline Long (i4 a) : BaseLong (a) {}
    inline Long (void) : BaseLong () {}
    inline Long (double);

    Long  operator/  (Long); // divide
    Long& operator/= (Long); // divide and assign

    Long  operator%  (Long); // modulus
    Long& operator%= (Long); // modulus and assign

    Long  operator>> (Long); // right shift
    Long& operator>>=(Long); // right shift and assign

    bool  operator<  (Long); // less-than
    bool  operator>  (Long); // greater-than
    bool  operator<= (Long); // less-than or equal
    bool  operator>= (Long); // greater-than or equal

    double Double();         // convert Long value to a double value
    void String(char *);     // convert Long value to its character string representation
};


class ULong : public BaseLong
{
public:

    inline ULong (u4 a, u4 b) : BaseLong (a, b) {}
    inline ULong (u4 a) : BaseLong (a) {}
    inline ULong (i4 a) : BaseLong (a) {}
    inline ULong (void) : BaseLong () {}
    inline ULong (double);

    ULong  operator/  (ULong); // divide
    ULong& operator/= (ULong); // divide and assign

    ULong  operator%  (ULong); // modulus
    ULong& operator%= (ULong); // modulus and assign

    ULong  operator>> (ULong); // right shift
    ULong& operator>>=(ULong); // right shift and assign

    bool   operator<  (ULong); // less-than
    bool   operator>  (ULong); // greater-than
    bool   operator<= (ULong); // less-than or equal
    bool   operator>= (ULong); // greater-than or equal

    double Double();           // convert Long value to a double value
    void String(char *);       // convert Long value to its character string representation
};


inline BaseLong::operator Long()
{
    return Long(high_word(), low_word());
}

inline BaseLong::operator ULong()
{
    return ULong(high_word(), low_word());
}

inline bool BaseLong::operator== (BaseLong op)
{
    return ((high_word() == op.high_word()) && (low_word() == op.low_word()));
}

inline bool BaseLong::operator!= (BaseLong op)
{
    return ((high_word() != op.high_word()) || (low_word() != op.low_word()));
}

inline bool BaseLong::operator!()
{
    return (*this == 0);
}

inline BaseLong BaseLong::operator~()
{
    return BaseLong(~high_word(), ~low_word());
}

inline BaseLong BaseLong::operator^ (BaseLong op)
{
    return BaseLong(high_word() ^ op.high_word(), low_word() ^ op.low_word());
}

inline BaseLong& BaseLong::operator^= (BaseLong op)
{
    *this = *this ^ op;
    return *this;
}

inline BaseLong BaseLong::operator| (BaseLong op)
{
    return BaseLong(high_word() | op.high_word(), low_word() | op.low_word());
}

inline BaseLong& BaseLong::operator|= (BaseLong op)
{
    *this = *this | op;
    return *this;
}

inline BaseLong BaseLong::operator& (BaseLong op)
{
    return BaseLong(high_word() & op.high_word(), low_word() & op.low_word());
}

inline BaseLong& BaseLong::operator&= (BaseLong op)
{
    *this = *this & op;
    return *this;
}

inline bool BaseLong::operator&& (BaseLong op)
{
    return (*this != 0) && (op != 0);
}

inline bool BaseLong::operator|| (BaseLong op)
{
    return (*this != 0) || (op != 0);
}

inline BaseLong BaseLong::operator<< (BaseLong op)
{
    u4 n = op.low_word(); // Always treat this value as positive, since negative values are not allowed

    //
    // Note that this function assumes that for two 32-bit integers
    // x << y, where y = 0, is well-defined and that the result is
    // the value x. This is true in Ansi-C and C++ but not true in
    // old versions of C (See Kernighan and Ritchie).
    // Note also that if y >= 32 then the result is unpredictable. On Aix,
    // xlC will produce the result 0(good!) whereas on windows the Microsoft
    // compiler produces the value of x(very bad !).
    //
    return (n < 32 ? BaseLong((high_word() << n) | (low_word() >> (32 - n)), low_word() << n)
                   : BaseLong(low_word() << (n - 32), 0));
}

inline BaseLong& BaseLong::operator<<= (BaseLong op)
{
    *this = *this << op;
    return *this;
}

inline BaseLong BaseLong::operator+ (BaseLong op)
{
    u4 ushort1 = (low_word() & 0xFFFF) + (op.low_word() & 0xFFFF),
       ushort2 = (ushort1 >> 16) + (low_word() >> 16) + (op.low_word() >> 16),
       ushort3 = (ushort2 >> 16) + (high_word() & 0xFFFF) + (op.high_word() & 0xFFFF),
       ushort4 = (ushort3 >> 16) + (high_word() >> 16) + (op.high_word() >> 16);

    return BaseLong((ushort3 & 0xFFFF) | (ushort4 << 16), (ushort1 & 0xFFFF) | (ushort2 << 16));
}

inline BaseLong& BaseLong::operator+= (BaseLong op)
{
    *this = *this + op;
    return *this;
}

inline BaseLong BaseLong::operator++ (int dummy)
{
    BaseLong temp = *this;
    *this += 1;
    return temp;
}

inline BaseLong BaseLong::operator++ ()
{
    *this += 1;
    return *this;
}

inline BaseLong BaseLong::operator- ()
{
    return ~(*this) + 1;
}

inline BaseLong BaseLong::operator- (BaseLong op)
{
    return *this + (-op);
}

inline BaseLong& BaseLong::operator-= (BaseLong op)
{
    *this = *this - op;
    return *this;
}

inline BaseLong BaseLong::operator-- (int dummy)
{
    BaseLong temp = *this;
    *this -= 1;
    return temp;
}

inline BaseLong BaseLong::operator-- ()
{
    *this -= 1;
    return *this;
}

inline BaseLong BaseLong::operator* (BaseLong op)
{
    u4 x0 = this -> low_word()   & 0xFFFF,
       x1 = this -> low_word()  >> 16,
       x2 = this -> high_word()  & 0xFFFF,
       x3 = this -> high_word() >> 16;

    u4 y0 = op.low_word()   & 0xFFFF,
       y1 = op.low_word()  >> 16,
       y2 = op.high_word()  & 0xFFFF,
       y3 = op.high_word() >> 16;

    return (BaseLong(0, x0 * y0)) +
           (BaseLong(0, x0 * y1) << (1 << 4)) +
           (BaseLong(0, x0 * y2) << (2 << 4)) +
           (BaseLong(0, x0 * y3) << (3 << 4)) +

           (BaseLong(0, x1 * y0) << (1 << 4)) +
           (BaseLong(0, x1 * y1) << (2 << 4)) +
           (BaseLong(0, x1 * y2) << (3 << 4)) +
           (BaseLong(0, x1 * y3) << (4 << 4)) +

           (BaseLong(0, x2 * y0) << (2 << 4)) +
           (BaseLong(0, x2 * y1) << (3 << 4)) +
           (BaseLong(0, x2 * y2) << (4 << 4)) +
           (BaseLong(0, x2 * y3) << (5 << 4)) +

           (BaseLong(0, x3 * y0) << (3 << 4)) +
           (BaseLong(0, x3 * y1) << (4 << 4)) +
           (BaseLong(0, x3 * y2) << (5 << 4)) +
           (BaseLong(0, x3 * y3) << (6 << 4));
}

inline BaseLong& BaseLong::operator*= (BaseLong op)
{
    *this = *this * op;
    return *this;
}

inline Long Long::operator/ (Long op)
{
    bool negative_dividend = high_word() & 0x80000000,
         negative_divisor  = op.high_word() & 0x80000000;

    BaseLong a = (negative_dividend ? -(*this) : (BaseLong) *this),
             b = (negative_divisor  ? -(op)    : (BaseLong) op),
             quotient,
             remainder;

    divide(a, b, quotient, remainder);

    return (negative_dividend ^ negative_divisor ? -quotient : quotient);
}

inline Long& Long::operator/= (Long op)
{
    *this = *this / op;
    return *this;
}

inline Long Long::operator% (Long op)
{
    bool negative_dividend = high_word() & 0x80000000,
         negative_divisor  = op.high_word() & 0x80000000;

    BaseLong a = (negative_dividend ? -(*this) : (BaseLong) *this),
             b = (negative_divisor  ? -(op)    : (BaseLong) op),
             quotient,
             remainder;

    divide(a, b, quotient, remainder);

    return (negative_dividend ? -remainder : remainder);
}

inline Long& Long::operator%= (Long op)
{
    *this = *this % op;
    return *this;
}

inline Long Long::operator>> (Long op)
{
    u4 n = op.low_word(); // Always treat this value as positive, since negative values are not allowed

    //
    // Note that this function assumes that for two 32-bit integers
    // x >> y, where y = 0, is well-defined and that the result is
    // the value x. This is true in Ansi-C and C++ but not true in
    // old versions of C (See Kernighan and Ritchie).
    //
    // Note also that if y >= 32 then the result is unpredictable. The xlC compiler
    // on Aix will produce the result 0(good!) whereas on windows the Microsoft
    // C++ compiler produces the value of x(very bad !).
    //
    // Finally, note that the right-shitfting of the high_word is not guaranteed
    // to ripple the carry bit. Whether or not the carry-bit is rippled is
    // implementation-dependent. Therefore, this implementation is designed to
    // shift the "long" quantity in a similar manner as the system (compiler + environement)
    // used to compile it would shift a 32-bit signed integer.
    //
    return (n < 32 ? Long(((i4) high_word()) >> n, (high_word() << (32 - n)) | (low_word() >> n))
                   : Long(((i4) high_word()) >> 31, ((i4) high_word()) >> (n - 32)));
}

inline Long& Long::operator>>= (Long op)
{
    *this = *this >> op;
    return *this;
}

inline bool Long::operator< (Long op)
{
    return (high_word() == op.high_word() ? low_word() < op.low_word() : (i4) high_word() < (i4) op.high_word());
}

inline bool Long::operator<= (Long op)
{
    return (high_word() == op.high_word() ? low_word() <= op.low_word() : (i4) high_word() <= (i4) op.high_word());
}

inline bool Long::operator> (Long op)
{
    return (high_word() == op.high_word() ? low_word() > op.low_word() : (i4) high_word() > (i4) op.high_word());
}

inline bool Long::operator>= (Long op)
{
    return (high_word() == op.high_word() ? low_word() >= op.low_word() : (i4) high_word() >= (i4) op.high_word());
}

inline Long::Long(double a) : BaseLong (0,0)
{
    double b = floor(a < 0.0 ? -a : a);
    Long multiplier = 1;

    while (b > 0.0)
    {
        *this += (multiplier * (int) fmod(b, 10));
        b /=  10.0;
        multiplier *= 10;
    }

    if (a < 0.0)
        *this = -(*this);
}

inline double Long::Double()
{
    double value = 0.0;
    Long num = *this;
    double multiplier = 1.0;

    while (num > 0)
    {
        value += (multiplier * (num % 10).low_word());
        num /= 10;
        multiplier *= 10.0;
    }

    return value;
}

inline ULong ULong::operator/ (ULong op)
{
    BaseLong quotient,
             remainder;

    divide(*this, op, quotient, remainder);

    return quotient;
}

inline ULong& ULong::operator/= (ULong op)
{
    *this = *this / op;
    return *this;
}

inline ULong ULong::operator% (ULong op)
{
    BaseLong quotient,
             remainder;

    divide(*this, op, quotient, remainder);

    return remainder;
}

inline ULong& ULong::operator%= (ULong op)
{
    *this = *this % op;
    return *this;
}

inline ULong ULong::operator>> (ULong op)
{
    u4 n = op.low_word(); // Always treat this value as positive, since negative values are not allowed

    //
    // Note that this function assumes that for two 32-bit integers
    // x >> y, where y = 0, is well-defined and that the result is
    // the value x. This is true in Ansi-C and C++ but not true in
    // old versions of C (See Kernighan and Ritchie).
    // Note also that if y >= 32 then the result is unpredictable. On Aix,
    // xlC will produce the result 0(good!) whereas on windows the Microsoft
    // compiler produces the value of x(very bad !).
    //
    return (n < 32 ? ULong(high_word() >> n, (high_word() << (32 - n)) | (low_word() >> n))
                   : ULong(0, high_word() >> (n - 32)));
}

inline ULong& ULong::operator>>= (ULong op)
{
    *this = *this >> op;
    return *this;
}

inline bool ULong::operator< (ULong op)
{
    return (high_word() == op.high_word() ? low_word() < op.low_word() : high_word() < op.high_word());
}

inline bool ULong::operator<= (ULong op)
{
    return (high_word() == op.high_word() ? low_word() <= op.low_word() : high_word() <= op.high_word());
}

inline bool ULong::operator> (ULong op)
{
    return (high_word() == op.high_word() ? low_word() > op.low_word() : high_word() > op.high_word());
}

inline bool ULong::operator>= (ULong op)
{
    return (high_word() == op.high_word() ? low_word() >= op.low_word() : high_word() >= op.high_word());
}

inline ULong::ULong(double a) : BaseLong(0,0)
{
    double b = floor(a < 0.0 ? -a : a);
    ULong multiplier = 1;

    while (b > 0.0)
    {
        *this += (multiplier * (int) fmod(b, 10));
        b /= 10.0;
        multiplier *= 10;
    }
}

inline double ULong::Double()
{
    double value = 0.0;
    ULong num = *this;
    double multiplier = 1.0;

    while (num > 0)
    {
        value += (multiplier * (num % 10).low_word());
        num /= 10;
        multiplier *= 10.0;
    }

    return value;
}

#endif
