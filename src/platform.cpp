// $Id: platform.cpp,v 1.21 2001/09/14 05:31:34 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
//
// NOTE: The code for accurate conversions between floating point
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

#include "platform.h"
#include "long.h"
#include "double.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

// Define the PathSeparator() function with the proper
// impl for each platform.

#ifdef HAVE_PATHNAME_STYLE_DOS
char PathSeparator() { return U_SEMICOLON; } // ";"
#else
char PathSeparator() { return U_COLON; } // ":"
#endif


// Define the SystemMkdir() function with the proper
// impl for each platform.

int SystemMkdir(char *dirname) {

return

#ifdef HAVE_GLIBC_MKDIR
 mkdir(dirname, S_IRWXU | S_IRWXG | S_IRWXO);
#endif

#ifdef HAVE_LIBC5_MKDIR
 mkdir(dirname, S_IRWXU);
#endif

#ifdef HAVE_WIN32_MKDIR
 mkdir(dirname);
#endif

#ifdef HAVE_MAC_MKDIR
 mkdir(dirname,0);
#endif

}

//
// The configure script checks each of these to see if we need our own implementation
//

#ifndef HAVE_WCSLEN
size_t wcslen(const wchar_t *cs)
{
  int n = 0;
  while (*cs++)
    n++;

  return n;
}
#endif

#ifndef HAVE_WCSCPY
wchar_t *wcscpy(wchar_t *s, const wchar_t *ct)
{
  wchar_t *ptr;
  for (ptr = s; *ct; ptr++, ct++)
    *ptr = *ct;
  *ptr = U_NULL;

  return s;
}
#endif

#ifndef HAVE_WCSNCPY
wchar_t *wcsncpy(wchar_t *s, const wchar_t *ct, size_t n)
{
  wchar_t *ptr;
  for (ptr = s; *ct && n-- > 0; ptr++, ct++)
    *ptr = *ct;
  while (n-- > 0)
    *ptr++ = U_NULL;

  return s;
}
#endif

#ifndef HAVE_WCSCAT
wchar_t *wcscat(wchar_t *s, const wchar_t *ct)
{
  wchar_t *ptr = s;

  while (*ptr)
    ptr++;
  wcscpy(ptr, ct);

  return s;
}
#endif

#ifndef HAVE_WCSCMP
int wcscmp(const wchar_t *cs, const wchar_t *ct)
{
  while (*cs == *ct && *cs && *ct)
  {
    cs++;
    ct++;
  }

  return (*cs == *ct ? 0 : (*cs < *ct ? -1 : 1));
}
#endif

#ifndef HAVE_WCSNCMP
int wcsncmp(const wchar_t *cs, const wchar_t *ct, size_t n)
{
  while (*cs == *ct && *cs && *ct && n-- > 0)
  {
    cs++;
    ct++;
  }

  return (n <= 0 || *cs == *ct ? 0 : (*cs < *ct ? -1 : 1));
}
#endif


//
// If the system runs out of memory, this function is invoked
// This is tricky because VC++ on windows uses a non standard
// implementation of the set_new_handler function.
// 

#ifdef HAVE_VCPP_SET_NEW_HANDLER
int OutOfMemory(size_t)
#else
void OutOfMemory()
#endif
{
  fprintf(stderr, "***System Failure: Out of memory\n");
  exit(1);

#ifdef HAVE_VCPP_SET_NEW_HANDLER
  return 0;
#endif
}

void SetNewHandler()
{
#ifdef  HAVE_VCPP_SET_NEW_HANDLER
  _set_new_handler(OutOfMemory);
#else
  set_new_handler(OutOfMemory);
#endif
}


//
// When using the ICC compiler on Win95 or OS/2, we need to disable
// testing for various floating point exceptions. Default behavior
// was causing problems reading some standard class files.
//
// We obviously don't need this on AIX (non x86), which uses xlC/ICC           
//            
void FloatingPointCheck()
{
#ifdef HAVE_ICC_FP_BUGS
  _control87(EM_UNDERFLOW, EM_UNDERFLOW);
  _control87(EM_ZERODIVIDE, EM_ZERODIVIDE);
  _control87(EM_OVERFLOW, EM_OVERFLOW);
  _control87(EM_INVALID, EM_INVALID);
#endif
  return;
}


//
// This next set of functions may need some porting to work on various systems
//

int SystemStat(const char *name, struct stat *stat_struct)
{
  return stat(name, stat_struct);
}
FILE *SystemFopen(const char *name, const char *mode)
{
  return fopen(name, mode);
}
size_t SystemFread(char *ptr, size_t element_size, size_t count, FILE *stream)
{
  return fread(ptr, element_size, count, stream);
}
int SystemIsDirectory(char *name)
{
  struct stat status;
  return (((SystemStat(name, &status) == 0) && (status.st_mode & JIKES_STAT_S_IFDIR)) ? 1 : 0);
}

int SystemMkdirhier(char *dirname)
{
    if (SystemIsDirectory(dirname))
        return 0;

    for (char *ptr = dirname; *ptr; ptr++)
    {
        char delimiter = *ptr;
        if (delimiter == U_SLASH)
        {
            *ptr = U_NULL;

            if (! SystemIsDirectory(dirname))
                SystemMkdir(dirname);

            *ptr = delimiter;
        }
    }
    SystemMkdir(dirname);

    return (! SystemIsDirectory(dirname));
}



// FIXME: These next two should definitely be inlined; but when I
// add the "inline" keyword , I get linker problems.


// Given three strings, return a newly-allocated string which is their concatenation.
char* strcat3(const char* prefix, const char* middle, const char* suffix)
{
    int prefix_len = strlen(prefix);
    int prefix_middle_len = prefix_len + strlen(middle);

    char* result = new char[prefix_middle_len + strlen(suffix) + 1];
    strcpy(result, prefix);
    // The below is more efficient than this commented-out code.
    // strcat(result, middle);
    // strcat(result, suffix);
    strcpy(result + prefix_len, middle);
    strcpy(result + prefix_middle_len, suffix);
    return result;
}

// It's inconceivable that this is the right way to go about this.
// One alternative is to use ConvertUnicodeToUtf8.
char* wstring2string(wchar_t* in)
{
    char* result = new char[wcslen(in) + 1];
    result[wcslen(in)] = 0;
    for (size_t i=0; i<wcslen(in); i++) {
        wchar_t ch = in[i];
        result[i] = (ch >> 8 == 0 ? (char)ch : '?');
    }
    return result;
}


// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
// End of platform specific defines in this file, the rest of the code
// in this file should work on any system
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


IntToString::IntToString(int num)
{
    if (0x80000000 == (unsigned int) num)
    {
        str = info;
        strcpy(str, StringConstant::U8S_smallest_int);
    }
    else
    {
        str = &info[TAIL_INDEX];
        *str = U_NULL;
        int n = (num < 0 ? -num : num);
        do
        {
            *--str = (U_0 + n % 10);
            n /= 10;
        } while (n != 0);

        if (num < 0)
            *--str = U_MINUS;
    }

    return;
}


IntToWstring::IntToWstring(int num)
{
    if (0x80000000 == (unsigned int) num)
    {
        wstr = winfo;
        wcscpy(wstr,  StringConstant::US_smallest_int);
    }
    else
    {
        wstr = &winfo[TAIL_INDEX];
        *wstr = U_NULL;
        int n = (num < 0 ? -num : num);
        do
        {
            *--wstr = (U_0 + n % 10);
            n /= 10;
        } while (n != 0);

        if (num < 0)
            *--wstr = U_MINUS;
    }

    return;
}


ULongToDecString::ULongToDecString(ULongInt &num)
{
    str = &info[TAIL_INDEX];
    *str = U_NULL;

    ULongInt n = num; // make a copy in order to not destroy reference argument
    do
    {
        *--str = U_0 + (n % 10).LowWord();
        n /= 10;
    } while (n != 0);

    return;
}


LongToOctString::LongToOctString(BaseLong &num)
{
    str = &info[TAIL_INDEX];
    *str = U_NULL;

    ULongInt n = num; // make a copy in order to not destroy reference argument
    do
    {
        *--str = U_0 + (n % 8).LowWord();
        n /= 8;
    } while (n != 0);

    *--str = U_0;

    return;
}


LongToHexString::LongToHexString(BaseLong &num)
{
    str = &info[TAIL_INDEX];
    *str = U_NULL;

    ULongInt n = num; // make a copy in order to not destroy reference argument
    do
    {
        *--str = U_0 + (n % 16).LowWord();
        n /= 16;
    } while (n != 0);

    *--str = U_x;
    *--str = U_0;

    return;
}


LongToDecString::LongToDecString(LongInt &num)
{
    if (num.HighWord() == 0x80000000 && num.LowWord() == 0x00000000)
    {
        str = info;
        strcpy(str,  StringConstant::U8S_smallest_long_int);
    }
    else
    {
        str = &info[TAIL_INDEX];
        *str = U_NULL;

        // compute absolute value
        ULongInt n = (num.HighWord() & 0x80000000 ?
            (ULongInt) -num : (ULongInt) num);

        do
        {
            *--str = U_0 + (n % 10).LowWord();
            n /= 10;
        } while (n != 0);

        if (num.HighWord() & 0x80000000)
            *--str = U_MINUS;
    }

    return;
}


//
// Convert an double to its character string representation.
//
FloatToString::FloatToString(const IEEEfloat &f)
{
    int bbits, b2, b5, be, i,
        j, j1, k, m2, m5, s2, s5;
    bool neg,      // f is negative
        k_check,   // need to check if k is near power of ten
        spec_case, // f is normalized power of two
        denorm,    // f is denormalized
        round;     // round trailing 9's up
    IEEEfloat fs, f1;
    char *s, dig;

    //
    // Start with exceptional cases: zero, infinity, NaN
    //
    neg = f.IsNegative();
    if (f.IsNaN())
    {
        strcpy(str, StringConstant::U8S_NaN);
        length = strlen(str);
        return;
    }
    else if (f.IsInfinite())
    {
        if (neg)
            strcpy(str, StringConstant::U8S_neg_Infinity);
        else
            strcpy(str, StringConstant::U8S_pos_Infinity);
        length = strlen(str);
        return;
    }
    else if (f.IsZero())
    {
        if (neg)
            strcpy(str, StringConstant::U8S_neg_Zero);
        else
            strcpy(str, StringConstant::U8S_pos_Zero);
        length = strlen(str);
        return;
    }

    //
    // Create BigInt holding f.
    // bbits = # significant bits in f
    // be = log2(least significant bit)
    // i = log2(most significant bit)
    // f1 = mantissa of f
    // Therefore, f == f1 * 2**i, and i == be + bbits - 1.
    //
    s = str;
    BigInt b(f, be, bbits);
    u4 x;
    i = f.SplitInto(x);
    f1 = IEEEfloat((i4) x) / (1 << IEEEfloat::FractSize());
    denorm = i <= -IEEEfloat::Bias();
    //
    // log(x)   ~=~ log(1.5) + (x-1.5)/1.5
    // log10(x)  =  log(x) / log(10)
    //          ~=~ log(1.5)/log(10) + (x-1.5)/(1.5*log(10))
    // log10(f)  =  log10(f1 * 2**i)
    //           =  i*log10(2) + log10(f1)
    //
    // This suggests computing an approximation k to log10(f) by
    //
    // k = i*0.30103 + ( 0.17609125 + (f1-1.5)*0.28952965 );
    //
    // We want k to be too large rather than too small.
    // The error in the first-order Taylor series approximation
    // is in our favor, so we just round up the constant enough
    // to compensate for any error in the multiplication of
    // i by 0.30103; since |i| <= 152,
    // and 152 * 0.30103 * 2^-23 ~=~ 5.5e-6,
    // adding 1e-5 to the constant term more than suffices.
    // Hence we adjust the constant term to 0.1761.
    // (We could get a more accurate k by invoking log10,
    //  but this is probably not worthwhile.)
    //
    fs = IEEEfloat(i) * 0.30103f + 0.1761f + (f1 - 1.5f) * 0.28952965f;
    k = fs.IntValue();
    f1 = f.IsNegative() ? -f : f;
    k_check = true;
    if (fs < 0 && fs != k)
        k--;
    else if (k >= 0 && k <= 10)
    {
        if (f1 < IEEEfloat::tens[k])
            k--;
        k_check = false;
    }

    //
    // We have an integer (no fraction) represented in 24 bits.
    // For this special case, math on floats has no rounding errors.
    //
    if (be >= 0 && k <= 6)
    {
        fs = IEEEfloat::tens[k];
        do
        {
            dig = (char) (f1 / fs).IntValue();
            f1 -= fs * (i4) dig;
            *s++ = U_0 + dig;
        } while ((f1 *= 10) != 0);
        Format(s, k, neg);
        return;
    }

    //
    // Begin work. Find S = 2**s2 * 5**s5, and b adjustment 2**b2 * 5**b5,
    // that will be needed later on.
    //
    if (be <= 0)
    {
        b2 = 0;
        s2 = -be;
    }
    else
    {
        b2 = be;
        s2 = 0;
    }
    if (k >= 0)
    {
        b5 = 0;
        s5 = k;
        s2 += k;
    }
    else
    {
        b2 -= k;
        b5 = -k;
        s5 = 0;
    }

    m2 = b2;
    m5 = b5;
    i = denorm ? be + IEEEfloat::Bias() + IEEEfloat::FractSize()
               : 2 + IEEEfloat::FractSize() - bbits;
    b2 += i;
    s2 += i;
    BigInt mhi(1);
    if (m2 > 0 && s2 > 0)
    {
        i = m2 < s2 ? m2 : s2;
        b2 -= i;
        m2 -= i;
        s2 -= i;
    }
    if (b5 > 0)
    {
        if (m5 > 0)
        {
            mhi.pow5mult(m5);
            b *= mhi;
        }
        if ((j = b5 - m5) != 0)
            b.pow5mult(j);
    }
    BigInt S(1);
    if (s5 > 0)
        S.pow5mult(s5);
    spec_case = false;
    if (! (f.FractBits()) && f.Exponent())
    {
        b2++;
        s2++;
        spec_case = true;
    }
    
    // Arrange for convenient computation of quotients:
    // shift left if necessary so divisor has 4 leading 0 bits.
    //
    // Perhaps we should just compute leading 28 bits of S once
    // and for all and pass them and a shift to quorem, so it
    // can do shifts and ors to compute the numerator for q.
    //
    if ((i = ((s5 ? 32 - S.hi0bits() : 1) + s2) & 0x1f) != 0)
        i = 32 - i;
    if (i > 4)
    {
        i -= 4;
        b2 += i;
        m2 += i;
        s2 += i;
    }
    else if (i < 4)
    {
        i += 28;
        b2 += i;
        m2 += i;
        s2 += i;
    }
    if (b2 > 0)
        b <<= b2;
    if (s2 > 0)
        S <<= s2;
    if (k_check && b.compareTo(S) < 0)
    {
        k--;
        b *= 10;
        mhi *= 10;
    }
    if (m2 > 0)
        mhi <<= m2;
    BigInt mlo(mhi);
    if (spec_case)
        mhi = mlo << 1;
    round = false;
    while (true)
    {
        dig = (char) b.quorem(S) + U_0;
        //
        // Do we have the shortest decimal string that will round to f?
        //
        j = b.compareTo(mlo);
        BigInt delta = S - mhi;
        j1 = delta.IsNegative() ? 1 : b.compareTo(delta);
        if (j1 == 0 && ! (f.value.word & 1))
        {
            if (dig == U_9)
                round = true;
            else if (j > 0)
                dig++;
            *s++ = dig;
            break;
        }
        if ((j < 0 || j == 0 && ! (f.value.word & 1)) && s != str)
        {
            if (! b.IsZero() && j1 > 0)
            {
                b <<= 1;
                j1 = b.compareTo(S);
                if ((j1 > 0 || j1 == 0 && dig & 1) && dig++ == U_9)
                {
                    *s++ = U_9;
                    round = true;
                    break;
                }
            }
            *s++ = dig;
            break;
        }
        if (j1 > 0 && s != str)
        {
            if (dig == U_9)
            {
                *s++ = U_9;
                round = true;
            }
            else
                *s++ = dig + 1;
            break;
        }
        *s++ = dig;
        b *= 10;
        mlo *= 10;
        mhi *= 10;
    }
    if (round)
    {
        while (*--s == U_9)
            if (s == str)
            {
                k++;
                *s = U_0;
                break;
            }
        ++*s++;
    }
    Format(s, k, neg);
}

void FloatToString::Format(char *s, int exp, bool neg)
{
    //
    // at this point, str contains just the precise digits in the answer,
    // and s points to the slot just after the last digit
    //
    length = s - str + 1; // strlen(str) + '.'
    bool eneg;
    int i;
    switch (exp)
    {
    case -3: case -2: case -1:
        // remove final trailing 0, not needed in this format
        if (*(s - 1) == U_0)
        {
            length--;
            s--;
        }
        s--;
        // add enough of leading "0.00"
        length += -exp;
        do
            *(s + (neg ? 2 : 1) - exp) = *s;
        while (s-- != str);
        for (i = (neg ? 1 : 0); i < (neg ? 2 : 1) - exp; i++)
            str[i] = U_0;
        if (neg)
            str[0] = U_MINUS;
        str[neg ? 2 : 1] = U_DOT;
        break;
    case 0: case 1: case 2: case 3:
    case 4: case 5: case 6:
        while (length < exp + 3)
            // add trailing '0's
            str[length++ - 1] = U_0;
        s = &str[length - 2];
        do
            *(s + (neg ? 2 : 1)) = *s;
        while (s-- != str + exp + 1);
        if (neg)
        {
            do
                *(s + 1) = *s;
            while (s-- != str);
            str[0] = U_MINUS;
        }
        str[exp + (neg ? 2 : 1)] = U_DOT;
        break;
    default:
        if (length == 2)
            // add trailing '0', so at least one digit follows '.'
            str[length++ - 1] = U_0;
        eneg = exp < 0;
        if (eneg)
        {
            length++; // exponent '-'
            exp = -exp;
        }
        if (exp < 10)
            length += 2; // 'E' + 1 digit exponent
        else if (exp < 100)
            length += 3; // 'E' + 2 digit exponent
        else
            assert (! "unexpected exponent");
        s = &str[length + (neg ? 1 : 0)];
        do
            *--s = exp % 10 + U_0;
        while ((exp /= 10) != 0);
        if (eneg)
            *--s = U_MINUS;
        *--s = U_E;
        --s;
        do
            *s = *(s - (neg ? 2 : 1)); // shift digits right, to add '.'
        while (--s != str + (neg ? 2 : 1));
        if (neg)
        {
            str[1] = str[0];
            str[0] = U_MINUS;
        }    
        str[neg ? 2 : 1] = U_DOT;
    }      
    if (neg)
        length++;
    str[length] = U_NULL;
    assert(length <= MAXIMUM_STR_LENGTH);
}


//
// Convert an double to its character string representation.
//
DoubleToString::DoubleToString(const IEEEdouble &d)
{
    int bbits, b2, b5, be, i,
        j, j1, k, m2, m5, s2, s5;
    bool neg,      // f is negative
        k_check,   // need to check if k is near power of ten
        spec_case, // f is normalized power of two
        denorm,    // f is denormalized
        round;     // round trailing 9's up
    IEEEdouble ds, d1;
    char *s, dig;

    //
    // Start with exceptional cases: zero, infinity, NaN
    //
    neg = d.IsNegative();
    if (d.IsNaN())
    {
        strcpy(str, StringConstant::U8S_NaN);
        length = strlen(str);
        return;
    }
    else if (d.IsInfinite())
    {
        if (neg)
            strcpy(str, StringConstant::U8S_neg_Infinity);
        else
            strcpy(str, StringConstant::U8S_pos_Infinity);
        length = strlen(str);
        return;
    }
    else if (d.IsZero())
    {
        if (neg)
            strcpy(str, StringConstant::U8S_neg_Zero);
        else
            strcpy(str, StringConstant::U8S_pos_Zero);
        length = strlen(str);
        return;
    }

    //
    // Create BigInt holding d.
    // bbits = # significant bits in d
    // be = log2(least significant bit)
    // i = log2(most significant bit)
    // d1 = mantissa of d
    // Therefore, d == d1 * 2**i, and i == be + bbits - 1.
    //
    s = str;
    BigInt b(d, be, bbits);
    LongInt x;
    i = d.SplitInto(x);
    d1 = IEEEdouble(x) / IEEEdouble(LongInt(1) << IEEEdouble::FractSize());
    denorm = i <= -IEEEdouble::Bias();
    //
    // log(x)   ~=~ log(1.5) + (x-1.5)/1.5
    // log10(x)  =  log(x) / log(10)
    //          ~=~ log(1.5)/log(10) + (x-1.5)/(1.5*log(10))
    // log10(d)  =  log10(d2 * 2**i)
    //           =  i*log10(2) + log10(d2)
    //
    // This suggests computing an approximation k to log10(d) by
    //
    // k = i*0.301029995663981
    //   + ( 0.176091259055681 + (d2-1.5)*0.289529654602168 );
    //
    // We want k to be too large rather than too small.
    // The error in the first-order Taylor series approximation
    // is in our favor, so we just round up the constant enough
    // to compensate for any error in the multiplication of
    // i by 0.301029995663981; since |i| <= 1077,
    // and 1077 * 0.30103 * 2^-52 ~=~ 7.2e-14,
    // adding 1e-13 to the constant term more than suffices.
    // Hence we adjust the constant term to 0.1760912590558.
    // (We could get a more accurate k by invoking log10,
    //  but this is probably not worthwhile.)
    //
    ds = IEEEdouble(i) * 0.301029995663981 + 0.1760912590558
        + (d1 - 1.5) * 0.289529654602168;
    k = ds.IntValue();
    d1 = d.IsNegative() ? -d : d;
    k_check = true;
    if (ds < 0 && ds != k)
        k--;
    else if (k >= 0 && k <= 22)
    {
        if (d1 < IEEEdouble::tens[k])
            k--;
        k_check = false;
    }

    //
    // We have an integer (no fraction) represented in 53 bits.
    // For this special case, math on doubles has no rounding errors.
    //
    if (be >= 0 && k <= 14)
    {
        ds = IEEEdouble::tens[k];
        do
        {
            dig = (char) (d1 / ds).IntValue();
            d1 -= ds * (i4) dig;
            *s++ = U_0 + dig;
        } while ((d1 *= 10) != 0);
        Format(s, k, neg);
        return;
    }

    //
    // Begin work. Find S = 2**s2 * 5**s5, and b adjustment 2**b2 * 5**b5,
    // that will be needed later on.
    //
    if (be <= 0)
    {
        b2 = 0;
        s2 = -be;
    }
    else
    {
        b2 = be;
        s2 = 0;
    }
    if (k >= 0)
    {
        b5 = 0;
        s5 = k;
        s2 += k;
    }
    else
    {
        b2 -= k;
        b5 = -k;
        s5 = 0;
    }

    m2 = b2;
    m5 = b5;
    i = denorm ? be + IEEEdouble::Bias() + IEEEdouble::FractSize()
               : 2 + IEEEdouble::FractSize() - bbits;
    b2 += i;
    s2 += i;
    BigInt mhi(1);
    if (m2 > 0 && s2 > 0)
    {
        i = m2 < s2 ? m2 : s2;
        b2 -= i;
        m2 -= i;
        s2 -= i;
    }
    if (b5 > 0)
    {
        if (m5 > 0)
        {
            mhi.pow5mult(m5);
            b *= mhi;
        }
        if ((j = b5 - m5) != 0)
            b.pow5mult(j);
    }
    BigInt S(1);
    if (s5 > 0)
        S.pow5mult(s5);
    spec_case = false;
    if (! (d.FractBits()) && d.Exponent())
    {
        b2++;
        s2++;
        spec_case = true;
    }
    
    // Arrange for convenient computation of quotients:
    // shift left if necessary so divisor has 4 leading 0 bits.
    //
    // Perhaps we should just compute leading 28 bits of S once
    // and for all and pass them and a shift to quorem, so it
    // can do shifts and ors to compute the numerator for q.
    //
    if ((i = ((s5 ? 32 - S.hi0bits() : 1) + s2) & 0x1f) != 0)
        i = 32 - i;
    if (i > 4)
    {
        i -= 4;
        b2 += i;
        m2 += i;
        s2 += i;
    }
    else if (i < 4)
    {
        i += 28;
        b2 += i;
        m2 += i;
        s2 += i;
    }
    if (b2 > 0)
        b <<= b2;
    if (s2 > 0)
        S <<= s2;
    if (k_check && b.compareTo(S) < 0)
    {
        k--;
        b *= 10;
        mhi *= 10;
    }
    if (m2 > 0)
        mhi <<= m2;
    BigInt mlo(mhi);
    if (spec_case)
        mhi = mlo << 1;
    round = false;
    while (true)
    {
        dig = (char) b.quorem(S) + U_0;
        //
        // Do we have the shortest decimal string that will round to d?
        //
        j = b.compareTo(mlo);
        BigInt delta = S - mhi;
        j1 = delta.IsNegative() ? 1 : b.compareTo(delta);
        if (j1 == 0 && ! (d.LowWord() & 1))
        {
            if (dig == U_9)
                round = true;
            else if (j > 0)
                dig++;
            *s++ = dig;
            break;
        }
        if ((j < 0 || j == 0 && ! (d.LowWord() & 1)) && s != str)
        {
            if (! b.IsZero() && j1 > 0)
            {
                b <<= 1;
                j1 = b.compareTo(S);
                if ((j1 > 0 || j1 == 0 && dig & 1) && dig++ == U_9)
                {
                    *s++ = U_9;
                    round = true;
                    break;
                }
            }
            *s++ = dig;
            break;
        }
        if (j1 > 0 && s != str)
        {
            if (dig == U_9)
            {
                *s++ = U_9;
                round = true;
            }
            else
                *s++ = dig + 1;
            break;
        }
        *s++ = dig;
        b *= 10;
        mlo *= 10;
        mhi *= 10;
    }
    if (round)
    {
        while (*--s == U_9)
            if (s == str)
            {
                k++;
                *s = U_0;
                break;
            }
        ++*s++;
    }
    Format(s, k, neg);
}

void DoubleToString::Format(char *s, int exp, bool neg)
{
    //
    // at this point, str contains just the precise digits in the answer,
    // and s points to the slot just after the last digit
    //
    length = s - str + 1; // strlen(str) + '.'
    bool eneg;
    int i;
    switch (exp)
    {
    case -3: case -2: case -1:
        // remove final trailing 0, not needed in this format
        if (*(s - 1) == U_0)
        {
            length--;
            s--;
        }
        s--;
        // add enough of leading "0.00"
        length += -exp;
        do
            *(s + (neg ? 2 : 1) - exp) = *s;
        while (s-- != str);
        for (i = (neg ? 1 : 0); i < (neg ? 2 : 1) - exp; i++)
            str[i] = U_0;
        if (neg)
            str[0] = U_MINUS;
        str[neg ? 2 : 1] = U_DOT;
        break;
    case 0: case 1: case 2: case 3:
    case 4: case 5: case 6:
        while (length < exp + 3)
            // add trailing '0's
            str[length++ - 1] = U_0;
        s = &str[length - 2];
        do
            *(s + (neg ? 2 : 1)) = *s;
        while (s-- != str + exp + 1);
        if (neg)
        {
            do
                *(s + 1) = *s;
            while (s-- != str);
            str[0] = U_MINUS;
        }
        str[exp + (neg ? 2 : 1)] = U_DOT;
        break;
    default:
        if (length == 2)
            // add trailing '0', so at least one digit follows '.'
            str[length++ - 1] = U_0;
        eneg = exp < 0;
        if (eneg)
        {
            length++; // exponent '-'
            exp = -exp;
        }
        if (exp < 10)
            length += 2; // 'E' + 1 digit exponent
        else if (exp < 100)
            length += 3; // 'E' + 2 digit exponent
        else if (exp < 1000)
            length += 4; // 'E' + 3 digit exponent
        else
            assert (! "unexpected exponent");
        s = &str[length + (neg ? 1 : 0)];
        do
            *--s = exp % 10 + U_0;
        while ((exp /= 10) != 0);
        if (eneg)
            *--s = U_MINUS;
        *--s = U_E;
        --s;
        do
            *s = *(s - (neg ? 2 : 1)); // shift digits right, to add '.'
        while (--s != str + (neg ? 2 : 1));
        if (neg)
        {
            str[1] = str[0];
            str[0] = U_MINUS;
        }    
        str[neg ? 2 : 1] = U_DOT;
    }      
    if (neg)
        length++;
    str[length] = U_NULL;
    assert(length <= MAXIMUM_STR_LENGTH);
}


Ostream &Ostream::operator<<(LongInt a)
{
    if (os -> flags() & os -> dec)
    {
        LongToDecString long_int(a);
        *os << long_int.String();
    }
    else if (os -> flags() & os -> oct)
    {
        LongToOctString long_int(a);
        *os << (os -> flags() & os -> showbase ? long_int.StringWithBase() : long_int.String());
    }
    else if (os -> flags() & os -> hex)
    {
        LongToHexString long_int(a);
        *os << (os -> flags() & os -> showbase ? long_int.StringWithBase() : long_int.String());
    }
    else
    {
         os -> flush();
         assert(! "know how to print signed long value in specified format yet !!!");
    }

    return *this;
}

Ostream &Ostream::operator<<(ULongInt a)
{
    if (os -> flags() & os -> dec)
    {
        ULongToDecString ulong_int(a);
        *os << ulong_int.String();
    }
    else if (os -> flags() & os -> oct)
    {
        LongToOctString ulong_int(a);
        *os << (os -> flags() & os -> showbase ? ulong_int.StringWithBase() : ulong_int.String());
    }
    else if (os -> flags() & os -> hex)
    {
        LongToHexString ulong_int(a);
        *os << (os -> flags() & os -> showbase ? ulong_int.StringWithBase() : ulong_int.String());
    }
    else
    {
        os -> flush();
        assert(! "know how to print unsigned long value in specified format yet !!!");
    }

    return *this;
}

wchar_t StringConstant::US_AND[]                        = {U_AM, U_NU}, // "&"
        StringConstant::US_AND_AND[]                    = {U_AM, U_AM, U_NU}, // "&&"
        StringConstant::US_AND_EQUAL[]                  = {U_AM, U_EQ, U_NU}, // "&="
        StringConstant::US_COLON[]                      = {U_CO, U_NU}, // ":"
        StringConstant::US_COMMA[]                      = {U_CM, U_NU}, // ","
        StringConstant::US_DIVIDE[]                     = {U_SL, U_NU}, // "/"
        StringConstant::US_DIVIDE_EQUAL[]               = {U_SL, U_EQ, U_NU}, // "/="
        StringConstant::US_DOT[]                        = {U_DO, U_NU}, // "."
        StringConstant::US_EMPTY[]                      = {U_NU}, // ""
        StringConstant::US_EQUAL[]                      = {U_EQ, U_NU}, // "="
        StringConstant::US_EQUAL_EQUAL[]                = {U_EQ, U_EQ, U_NU}, // "=="
        StringConstant::US_GREATER[]                    = {U_GT, U_NU}, // ">"
        StringConstant::US_GREATER_EQUAL[]              = {U_GT, U_EQ, U_NU}, // ">="
        StringConstant::US_LBRACE[]                     = {U_OS, U_NU}, // "{"
        StringConstant::US_LBRACKET[]                   = {U_LB, U_NU}, // "["
        StringConstant::US_LEFT_SHIFT[]                 = {U_LT, U_LT, U_NU}, // "<<"
        StringConstant::US_LEFT_SHIFT_EQUAL[]           = {U_LT, U_LT, U_EQ, U_NU}, // "<<="
        StringConstant::US_LESS[]                       = {U_LT, U_NU}, // "<"
        StringConstant::US_LESS_EQUAL[]                 = {U_LT, U_EQ, U_NU}, // "<="
        StringConstant::US_LPAREN[]                     = {U_LP, U_NU}, // "("
        StringConstant::US_MINUS[]                      = {U_MI, U_NU}, // "-"
        StringConstant::US_MINUS_EQUAL[]                = {U_MI, U_EQ, U_NU}, // "-="
        StringConstant::US_MINUS_MINUS[]                = {U_MI, U_MI, U_NU}, // "--"
        StringConstant::US_MULTIPLY[]                   = {U_ST, U_NU}, // "*"
        StringConstant::US_MULTIPLY_EQUAL[]             = {U_ST, U_EQ, U_NU}, // "*="
        StringConstant::US_NOT[]                        = {U_EX, U_NU}, // "!"
        StringConstant::US_NOT_EQUAL[]                  = {U_EX, U_EQ, U_NU}, // "!="
        StringConstant::US_OR[]                         = {U_BA, U_NU}, // "|"
        StringConstant::US_OR_EQUAL[]                   = {U_BA, U_EQ, U_NU}, // "|="
        StringConstant::US_OR_OR[]                      = {U_BA, U_BA, U_NU}, // "||"
        StringConstant::US_PLUS[]                       = {U_PL, U_NU}, // "+"
        StringConstant::US_PLUS_EQUAL[]                 = {U_PL, U_EQ, U_NU}, // "+="
        StringConstant::US_PLUS_PLUS[]                  = {U_PL, U_PL, U_NU}, // "++"
        StringConstant::US_QUESTION[]                   = {U_QU, U_NU}, // "?"
        StringConstant::US_RBRACE[]                     = {U_CS, U_NU}, // "}"
        StringConstant::US_RBRACKET[]                   = {U_RB, U_NU}, // "]"
        StringConstant::US_REMAINDER[]                  = {U_PE, U_NU}, // "%"
        StringConstant::US_REMAINDER_EQUAL[]            = {U_PE, U_EQ, U_NU}, // "%="
        StringConstant::US_RIGHT_SHIFT[]                = {U_GT, U_GT, U_NU}, // ">>"
        StringConstant::US_RIGHT_SHIFT_EQUAL[]          = {U_GT, U_GT, U_EQ, U_NU}, // ">>="
        StringConstant::US_RPAREN[]                     = {U_RP, U_NU}, // ")"
        StringConstant::US_SEMICOLON[]                  = {U_SC, U_NU}, // ";"
        StringConstant::US_TWIDDLE[]                    = {U_TI, U_NU}, // "~"
        StringConstant::US_UNSIGNED_RIGHT_SHIFT[]       = {U_GT, U_GT, U_GT, U_NU}, // ">>>"
        StringConstant::US_UNSIGNED_RIGHT_SHIFT_EQUAL[] = {U_GT, U_GT, U_GT, U_EQ, U_NU}, // ">>>="
        StringConstant::US_XOR[]                        = {U_CA, U_NU}, // "^"
        StringConstant::US_XOR_EQUAL[]                  = {U_CA, U_EQ, U_NU}, // "^="

        StringConstant::US_Boolean[] = {U_B, U_o, U_o, U_l, U_e, U_a, U_n, U_NU}, // "Boolean"
        StringConstant::US_Byte[] = {U_B, U_y, U_t, U_e, U_NU}, // "Byte"
        StringConstant::US_Character[] = {U_C, U_h, U_a, U_r, U_a, U_c, U_t, U_e, U_r, U_NU}, // "Character"
        StringConstant::US_Class[] = {U_C, U_l, U_a, U_s, U_s, U_NU}, // "Class"
        StringConstant::US_ClassNotFoundException[] = {U_C, U_l, U_a, U_s, U_s, U_N, U_o, U_t, U_F, U_o, U_u, U_n, U_d, U_E, U_x, U_c, U_e, U_p, U_t, U_i, U_o, U_n, U_NU}, // "ClassNotFoundException"
        StringConstant::US_Cloneable[] = {U_C, U_l, U_o, U_n, U_e, U_a, U_b, U_l, U_e, U_NU}, // "Cloneable"
        StringConstant::US_Double[] = {U_D, U_o, U_u, U_b, U_l, U_e, U_NU}, // "Double"
        StringConstant::US_Error[] = {U_E, U_r, U_r, U_o, U_r, U_NU}, // "Error"
        StringConstant::US_Exception[] = { U_E, U_x, U_c, U_e, U_p, U_t, U_i, U_o, U_n, U_NU}, // "Exception"
        StringConstant::US_Float[] = {U_F, U_l, U_o, U_a, U_t, U_NU},  // "Float"
        StringConstant::US_Integer[] = {U_I, U_n, U_t, U_e, U_g, U_e, U_r, U_NU}, // "Integer"
        StringConstant::US_L[] = {U_L, U_NU}, // "L"
        StringConstant::US_Long[]  = {U_L, U_o, U_n, U_g, U_NU}, // "Long"
        StringConstant::US_NoClassDefFoundError[] = {U_N, U_o, U_C, U_l, U_a, U_s, U_s, U_D, U_e, U_f, U_F, U_o, U_u, U_n, U_d, U_E, U_r, U_r, U_o, U_r, U_NU}, // "NoClassDefFoundError"
        StringConstant::US_Object[] = {U_O, U_b, U_j, U_e, U_c, U_t, U_NU}, // "Object"
        StringConstant::US_PObject[] = {U_P, U_O, U_b, U_j, U_e, U_c, U_t, U_NU}, // "PObject"
        StringConstant::US_RuntimeException[] = {U_R, U_u, U_n, U_t, U_i, U_m, U_e, U_E, U_x, U_c, U_e, U_p, U_t, U_i, U_o, U_n, U_NU}, // "RuntimeException"
        StringConstant::US_Serializable[] = {U_S, U_e, U_r, U_i, U_a, U_l, U_i, U_z, U_a, U_b, U_l, U_e, U_NU}, // "Serializable"
        StringConstant::US_Short[] = {U_S, U_h, U_o, U_r, U_t, U_NU}, // "Short"
        StringConstant::US_String[] = {U_S, U_t, U_r, U_i, U_n, U_g, U_NU}, // "String"
        StringConstant::US_StringBuffer[] = {U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_NU}, // "StringBuffer"
        StringConstant::US_TYPE[] = {U_T, U_Y, U_P, U_E, U_NU}, // "TYPE"
        StringConstant::US_Throwable[] = {U_T, U_h, U_r, U_o, U_w, U_a, U_b, U_l, U_e, U_NU}, // "Throwable"
        StringConstant::US_Void[] = {U_V, U_o, U_i, U_d, U_NU}, // "Void"
        StringConstant::US__DO[] = {U_DO, U_NU}, // "."
        StringConstant::US__DO__DO[] = {U_DO, U_DO, U_NU}, // ".."
        StringConstant::US__DS[] = {U_DS, U_NU}, // "$"
        StringConstant::US__LB__RB[] = {U_LB, U_RB, U_NU}, // "[]"
        StringConstant::US__LT_clinit_GT[] = {U_LT, U_c, U_l, U_i, U_n, U_i, U_t, U_GT, U_NU}, // "<clinit>"
        StringConstant::US__LT_init_GT[] = {U_LT, U_i, U_n, U_i, U_t, U_GT, U_NU}, // "<init>"
        StringConstant::US__QU__QU[] = {U_QU, U_QU, U_NU},  // "??"
        StringConstant::US__SC[] = {U_SC, U_NU}, // ";"
        StringConstant::US__SL[] = {U_SL, U_NU}, // "/"

        StringConstant::US__zip[] = {U_z, U_i, U_p, U_NU}, // "zip"
        StringConstant::US__jar[] = {U_j, U_a, U_r, U_NU}, // "jar"

        StringConstant::US__array[] = {U_a, U_r, U_r, U_a, U_y, U_NU}, // "array"
        StringConstant::US__access_DOLLAR[] = {U_a, U_c, U_c, U_e, U_s, U_s, U_DS, U_NU}, // "access$"
        StringConstant::US__class_DOLLAR[] = {U_c, U_l, U_a, U_s, U_s, U_DS, U_NU}, // "class$"
        StringConstant::US__constructor_DOLLAR[] = {U_c, U_o, U_n, U_s, U_t, U_r, U_u, U_c, U_t, U_o, U_r, U_DS, U_NU}, // "constructor$"
        StringConstant::US__this_DOLLAR[] = {U_t, U_h, U_i, U_s, U_DS, U_NU}, // "this$"
        StringConstant::US__val_DOLLAR[] = {U_v, U_a, U_l, U_DS, U_NU}, // "val$"

        StringConstant::US_abstract[] = {U_a, U_b, U_s, U_t, U_r, U_a, U_c, U_t, U_NU}, // "abstract"
        StringConstant::US_append[] = {U_a, U_p, U_p, U_e, U_n, U_d, U_NU}, // "append"
        StringConstant::US_block_DOLLAR[] = {U_b, U_l, U_o, U_c, U_k, U_DS, U_NU}, // "block$"
        StringConstant::US_boolean[] = {U_b, U_o, U_o, U_l, U_e, U_a, U_n, U_NU}, // "boolean"
        StringConstant::US_break[] = {U_b, U_r, U_e, U_a, U_k, U_NU}, // "break"
        StringConstant::US_byte[] = {U_b, U_y, U_t, U_e, U_NU}, // "byte"
        StringConstant::US_case[] = {U_c, U_a, U_s, U_e, U_NU}, // "case"
        StringConstant::US_catch[] = {U_c, U_a, U_t, U_c, U_h, U_NU}, // "catch"
        StringConstant::US_char[] = {U_c, U_h, U_a, U_r, U_NU}, // "char"
        StringConstant::US_class[] = {U_c, U_l, U_a, U_s, U_s, U_NU}, // "class"
        StringConstant::US_clone[] = {U_c, U_l, U_o, U_n, U_e, U_NU}, // "clone"
        StringConstant::US_const[] = {U_c, U_o, U_n, U_s, U_t, U_NU}, // "const"
        StringConstant::US_continue[] = {U_c, U_o, U_n, U_t, U_i, U_n, U_u, U_e, U_NU}, // "continue"
        StringConstant::US_default[] = {U_d, U_e, U_f, U_a, U_u, U_l, U_t, U_NU}, // "default"
        StringConstant::US_do[] = {U_d, U_o, U_NU}, // "do"
        StringConstant::US_double[] = {U_d, U_o, U_u, U_b, U_l, U_e, U_NU}, // "double"
        StringConstant::US_else[] = {U_e, U_l, U_s, U_e, U_NU}, // "else"
        StringConstant::US_extends[] = {U_e, U_x, U_t, U_e, U_n, U_d, U_s, U_NU}, // "extends"
        StringConstant::US_false[] = {U_f, U_a, U_l, U_s, U_e, U_NU}, // "false"
        StringConstant::US_final[] = {U_f, U_i, U_n, U_a, U_l, U_NU}, // "final"
        StringConstant::US_finally[] = {U_f, U_i, U_n, U_a, U_l, U_l, U_y, U_NU}, // "finally"
        StringConstant::US_float[] = {U_f, U_l, U_o, U_a, U_t, U_NU}, // "float"
        StringConstant::US_for[] = {U_f, U_o, U_r, U_NU}, // "for"
        StringConstant::US_forName[] = {U_f, U_o, U_r, U_N, U_a, U_m, U_e, U_NU}, // "forName"
        StringConstant::US_getMessage[] = {U_g, U_e, U_t, U_M, U_e, U_s, U_s, U_a, U_g, U_e, U_NU}, // "getMessage"
        StringConstant::US_goto[] = {U_g, U_o, U_t, U_o, U_NU}, // "goto"
        StringConstant::US_if[] = {U_i, U_f, U_NU}, // "if"
        StringConstant::US_implements[] = {U_i, U_m, U_p, U_l, U_e, U_m, U_e, U_n, U_t, U_s, U_NU}, // "implements"
        StringConstant::US_import[] = {U_i, U_m, U_p, U_o, U_r, U_t, U_NU}, // "import"
        StringConstant::US_instanceof[] = {U_i, U_n, U_s, U_t, U_a, U_n, U_c, U_e, U_o, U_f, U_NU}, // "instanceof"
        StringConstant::US_int[] = {U_i, U_n, U_t, U_NU}, // "int"
        StringConstant::US_interface[] = {U_i, U_n, U_t, U_e, U_r, U_f, U_a, U_c, U_e, U_NU}, // "interface"
        StringConstant::US_java_SL_io[] =  {U_j, U_a, U_v, U_a, U_SL, U_i, U_o, U_NU}, // "java/io"
        StringConstant::US_java_SL_lang[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_NU}, // "java/lang"
        StringConstant::US_java_SL_util[] = {U_j, U_a, U_v, U_a, U_SL, U_u, U_t, U_i, U_l, U_NU}, // "java/util"
        StringConstant::US_length[] = {U_l, U_e, U_n, U_g, U_t, U_h, U_NU}, // "length"
        StringConstant::US_long[] = {U_l, U_o, U_n, U_g, U_NU}, // "long"
        StringConstant::US_native[] = {U_n, U_a, U_t, U_i, U_v, U_e, U_NU}, // "native"
        StringConstant::US_new[] = {U_n, U_e, U_w, U_NU}, // "new"
        StringConstant::US_null[] = {U_n, U_u, U_l, U_l, U_NU}, // "null"
        StringConstant::US_package[] = {U_p, U_a, U_c, U_k, U_a, U_g, U_e, U_NU}, // "package"
        StringConstant::US_private[] = {U_p, U_r, U_i, U_v, U_a, U_t, U_e, U_NU}, // "private"
        StringConstant::US_protected[] = {U_p, U_r, U_o, U_t, U_e, U_c, U_t, U_e, U_d, U_NU}, // "protected"
        StringConstant::US_public[] = {U_p, U_u, U_b, U_l, U_i, U_c, U_NU}, // "public"
        StringConstant::US_return[] = {U_r, U_e, U_t, U_u, U_r, U_n, U_NU}, // "return"
        StringConstant::US_short[] = {U_s, U_h, U_o, U_r, U_t, U_NU}, // "short"
        StringConstant::US_static[] = {U_s, U_t, U_a, U_t, U_i, U_c, U_NU}, // "static"
        StringConstant::US_strictfp[] = {U_s, U_t, U_r, U_i, U_c, U_t, U_f, U_p, U_NU}, // "strictfp"
        StringConstant::US_super[] = {U_s, U_u, U_p, U_e, U_r, U_NU}, // "super"
        StringConstant::US_switch[] = {U_s, U_w, U_i, U_t, U_c, U_h, U_NU}, // "switch"
        StringConstant::US_synchronized[] = {U_s, U_y, U_n, U_c, U_h, U_r, U_o, U_n, U_i, U_z, U_e, U_d, U_NU}, // "synchronized"
        StringConstant::US_this0[] = {U_t, U_h, U_i, U_s, U_DS, U_0, U_NU}, // "this$0"
        StringConstant::US_this[] = {U_t, U_h, U_i, U_s, U_NU}, // "this"
        StringConstant::US_throw[] = {U_t, U_h, U_r, U_o, U_w, U_NU}, // "throw"
        StringConstant::US_throws[] = {U_t, U_h, U_r, U_o, U_w, U_s, U_NU}, // "throws"
        StringConstant::US_toString[] = {U_t, U_o, U_S, U_t, U_r, U_i, U_n, U_g, U_NU}, // "toString"
        StringConstant::US_transient[] = {U_t, U_r, U_a, U_n, U_s, U_i, U_e, U_n, U_t, U_NU}, // "transient"
        StringConstant::US_true[] = {U_t, U_r, U_u, U_e, U_NU}, // "true"
        StringConstant::US_try[] = {U_t, U_r, U_y, U_NU}, // "try"
        StringConstant::US_void[] = {U_v, U_o, U_i, U_d, U_NU}, // "void"
        StringConstant::US_volatile[] = {U_v, U_o, U_l, U_a, U_t, U_i, U_l, U_e, U_NU}, // "volatile"
        StringConstant::US_while[] = {U_w, U_h, U_i, U_l, U_e, U_NU}, // "while"

        StringConstant::US_EOF[] = {U_E, U_O, U_F, U_NU}; // "EOF"

wchar_t StringConstant::US_smallest_int[] = {U_MINUS, U_2, U_1, U_4, U_7, U_4, U_8, U_3, U_6, U_4, U_8, U_NU}; // "-2147483648"


char StringConstant::U8S_command_format[] = "use: jikes [-bootclasspath path][-classpath path][-d dir][-debug][-depend|-Xdepend][-deprecation]"
#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)
                                            "[-encoding encoding]"
#endif
                                            "[-extdirs path][-g][-nowarn][-nowrite][-O][-sourcepath path][-verbose][-Xstdout]"
                                            "[++][+B][+c][+OLDCSO][+D][+DR=filename][+E][+F][+Kname=TypeKeyWord][+M][+P][+Td...d][+U][+Z]"
                                            " file.java...";

char StringConstant::U8S_B[] = {U_B, U_NU}, // "B"
     StringConstant::U8S_C[] = {U_C, U_NU}, // "C"
     StringConstant::U8S_Code[] = {U_C, U_o, U_d, U_e, U_NU}, // "Code"
     StringConstant::U8S_ConstantValue[] = {U_C, U_o, U_n, U_s, U_t, U_a, U_n, U_t, U_V, U_a, U_l, U_u, U_e, U_NU}, // "ConstantValue"
     StringConstant::U8S_D[] = {U_D, U_NU}, // "D"
     StringConstant::U8S_Exceptions[] = {U_E, U_x, U_c, U_e, U_p, U_t, U_i, U_o, U_n, U_s, U_NU}, // "Exceptions"
     StringConstant::U8S_F[] = {U_F, U_NU}, // "F"
     StringConstant::U8S_I[] = {U_I, U_NU}, // "I"
     StringConstant::U8S_InnerClasses[] = {U_I, U_n, U_n, U_e, U_r, U_C, U_l, U_a, U_s, U_s, U_e, U_s, U_NU}, // "InnerClasses"
     StringConstant::U8S_J[] = {U_J, U_NU},  // "J"
     StringConstant::U8S_LP_Ljava_SL_lang_SL_String_SC_RP_Ljava_SL_lang_SL_Class_SC[] = {U_LP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_SC, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_C, U_l, U_a, U_s, U_s, U_SC, U_NU}, // "(Ljava/lang/String;)Ljava/lang/Class;"
     StringConstant::U8S_LP_Ljava_SL_lang_SL_String_SC_RP_V[] = {U_LP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_SC, U_RP, U_V, U_NU}, // "(Ljava/lang/String;)V"
     StringConstant::U8S_LP_RP_Ljava_SL_lang_SL_String_SC[] = {U_LP, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_SC, U_NU}, // "()_Ljava/lang/String;"
     StringConstant::U8S_LP_RP_V[] = {U_LP, U_RP, U_V, U_NU}, // "()V"
     StringConstant::U8S_LineNumberTable[] = {U_L, U_i, U_n, U_e, U_N, U_u, U_m, U_b, U_e, U_r, U_T, U_a, U_b, U_l, U_e, U_NU}, // "LineNumberTable"
     StringConstant::U8S_LocalVariableTable[] = {U_L, U_o, U_c, U_a, U_l, U_V, U_a, U_r, U_i, U_a, U_b, U_l, U_e, U_T, U_a, U_b, U_l, U_e, U_NU}, // "LocalVariableTable"
     StringConstant::U8S_S[] = {U_S, U_NU}, // "S"
     StringConstant::U8S_Sourcefile[] = {U_S, U_o, U_u, U_r, U_c, U_e, U_F, U_i, U_l, U_e, U_NU}, // "Sourcefile"
     StringConstant::U8S_Synthetic[] = {U_S, U_y, U_n, U_t, U_h, U_e, U_t, U_i, U_c, U_NU}, // "Synthetic"
     StringConstant::U8S_Deprecated[] = {U_D, U_e, U_p, U_r, U_e, U_c, U_a, U_t, U_e, U_d, U_NU}, // "Deprecated"
     StringConstant::U8S_V[] = {U_V, U_NU}, // "V"
     StringConstant::U8S_Z[] = {U_Z, U_NU}, // "Z"

     StringConstant::U8S__DO[] = {U_DO, U_NU}, // "."
     StringConstant::U8S__DO_class[] = {U_DO, U_c, U_l, U_a, U_s, U_s, U_NU}, // ".class"
     StringConstant::U8S__DO_java[] = {U_DO, U_j, U_a, U_v, U_a, U_NU}, // ".java"
     StringConstant::U8S__DO_tok[] = {U_DO, U_t, U_o, U_k, U_NU}, // ".tok"
     StringConstant::U8S__DO_u[] = {U_DO, U_u, U_NU}, // ".u"
     StringConstant::U8S__LP[] = {U_LP, U_NU}, // "("
     StringConstant::U8S__RP[] = {U_RP, U_NU}, // ")"
     StringConstant::U8S__SL[] = {U_SL, U_NU}, // "/"
     StringConstant::U8S__ST[] = {U_ST, U_NU}, // "*"

     StringConstant::U8S_class[] = {U_c, U_l, U_a, U_s, U_s, U_NU}, // "class"
     StringConstant::U8S_java[] = {U_j, U_a, U_v, U_a, U_NU}, // "java"
     StringConstant::U8S_java_SL_lang_SL_ClassNotFoundException[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_C, U_l, U_a, U_s, U_s, U_N, U_o, U_t, U_F, U_o, U_u, U_n, U_d, U_E, U_x, U_c, U_e, U_p, U_t, U_i, U_o, U_n, U_NU}, // "java/lang/ClassNotFoundException"
     StringConstant::U8S_java_SL_lang_SL_Class[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_C, U_l, U_a, U_s, U_s, U_NU}, // "java/lang/Class"
     StringConstant::U8S_java_SL_lang_SL_InternalError[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_I, U_n, U_t, U_e, U_r, U_n, U_a, U_l, U_E, U_r, U_r, U_o, U_r, U_NU}, // "java/lang/InternalError"
     StringConstant::U8S_java_SL_lang_SL_NoClassDefFoundError[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_N, U_o, U_C, U_l, U_a, U_s, U_s, U_D, U_e, U_f, U_F, U_o, U_u, U_n, U_d, U_E, U_r, U_r, U_o, U_r, U_NU}, // "java/lang/NoClassDefFoundError"
     StringConstant::U8S_java_SL_lang_SL_StringBuffer[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_NU}, // "java/lang/StringBuffer"
     StringConstant::U8S_java_SL_lang_SL_Throwable[] = {U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_T, U_h, U_r, U_o, U_w, U_a, U_b, U_l, U_e, U_NU}, // "java/lang/Throwable"
     StringConstant::U8S_null[] = {U_n, U_u, U_l, U_l, U_NU}, // "null"
     StringConstant::U8S_quit[] = {U_q, U_u, U_i, U_t, U_NU}, // "quit"
     StringConstant::U8S_this[] = {U_t, U_h, U_i, U_s, U_NU}, // "this"

     StringConstant::U8S_LP_LB_C_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_LB, U_C, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_C_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_C, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_Z_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_Z, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_I_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_I, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_J_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_J, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_F_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_F, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_D_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_D, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_Ljava_SL_lang_SL_String_SC_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_SC, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_Ljava_SL_lang_SL_Object_SC_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_O, U_b, U_j, U_e, U_c, U_t, U_SC, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU};

char StringConstant::U8S_smallest_int[] = {U_MINUS, U_2, U_1, U_4, U_7, U_4, U_8, U_3, U_6, U_4, U_8, U_NU}, // "-2147483648"
     StringConstant::U8S_smallest_long_int[] = {U_MINUS, U_9, U_2, U_2, U_3, U_3, U_7, U_2, U_0, U_3, U_6,
                                                         U_8, U_5, U_4, U_7, U_7, U_5, U_8, U_0, U_8, U_NU}, // "-9223372036854775808"
     StringConstant::U8S_NaN[] = {U_N, U_a, U_N, U_NU}, // "NaN"
     StringConstant::U8S_pos_Infinity[] = {U_I, U_n, U_f, U_i, U_n, U_i, U_t, U_y, U_NU}, // "Infinity"
     StringConstant::U8S_neg_Infinity[] = {U_MINUS, U_I, U_n, U_f, U_i, U_n, U_i, U_t, U_y, U_NU}, // "-Infinity"
     StringConstant::U8S_pos_Zero[] = {U_0, U_DOT, U_0, U_NU}, // "0.0"
     StringConstant::U8S_neg_Zero[] = {U_MINUS, U_0, U_DOT, U_0, U_NU}; // "-0.0"

int StringConstant::U8S_ConstantValue_length = strlen(U8S_ConstantValue),
    StringConstant::U8S_Exceptions_length = strlen(U8S_Exceptions),
    StringConstant::U8S_InnerClasses_length = strlen(U8S_InnerClasses),
    StringConstant::U8S_Synthetic_length = strlen(U8S_Synthetic),
    StringConstant::U8S_Deprecated_length = strlen(U8S_Deprecated),
    StringConstant::U8S_LineNumberTable_length = strlen(U8S_LineNumberTable),
    StringConstant::U8S_LocalVariableTable_length = strlen(U8S_LocalVariableTable),
    StringConstant::U8S_Code_length = strlen(U8S_Code),
    StringConstant::U8S_Sourcefile_length = strlen(U8S_Sourcefile),

    StringConstant::U8S_null_length = strlen(U8S_null),
    StringConstant::U8S_this_length = strlen(U8S_this);


Ostream Coutput;

ErrorString::ErrorString():ConvertibleArray<wchar_t>(1024), fill_char(' '), field_width(0)
{
}

void ErrorString::do_fill(int n)
{
    while (n < field_width)
    {
        Next() = (wchar_t) fill_char;
        n++;
    }
    field_width = 0;
}

ErrorString &ErrorString::operator<<(const wchar_t c)
{
    do_fill(1);
    Next() = c;
    return *this;
}

ErrorString &ErrorString::operator<<(const char c)
{
    do_fill(1);
    Next() = (wchar_t) c;
    return *this;
}

ErrorString &ErrorString::operator<<(const wchar_t *s)
{
    do_fill(
            wcslen(
#ifdef HAVE_ERROR_CALL_WCSLEN_CONST
                   (wchar_t *)
#endif
                   s)
            );
    if (s)
        while (*s)
            Next() = *(s++);

    return *this;
}

ErrorString &ErrorString::operator<<(const char *s)
{
    do_fill(strlen(s));
    if (s)
        while (*s)
            Next() = (wchar_t) *(s++);
    
    return *this;
}

ErrorString &ErrorString::operator<<(int n)
{
    char buf[64];
    sprintf(buf, "%d", n);
    
    return (*this << buf);
}

wchar_t *ErrorString::Array()
{
    Next() = U_NULL; // zero terminate string
    wchar_t *s = ConvertibleArray<wchar_t>::Array();
    if (!s)
        return NULL;

    //TODO: optimize!
    wchar_t *res = new wchar_t[top];
    memcpy(res, s, top * sizeof(wchar_t));
    return res;
}

void ErrorString::width(int w)
{
    field_width = w;
}

void ErrorString::fill(const char c)
{
    fill_char = c;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

