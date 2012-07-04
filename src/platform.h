// $Id: platform.h,v 1.37 2002/08/02 21:29:47 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 2000, 2001, 2002 International Business
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

#ifndef platform_INCLUDED
#define platform_INCLUDED


#include <config.h>

/*
   undefine any symbols defined by the autotools
   build process that can conflict with our symbols
*/

#undef PACKAGE
#undef PACKAGE_NAME
#undef VERSION


/* Boilerplate autoconf checking */

#include <sys/types.h>
//FIXME: all stat stuff should be included in the platform.cpp file!
#include <sys/stat.h>
#ifdef STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# ifdef HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#ifdef HAVE_STRING_H
# ifndef STDC_HEADERS
#  ifdef HAVE_MEMORY_H
#   include <memory.h>
#  endif
# endif
# include <string.h>
#else
# ifdef HAVE_STRINGS_H
#  include <strings.h>
# endif
#endif

#ifdef HAVE_SYS_CYGWIN_H
#include <sys/cygwin.h>
#endif

#if defined(HAVE_LIBICU_UC)
# include <unicode/ucnv.h>
#elif defined(HAVE_ICONV_H)
# include <iconv.h>
# include <errno.h>
#endif

/*
Currently, we do not use this one
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
*/

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

#ifdef HAVE_DIRECT_H
# include <direct.h>
#endif

#ifdef HAVE_DIRENT_H
# include <dirent.h>
#endif

#ifndef HAVE_WINT_T
/* On some systems the type wint_t is not defined in wchar.h */
typedef unsigned int wint_t;
#endif

#ifdef HAVE_WCHAR_H
# include <wchar.h>
#endif

#ifdef HAVE_CTYPE_H
# include <ctype.h>
#endif

#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif

#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#ifdef HAVE_MATH_H
# include <math.h>
#endif

#ifdef HAVE_FLOAT_H
# include <float.h>
#endif

#ifdef HAVE_TIME_H
# include <time.h>
#endif

// C++ standard support

#ifdef HAVE_STD
# include <iostream>
# include <fstream>
#else
# include <iostream.h>
# include <fstream.h>
#endif

// VC++ pretends to support the C++ standard, but it does not.
// The set_new_handler method in <new> is not implemented so
// the _set_new_handler method in <new.h> must be used.

#ifdef HAVE_VCPP_SET_NEW_HANDLER
# include <new.h>
#else
# ifdef HAVE_STD
#  include <new>
# else
#  include <new.h>
# endif
#endif

#ifdef HAVE_STD
# ifdef HAVE_NAMESPACES
   using namespace std;
# endif
#endif


#ifdef HAVE_BROKEN_USHRT_MAX
    /* There is a bug in mingwin's limits.h file we need to work around */
# undef USHRT_MAX
# ifdef SHRT_MAX
#  define USHRT_MAX (2U * SHRT_MAX + 1)
# else
#  define USHRT_MAX 0xFFFF
# endif
#endif


//
// These limit definitions are correct for all the platforms that
// we are currently using. When porting this code, they should
// always be reviewed.
//

#ifdef HAVE_32BIT_TYPES

// MS VC++ defines __int64
# ifdef HAVE_UNSIGNED_INT64
// Range 0..18446744073709551615
typedef unsigned __int64 u8;

// Range -9223372036854775808..9223372036854775807
typedef signed __int64 i8;
# endif // HAVE_UNSIGNED_INT64

// gcc defines long long
# ifdef HAVE_UNSIGNED_LONG_LONG
// Range 0..18446744073709551615
typedef unsigned long long u8;

// Range -9223372036854775808..9223372036854775807
typedef signed long long i8;
# endif // HAVE_UNSIGNED_LONG_LONG

// Range 0..4294967295
typedef unsigned int u4;

// Range -2147483648..+2147483647
typedef signed int i4;

// Range 0..65535
typedef unsigned short u2;

// Range -32768..32767
typedef signed short i2;

// Range 0..255 in this system
typedef unsigned char u1;

// Range -128..+127 in this system
typedef signed char i1;

#endif // HAVE_32BIT_TYPES


//
// Some compilers do not correctly predefine the primitive type "bool"
// and its possible values: "false" and "true"
//
#ifndef HAVE_BOOL
//======================================================================
// We define the type "bool" and the constants "false" and "true".
// The type bool as well as the constants false and true are expected
// to become standard C++. When that happens, these declarations should
// be removed.
//======================================================================
typedef unsigned char bool;
enum { false = 0, true = 1 };
#endif


// tuple.h needs the above typedefs first, but has it's own namespace block...
// cabbey would also argue that the wsclen and family don't need to be in our
// namespace, 'cuz if we need to define them we aren't going to clash. ;)
#include "tuple.h"



#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

//
// The configure scripts check each of these to see if we need our own
// implementation
//

#ifndef HAVE_WCSLEN
    extern size_t wcslen(const wchar_t *);
#endif

#ifndef HAVE_WCSCPY
    extern wchar_t *wcscpy(wchar_t *, const wchar_t *);
#endif

#ifndef HAVE_WCSNCPY
    extern wchar_t *wcsncpy(wchar_t *, const wchar_t *, size_t);
#endif

#ifndef HAVE_WCSCAT
    extern wchar_t *wcscat(wchar_t *, const wchar_t *);
#endif

#ifndef HAVE_WCSCMP
    extern int wcscmp(const wchar_t *, const wchar_t *);
#endif

#ifndef HAVE_WCSNCMP
    extern int wcsncmp(const wchar_t *, const wchar_t *, size_t);
#endif



//
// If the system runs out of memory, this function is invoked.
//
void SetNewHandler();

#ifdef HAVE_VCPP_SET_NEW_HANDLER
extern int OutOfMemory(size_t);
#else
extern void OutOfMemory();
#endif

//
// When using the ICC compiler on Win95 or OS/2, we need to disable
// testing for various floating point exceptions. Default behavior
// was causing problems reading some standard class files.
//
extern void FloatingPointCheck();


//
// variants of system functions
// are declared here and defined in code.cpp
//
extern int SystemStat(const char *name, struct stat *stat_struct);
extern FILE *SystemFopen(const char *name, const char *mode);
extern size_t SystemFread(char *ptr, size_t element_size, size_t count,
                          FILE *stream);
extern int SystemIsDirectory(char *name);

//
// The symbol used in this environment for separating argument in a system
// string. E.g., in a unix system directories specified in the CLASSPATH
// are separated by a ':', whereas in win95 it is ';'.
//
extern char PathSeparator();
extern int SystemMkdir(char *);
extern int SystemMkdirhier(char *);
extern int SystemMkdirhierForFile(char *);

extern char * strcat3(const char *, const char *, const char *);
extern char * wstring2string(wchar_t * in);




// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
// End of platform specific defines in this file, the rest of the code
// in this file should work on any system
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



// The following comment was part of the original config.h, it explains why
// some parts of the guts of the compiler are written the way they are. This
// comment was removed when the ifdef'd EBCDIC code was initially removed.
// I've added it back for its "why do they do it that way" value, hopefully
// its presence will help prevent people from making assumptions and breaking
// the code for non-ascii platforms. At some point we'll want to be able to
// compile on EBCDIC systems again, using the API and iconv/ICU instead of
// ifdefs scattered all over creation, if the core code hasn't been
// compromised this should be easier. For those that have no idea what EBCDIC
// is... it is like ASCII in that it is an encoding system, but it predates
// ASCII by a number of years (think punchcards). It is still the default
// encoding on some mainframes and minicomputers such as the IBM zSeries and
// iSeries (aka the 390 and the 400). To see what the old EBCDIC code looked
// like, pull a cvs tree with the tag v1-06.
//
//
// Jikes can be compiled on systems using the EBCDIC character set, for which
// character and string literals are translated by the compiler to EBCDIC and
// not ASCII, and so cannot be used to represent ASCII/UNICODE values. Such
// values are constructed using the U_ values defined below. Thus 'a' is
// represented using U_a, and ".java" is represented by an explicit literal:
//    {U_DOT, U_j, U_a, U_v, U_a, U_NULL}
// Variables associated with such literals have names beginning with US_ if
// the value are 16-bits or U8S_ for 8 bits. The initial underscore is
// followed by the characters being represented, with letters and digits
// representing themselves, and other values, all of which have a two
// character code, surrounded by underscore. Thus the name used for the
// literal above is US_DO_java.
//
// All string-related values are represented internally in ASCII/UNICODE
// using the U_ values defined below. EBCDIC systems also require that
// arguments to system functions representing file names be converted from
// the internal form used by Jikes to EBCDIC, and such functions are referred
// to using their usual name prefixed with "system_"; for example, a call to
// "fopen(..." is written "system_fopen(..." The "system_" procedures are
// define in the file code.cpp.
//

enum U_chars
{
    U_NULL = 0,               U_NU = U_NULL,            // L'\0'
    U_BACKSPACE = 8,          U_BS = U_BACKSPACE,       // L'\b'
    U_HORIZONTAL_TAB = 9,     U_HT = U_HORIZONTAL_TAB,  // L'\t'
    U_LINE_FEED = 10,         U_LF = U_LINE_FEED,       // L'\n'
    U_FORM_FEED = 12,         U_FF = U_FORM_FEED,       // L'\f'
    U_CARRIAGE_RETURN = 13,   U_CR = U_CARRIAGE_RETURN, // L'\r'

    U_CTL_Z = 26,
    U_ESCAPE = 27,

    U_SPACE = 32,             U_SP = U_SPACE,             // L' '
    U_EXCLAMATION = 33,       U_EX = U_EXCLAMATION,       // L'!'
    U_DOUBLE_QUOTE = 34,      U_DQ = U_DOUBLE_QUOTE,      // L'"'
    U_POUND = 35,             U_SH = U_POUND,             // L'#'
    U_DOLLAR = 36,            U_DS = U_DOLLAR,            // L'$'
    U_PERCENT = 37,           U_PE = U_PERCENT,           // L'%'
    U_AMPERSAND = 38,         U_AM = U_AMPERSAND,         // L'&'
    U_SINGLE_QUOTE = 39,      U_SQ = U_SINGLE_QUOTE,      // L'\''
    U_LEFT_PARENTHESIS = 40,  U_LP = U_LEFT_PARENTHESIS,  // L'('
    U_RIGHT_PARENTHESIS = 41, U_RP = U_RIGHT_PARENTHESIS, // L')'
    U_STAR = 42,              U_ST = U_STAR,              // L'*'
    U_PLUS = 43,              U_PL = U_PLUS,              // L'+'
    U_MINUS = 45,             U_MI = U_MINUS,             // L'-'
    U_COMMA = 44,             U_CM = U_COMMA,             // L','
    U_DOT = 46,               U_DO = U_DOT,               // L'.'
    U_SLASH = 47,             U_SL = U_SLASH,             // L'/'

    U_0 = 48, // L'0'
    U_1 = 49, // L'1'
    U_2 = 50, // L'2'
    U_3 = 51, // L'3'
    U_4 = 52, // L'4'
    U_5 = 53, // L'5'
    U_6 = 54, // L'6'
    U_7 = 55, // L'7'
    U_8 = 56, // L'8'
    U_9 = 57, // L'9'

    U_COLON = 58,             U_CO = U_COLON,     // L':'
    U_SEMICOLON = 59,         U_SC = U_SEMICOLON, // L';'
    U_LESS = 60,              U_LT = U_LESS,      // L'<'
    U_EQUAL = 61,             U_EQ = U_EQUAL,     // L'='
    U_GREATER = 62,           U_GT = U_GREATER,   // L'>'
    U_QUESTION = 63,          U_QU = U_QUESTION,  // L'?'
    U_AT = 64,                                    // L'@'

    U_A = 65, // L'A'
    U_B = 66, // L'B'
    U_C = 67, // L'C'
    U_D = 68, // L'D'
    U_E = 69, // L'E'
    U_F = 70, // L'F'
    U_G = 71, // L'G'
    U_H = 72, // L'H'
    U_I = 73, // L'I'
    U_J = 74, // L'J'
    U_K = 75, // L'K'
    U_L = 76, // L'L'
    U_M = 77, // L'M'
    U_N = 78, // L'N'
    U_O = 79, // L'O'
    U_P = 80, // L'P'
    U_Q = 81, // L'Q'
    U_R = 82, // L'R'
    U_S = 83, // L'S'
    U_T = 84, // L'T'
    U_U = 85, // L'U'
    U_V = 86, // L'V'
    U_W = 87, // L'W'
    U_X = 88, // L'X'
    U_Y = 89, // L'Y'
    U_Z = 90, // L'Z'

    U_LEFT_BRACKET = 91,      U_LB = U_LEFT_BRACKET,  // L'['
    U_BACKSLASH = 92,         U_RS = U_BACKSLASH,     // L'\\'
    U_RIGHT_BRACKET = 93,     U_RB = U_RIGHT_BRACKET, // L']'
    U_CARET = 94,             U_CA = U_CARET,         // L'^'
    U_UNDERSCORE = 95,        U_UN = U_UNDERSCORE,    // L'_'

    U_a = 97, // L'a'
    U_b = 98, // L'b'
    U_c = 99, // L'c'
    U_d = 100, // L'd'
    U_e = 101, // L'e'
    U_f = 102, // L'f'
    U_g = 103, // L'g'
    U_h = 104, // L'h'
    U_i = 105, // L'i'
    U_j = 106, // L'j'
    U_k = 107, // L'k'
    U_l = 108, // L'l'
    U_m = 109, // L'm'
    U_n = 110, // L'n',
    U_o = 111, // L'o'
    U_p = 112, // L'p'
    U_q = 113, // L'q'
    U_r = 114, // L'r'
    U_s = 115, // L's'
    U_t = 116, // L't'
    U_u = 117, // L'u'
    U_v = 118, // L'v'
    U_w = 119, // L'w'
    U_x = 120, // L'x'
    U_y = 121, // L'y'
    U_z = 122, // L'z'

    U_LEFT_BRACE = 123,       U_OS = U_LEFT_BRACE,  // L'{'
    U_BAR = 124,              U_BA = U_BAR,         // L'|'
    U_RIGHT_BRACE = 125,      U_CS = U_RIGHT_BRACE, // L'}'
    U_TILDE = 126,            U_TI = U_TILDE,       // L'~'

    U_BOM         = 0xfeff,
    U_REVERSE_BOM = 0xfffe
};


//
// Constant strings used in the program. Those prefixed with US_ are
// Unicode strings in wchar_t, and those with U8S are UTF8 strings in char.
//
class StringConstant
{
public:
    //
    // Symbols and operators.
    //
    static wchar_t US_AND[], // L"&"
                   US_AND_AND[], // L"&&"
                   US_AND_EQUAL[], // L"&="
                   US_COLON[], // L":"
                   US_COMMA[], // L","
                   US_DIVIDE[], // L"/"
                   US_DIVIDE_EQUAL[], // L"/="
                   US_DOT[], // L"."
                   US_EMPTY[], // L""
                   US_EQUAL[], // L"="
                   US_EQUAL_EQUAL[], // L"=="
                   US_GREATER[], // L">"
                   US_GREATER_EQUAL[], // L">="
                   US_LBRACE[], // L"{"
                   US_LBRACKET[], // L"["
                   US_LEFT_SHIFT[], // L"<<"
                   US_LEFT_SHIFT_EQUAL[], // L"<<="
                   US_LESS[], // L"<"
                   US_LESS_EQUAL[], // L"<="
                   US_LPAREN[], // L"("
                   US_MINUS[], // L"-"
                   US_MINUS_EQUAL[], // L"-="
                   US_MINUS_MINUS[], // L"--"
                   US_MULTIPLY[], // L"*"
                   US_MULTIPLY_EQUAL[], // L"*="
                   US_NOT[], // L"!"
                   US_NOT_EQUAL[], // L"!="
                   US_OR[], // L"|"
                   US_OR_EQUAL[], // L"|="
                   US_OR_OR[], // L"||"
                   US_PLUS[], // L"+"
                   US_PLUS_EQUAL[], // L"+="
                   US_PLUS_PLUS[], // L"++"
                   US_QUESTION[], // L"?"
                   US_RBRACE[], // L"}"
                   US_RBRACKET[], // L"]"
                   US_REMAINDER[], // L"%"
                   US_REMAINDER_EQUAL[], // L"%="
                   US_RIGHT_SHIFT[], // L">>"
                   US_RIGHT_SHIFT_EQUAL[], // L">>="
                   US_RPAREN[], // L")"
                   US_SEMICOLON[], // L";"
                   US_TWIDDLE[], // L"~"
                   US_UNSIGNED_RIGHT_SHIFT[], // L">>>"
                   US_UNSIGNED_RIGHT_SHIFT_EQUAL[], // L">>>="
                   US_XOR[], // L"^"
                   US_XOR_EQUAL[]; // L"^="

    //
    // Library classes and methods.
    //
    static wchar_t US_AssertionError[], // L"AssertionError"
                   US_Boolean[], // L"Boolean"
                   US_Byte[], // L"Byte"
                   US_Character[], // L"Character"
                   US_Class[], // L"Class"
                   US_ClassNotFoundException[], // L"ClassNotFoundException"
                   US_Cloneable[], // L"Cloneable"
                   US_Comparable[], // L"Comparable"
                   US_Double[], // L"Double"
                   US_Error[], // L"Error"
                   US_Exception[], // L"Exception"
                   US_Float[],  // L"Float"
                   US_Integer[], // L"Integer"
                   US_L[], // L"L"
                   US_Long[], // L"Long"
                   US_NoClassDefFoundError[], // L"NoClassDefFoundError"
                   US_Object[], // L"Object"
                   US_PObject[], // L"PObject"
                   US_RuntimeException[], // L"RuntimeException"
                   US_Serializable[], // L"Serializable"
                   US_Short[], // L"Short"
                   US_String[], // L"String"
                   US_StringBuffer[], // L"StringBuffer"
                   US_TYPE[], // L"TYPE"
                   US_Throwable[], // L"Throwable"
                   US_Void[], // L"Void"
                   US_DO[], // L"."
                   US_DO_DO[], // L".."
                   US_DS[], // L"$"
                   US_LB_RB[], // L"[]"
                   US_LT_clinit_GT[], // L"<clinit>"
                   US_LT_init_GT[], // L"<init>"
                   US_QU_QU[],  // L"??"
                   US_SC[], // L";"
                   US_SL[], // L"/"
                   US_zip[], // L"zip"
                   US_jar[]; // L"jar"

    //
    // Used in synthetic (compiler-generated) code.
    //
    static wchar_t US_DOLLAR_noassert[], // L"$noassert"
                   US_append[], // L"append"
                   US_array[], // L"array"
                   US_access_DOLLAR[], // L"access$"
                   US_class_DOLLAR[], // L"class$"
                   US_clone[], // L"clone"
                   US_constructor_DOLLAR[], // L"constructor$"
                   US_desiredAssertionStatus[], // L"desiredAssertionStatus"
                   US_forName[], // L"forName"
                   US_getClass[], // L"getClass"
                   US_getComponentType[], // L"getComponentType"
                   US_getMessage[], // L"getMessage"
                   US_initCause[], // L"initCause"
                   US_java_SL_io[], // L"java/io"
                   US_java_SL_lang[], // L"java/lang"
                   US_length[], // L"length"
                   US_this_DOLLAR[], // L"this$"
                   US_this0[], // L"this$0"
                   US_toString[], // L"toString"
                   US_val_DOLLAR[]; // L"val$"

    //
    // Java keywords.
    //
    static wchar_t US_abstract[], // L"abstract"
                   US_assert[], // L"assert"
                   US_boolean[], // L"boolean"
                   US_break[], // L"break"
                   US_byte[], // L"byte"
                   US_case[], // L"case"
                   US_catch[], // L"catch"
                   US_char[], // L"char"
                   US_class[], // L"class"
                   US_const[], // L"const"
                   US_continue[], // L"continue"
                   US_default[], // L"default"
                   US_do[], // L"do"
                   US_double[], // L"double"
                   US_else[], // L"else"
                   US_extends[], // L"extends"
                   US_false[], // L"false"
                   US_final[], // L"final"
                   US_finally[], // L"finally"
                   US_float[], // L"float"
                   US_for[], // L"for"
                   US_goto[], // L"goto"
                   US_if[], // L"if"
                   US_implements[], // L"implements"
                   US_import[], // L"import"
                   US_instanceof[], // L"instanceof"
                   US_int[], // L"int"
                   US_interface[], // L"interface"
                   US_long[], // L"long"
                   US_native[], // L"native"
                   US_new[], // L"new"
                   US_null[], // L"null"
                   US_package[], // L"package"
                   US_private[], // L"private"
                   US_protected[], // L"protected"
                   US_public[], // L"public"
                   US_return[], // L"return"
                   US_short[], // L"short"
                   US_static[], // L"static"
                   US_strictfp[], // L"strictfp"
                   US_super[], // L"super"
                   US_switch[], // L"switch"
                   US_synchronized[], // L"synchronized"
                   US_this[], // L"this"
                   US_throw[], // L"throw"
                   US_throws[], // L"throws"
                   US_transient[], // L"transient"
                   US_true[], // L"true"
                   US_try[], // L"try"
                   US_void[], // L"void"
                   US_volatile[], // L"volatile"
                   US_while[]; // L"while"

    //
    // Miscellaneous strings.
    //
    static wchar_t US_EOF[]; // L"EOF"

    static wchar_t US_smallest_int[]; // L"-2147483648"

    static char U8S_help_header[];
    static char U8S_command_format[];

    //
    // Constant pool entries.
    //
    static int U8S_ConstantValue_length,
               U8S_Exceptions_length,
               U8S_InnerClasses_length,
               U8S_Synthetic_length,
               U8S_Deprecated_length,
               U8S_LineNumberTable_length,
               U8S_LocalVariableTable_length,
               U8S_Code_length,
               U8S_SourceFile_length,

               U8S_null_length,
               U8S_this_length;

    static char U8S_B[], // "B"
                U8S_C[], // "C"
                U8S_Code[], // "Code"
                U8S_ConstantValue[], // "ConstantValue"
                U8S_D[], // "D"
                U8S_Exceptions[], // "Exceptions"
                U8S_F[], // "F"
                U8S_I[], // "I"
                U8S_InnerClasses[], // "InnerClasses"
                U8S_J[],  // "J"
                U8S_LP_C_RP_V[], // "(C)V"
                U8S_LP_D_RP_V[], // "(D)V"
                U8S_LP_F_RP_V[], // "(F)V"
                U8S_LP_I_RP_V[], // "(I)V"
                U8S_LP_J_RP_V[], // "(J)V"
                U8S_LP_Object_RP_V[], // "(Ljava/lang/Object;)V"
                U8S_LP_String_RP_Class[], // "(Ljava/lang/String;)Ljava/lang/Class;"
                U8S_LP_String_RP_V[], // "(Ljava/lang/String;)V"
                U8S_LP_Throwable_RP_Throwable[], // "(Ljava/lang/Throwable;)Ljava/lang/Throwable;"
                U8S_LP_RP_Class[], // "()Ljava/lang/Class;"
                U8S_LP_RP_String[], // "()Ljava/lang/String;"
                U8S_LP_RP_V[], // "()V"
                U8S_LP_RP_Z[], // "()Z"
                U8S_LP_Z_RP_V[], // "(Z)V"
                U8S_LineNumberTable[], // "LineNumberTable"
                U8S_LocalVariableTable[], // "LocalVariableTable"
                U8S_S[], // "S"
                U8S_SourceFile[], // "SourceFile"
                U8S_Synthetic[], // "Synthetic"
                U8S_Deprecated[], // "Deprecated"
                U8S_V[], // "V"
                U8S_Z[], // "Z"

                U8S_DO[], // "."
                U8S_DO_DO[], // ".."
                U8S_DO_class[], // ".class"
                U8S_DO_java[], // ".java"
                U8S_DO_tok[], // ".tok"
                U8S_DO_u[], // ".u"
                U8S_LP[], // "("
                U8S_RP[], // ")"
                U8S_SL[], // "/"
                U8S_ST[], // "*"

                U8S_class[], // "class"
                U8S_java[], // "java"
                U8S_java_SL_lang_SL_ClassNotFoundException[], // "java/lang/ClassNotFoundException"
                U8S_java_SL_lang_SL_Class[], // "java/lang/Class"
                U8S_java_SL_lang_SL_InternalError[], // "java/lang/InternalError"
                U8S_java_SL_lang_SL_NoClassDefFoundError[], // "java/lang/NoClassDefFoundError"
                U8S_java_SL_lang_SL_StringBuffer[], // "java/lang/StringBuffer"
                U8S_java_SL_lang_SL_Throwable[], // "java/lang/Throwable"
                U8S_false[], // "false"
                U8S_null[], // "null"
                U8S_quit[], // "quit"
                U8S_this[], // "this"
                U8S_true[], // "true"

                U8S_LP_C_RP_StringBuffer[], // "(C)Ljava/lang/StringBuffer;"
                U8S_LP_Z_RP_StringBuffer[], // "(Z)Ljava/lang/StringBuffer;"
                U8S_LP_I_RP_StringBuffer[], // "(I)Ljava/lang/StringBuffer;"
                U8S_LP_J_RP_StringBuffer[], // "(J)Ljava/lang/StringBuffer;"
                U8S_LP_F_RP_StringBuffer[], // "(F)Ljava/lang/StringBuffer;"
                U8S_LP_D_RP_StringBuffer[], // "(D)Ljava/lang/StringBuffer;"
                U8S_LP_String_RP_StringBuffer[], // "(Ljava/lang/String;)Ljava/lang/StringBuffer;"
                U8S_LP_Object_RP_StringBuffer[]; // "(Ljava/lang/Object;)Ljava/lang/StringBuffer;"

    static char U8S_smallest_int[],      // "-2147483648"
                U8S_smallest_long_int[], // "-9223372036854775808"
                U8S_NaN[],               // "NaN"
                U8S_pos_Infinity[],      // "Infinity"
                U8S_neg_Infinity[],      // "-Infinity"
                U8S_pos_Zero[],          // "0.0"
                U8S_neg_Zero[];          // "-0.0"
};


//
// Convert an integer to its character string representation.
//
class IntToString
{
public:
    IntToString(int);

    char *String() { return str; }
    int Length()   { return (&info[TAIL_INDEX]) - str; }

private:
    enum { TAIL_INDEX = 1 + 10 }; // 1 for sign, +10 significant digits

    char info[TAIL_INDEX + 1], // +1 for '\0'
         *str;
};


//
// Same as IntToString for wide strings
//
class IntToWstring
{
public:
    IntToWstring(int);

    wchar_t *String() { return wstr; }
    int Length()      { return (&winfo[TAIL_INDEX]) - wstr; }

private:
    enum { TAIL_INDEX = 1 + 10 }; // 1 for sign, +10 significant digits

    wchar_t winfo[TAIL_INDEX + 1], // 1 for sign, +10 significant digits + '\0'
            *wstr;
};


//
// Convert an unsigned Long integer to its character string representation in
// decimal.
//
class ULongInt;
class ULongToDecString
{
public:
    ULongToDecString(ULongInt &);

    char *String() { return str; }
    int Length()   { return (&info[TAIL_INDEX]) - str; }

private:
    enum { TAIL_INDEX = 20 }; // 20 significant digits

    char info[TAIL_INDEX + 1], // +1 for '\0'
         *str;
};


//
// Convert a signed or unsigned Long integer to its character string
// representation in octal.
//
class BaseLong;
class LongToOctString
{
public:
    LongToOctString(BaseLong &);

    char *String() { return str + 1; } // +1 to skip initial '0'
    char *StringWithBase() { return str; }
    int Length()   { return (&info[TAIL_INDEX]) - str; }

private:
    enum { TAIL_INDEX = 1 + 22 }; // 1 for initial '0', +22 significant digits

    char info[TAIL_INDEX + 1], // + 1 for '\0'
         *str;
};


//
// Convert a signed or unsigned Long integer to its character string
// representation in hexadecimal.
//
class LongToHexString
{
public:
    LongToHexString(BaseLong &);

    char *String() { return str + 2; } // +2 to skip initial "0x"
    char *StringWithBase() { return str; }
    int Length()   { return (&info[TAIL_INDEX]) - str; }

private:
    // 1 for initial '0', +1 for 'x', + 16 significant digits
    enum { TAIL_INDEX = 1 + 1 + 16 };

    char info[TAIL_INDEX + 1], // +1 for '\0'
         *str;
};


//
// Convert a signed Long integer to its character string representation in
// decimal.
//
class LongInt;
class LongToDecString
{
public:
    LongToDecString(LongInt &);

    char *String() { return str; }
    int Length()   { return (&info[TAIL_INDEX]) - str; }

private:
    enum { TAIL_INDEX = 1 + 19 }; // 1 for sign, +19 significant digits

    char info[TAIL_INDEX + 1], // + 1 for '\0'
         *str;
};

//
// Convert an double to its character string representation.
//
class IEEEdouble;
class DoubleToString
{
public:
    DoubleToString(const IEEEdouble &);

    const char *String() const { return str; }
    int Length() const { return length; }

private:
    void Format(char *, int, bool);

    enum
    {
        MAXIMUM_PRECISION = 17,
        // +1 for sign, +17 significant digits +1 for ".", +5 for exponent
        MAXIMUM_STR_LENGTH = 1 + MAXIMUM_PRECISION + 1 + 5
    };

    char str[MAXIMUM_STR_LENGTH + 1]; // +1 for '\0'
    int length;
};

//
// Convert an float to its character string representation.
//
class IEEEfloat;
class FloatToString
{
public:
    FloatToString(const IEEEfloat &);

    const char *String() const { return str; }
    int Length() const   { return length; }

private:
    void Format(char *, int, bool);

    enum
    {
        MAXIMUM_PRECISION = 9,
        // +1 for sign, +9 significant digits +1 for ".", +4 for exponent
        MAXIMUM_STR_LENGTH = 1 + MAXIMUM_PRECISION + 1 + 4
    };

    char str[MAXIMUM_STR_LENGTH + 1]; // +1 for '\0'
    int length;
};



class LongInt;
class ULongInt;
//
//
//
class Ostream
{
    ostream *os;

    bool expand_wchar;

public:

    Ostream() : os(&cerr),
                expand_wchar(false)
    {}

    Ostream(ostream *_os) : os(_os),
                            expand_wchar(false)
    {}

    void StandardOutput() { os = &cout; }
    void StandardError() { os = &cerr; }
    void SetExpandWchar(bool val = true) { expand_wchar = val; }
    bool ExpandWchar() { return expand_wchar; }

    Ostream &operator<<(wchar_t ch)
    {
        // output only printable characters directly
        if (ch == U_CARRIAGE_RETURN || ch == U_LINE_FEED)
            *os << (char) U_LINE_FEED;
        else if (ch >= U_SPACE && ch < 0x0ff)
            *os << (char) ch;
        else
        {
            if (expand_wchar == 0)
                *os << (char) U_QUESTION;
            else
            {
                *os << (char) U_BACKSLASH
                    << (char) U_u;
                char str[4];
                for (int i = 3; i >= 0; i--)
                {
                    int d = ch & 0x0f;
                    switch (d)
                    {
                    case 10: case 11: case 12: case 13: case 14: case 15:
                        str[i] = U_A - 10 + d;
                        break;
                    default:
                        str[i] = U_0 + d;
                        break;
                    }
                    ch >>= 4;
                }
                *os << str[0] << str[1] << str[2] << str[3];
            }
        }

        return *this;
    }

    Ostream &operator<<(const wchar_t *str)
    {
        for (; *str; str++)
            (*this) << *str;
        return *this;
    }


    Ostream &operator<<(char c)
    {
        *os << c;
        return *this;
    }

    Ostream &operator<<(signed char c)
    {
        *os << c;
        return *this;
    }

    Ostream &operator<<(unsigned char c)
    {
        *os << c;
        return *this;
    }

    Ostream &operator<<(const char *c)
    {
        *os << c;
        return *this;
    }

    Ostream &operator<<(const signed char *c)
    {
        *os << c;
        return *this;
    }

    Ostream &operator<<(const unsigned char *c)
    {
#ifndef HAVE_OSTREAM_CONST_UNSIGNED_CHAR_PTR
# ifdef HAVE_CONST_CAST
        *os << const_cast<unsigned char *> (c);
# else
        *os << (unsigned char *) c;
# endif
#else
        *os << c;
#endif
        return *this;
    }

    Ostream &operator<<(int a)
    {
        *os << a;
        return *this;
    }

    Ostream &operator<<(unsigned int a)
    {
        *os << a;
        return *this;
    }

    Ostream &operator<<(LongInt);
    Ostream &operator<<(ULongInt);

    Ostream &operator<<(float f)
    {
        *os << f;
        return *this;
    }

    Ostream &operator<<(double d)
    {
        *os << d;
        return *this;
    }

    char fill(char c) { return os -> fill(c); }

    Ostream &flush()
    {
        os -> flush();
        return *this;
    }

    int width(int w)
    {
        return os -> width(w);
    }

    Ostream &operator<<(ios &(*f)(ios&))
    {
        (*f)(*os);
        return *this;
    }

    Ostream &operator<<(ostream &(*f)(ostream&))
    {
        (*f)(*os);
        return *this;
    }
};

extern Ostream Coutput;

//
// From now on, DO NOT USE cout or cerr !
// Instead, use Coutput, which wraps either cout or cerr as determined by
// command line flags, and translates unicode output properly.
// If you try to use cerr or cout this define will
// create an undefined symbol and give you a linker error.
//
#define cout Please_Do_Not_Use_cout_Directly_But_use_an_instance_of_Ostream_with_cout_as_argument
#define cerr Please_Do_Not_Use_cerr_Directly_But_use_an_instance_of_Ostream_with_cerr_as_argument

// This is temporary solution.
// In future basic_ostringstream<wchar_t> will be used.
// But now it is not supported by libg++.
// (lord).
class ErrorString: public ConvertibleArray<wchar_t>
{
public:
    ErrorString();

    ErrorString &operator<<(const wchar_t *s);
    ErrorString &operator<<(const wchar_t c);
    ErrorString &operator<<(const char *s);
    ErrorString &operator<<(const char c);
    ErrorString &operator<<(int n);
    ErrorString &operator<<(ostream &(*f)(ostream&))
    {
        assert(f == (ostream &(*)(ostream&)) endl);
        return *this << '\n';
    }

    void width(int w);
    void fill(const char c);

    wchar_t *Array();

private:

    void do_fill(int n);
    char fill_char;
    int  field_width;
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // platform_INCLUDED

