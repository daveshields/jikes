// $Id: config.h,v 1.28 1999/11/03 00:46:30 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef config_INCLUDED
#define config_INCLUDED

#include <wchar.h>
#include <new.h>
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "string.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef TEST
#define NO_LEAKS
#endif

//
// We need to set up certain environment information depending on
// the host OS.
//
// Jikes uses the stat() system call to query file status. Traditional
// unix systems use S_IFDIR and S_IFREG as names for constants used to
// see if file is directory or regular file, respectively. POSIX-based
// systems use __S_IFDIR and __S_IFREG, with S_IFDIR and S_IFREG being
// defined as macros
//
#ifdef STAT_POSIX
#define STAT_S_IFDIR __S_IFDIR
#define STAT_S_IFREG __S_IFREG
#else
#define STAT_S_IFDIR S_IFDIR
#define STAT_S_IFREG S_IFREG
#endif

#ifdef STAT_POSIX_1
// Some variants of HPUX want just one underscore
#undef STAT_S_IFDIR
#undef STAT_S_IFREG
#define STAT_S_IFDIR _S_IFDIR
#define STAT_S_IFREG _S_IFREG
#endif
//
// These definitions are correct for all the platforms that
// we are currently using. When porting this code, they should
// always be reviewed.
//
#include <limits.h>

#if ! (UINT_MAX == 0xFFFFFFFF)
#error unsigned int does not store values in the range 0..4294967295 in this system
#else
    typedef unsigned int u4;
#endif

#if ! ((INT_MAX == 2147483647) && (INT_MIN + 1 == -INT_MAX))
#error signed int does not store values in the range -2147483648..+2147483647 in this system
#else
    typedef signed int i4;
#endif

#if ! (USHRT_MAX == 0xFFFF)
#error unsigned short does not store values in the range 0..65535 in this system
#else
    typedef unsigned short u2;
#endif

#if ! ((SHRT_MAX == 32767) && (SHRT_MIN + 1 == -SHRT_MAX))
#error signed short does not store values in the range -32767..+32768 in this system
#else
    typedef signed short i2;
#endif

#if ! (UCHAR_MAX == 0xFF)
#error unsigned char does not store values in the range 0..255 in this system
#else
    typedef unsigned char u1;
#endif

#if ! ((SCHAR_MAX == 127) && (SCHAR_MIN + 1 == -SCHAR_MAX))
#error signed character does not store values in the range -128..+127 in this system
#else
    typedef signed char i1;
#endif

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

    U_chars_size
};


//
// Constant strings used in the program.
//
class StringConstant
{
public:
    static wchar_t US_AND[], // "&"
                   US_AND_AND[], // "&&"
                   US_AND_EQUAL[], // "&="
                   US_COLON[], // ":"
                   US_COMMA[], // ","
                   US_DIVIDE[], // "/"
                   US_DIVIDE_EQUAL[], // "/="
                   US_DOT[], // "."
                   US_EMPTY[], // ""
                   US_EQUAL[], // "="
                   US_EQUAL_EQUAL[], // "=="
                   US_GREATER[], // ">"
                   US_GREATER_EQUAL[], // ">="
                   US_LBRACE[], // "{"
                   US_LBRACKET[], // "["
                   US_LEFT_SHIFT[], // "<<"
                   US_LEFT_SHIFT_EQUAL[], // "<<="
                   US_LESS[], // "<"
                   US_LESS_EQUAL[], // "<="
                   US_LPAREN[], // "("
                   US_MINUS[], // "-"
                   US_MINUS_EQUAL[], // "-="
                   US_MINUS_MINUS[], // "--"
                   US_MULTIPLY[], // "*"
                   US_MULTIPLY_EQUAL[], // "*="
                   US_NOT[], // "!"
                   US_NOT_EQUAL[], // "!="
                   US_OR[], // "|"
                   US_OR_EQUAL[], // "|="
                   US_OR_OR[], // "||"
                   US_PLUS[], // "+"
                   US_PLUS_EQUAL[], // "+="
                   US_PLUS_PLUS[], // "++"
                   US_QUESTION[], // "?"
                   US_RBRACE[], // "}"
                   US_RBRACKET[], // "]"
                   US_REMAINDER[], // "%"
                   US_REMAINDER_EQUAL[], // "%="
                   US_RIGHT_SHIFT[], // ">>"
                   US_RIGHT_SHIFT_EQUAL[], // ">>="
                   US_RPAREN[], // ")"
                   US_SEMICOLON[], // ";"
                   US_TWIDDLE[], // "~"
                   US_UNSIGNED_RIGHT_SHIFT[], // ">>>"
                   US_UNSIGNED_RIGHT_SHIFT_EQUAL[], // ">>>="
                   US_XOR[], // "^"
                   US_XOR_EQUAL[], // "^="

                   US_Boolean[], // "Boolean"
                   US_Byte[], // "Byte"
                   US_Character[], // "Character"
                   US_Class[], // "Class"
                   US_ClassNotFoundException[], // "ClassNotFoundException"
                   US_Cloneable[], // "Cloneable"
                   US_Double[], // "Double"
                   US_Error[], // "Error"
                   US_Exception[], // "Exception"
                   US_Float[],  // "Float"
                   US_Integer[], // "Integer"
                   US_L[], // "L"
                   US_Long[], // "Long"
                   US_NoClassDefFoundError[], // "NoClassDefFoundError"
                   US_Object[], // "Object"
                   US_PObject[], // "PObject"
                   US_RuntimeException[], // "RuntimeException"
                   US_Serializable[], // "Serializable"
                   US_Short[], // "Short"
                   US_StringBuffer[], // "StringBuffer"
                   US_String[], // "String"
                   US_TYPE[], // "TYPE"
                   US_Vector[], // Vector
                   US_Throwable[], // "Throwable"
                   US_Void[], // "Void"
                   US__DO[], // "."
                   US__DO__DO[], // ".."
                   US__DS[], // "$"
                   US__LB__RB[], // "[]"
                   US__LT_clinit_GT[], // "<clinit>"
                   US__LT_init_GT[], // "<init>"
                   US__QU__QU[],  // "??"
                   US__SC[], // ";"
                   US__SL[], // "/"

                   US__zip[], // "zip"
                   US__jar[], // "jar"

                   US__array[], // "array"
                   US__access_DOLLAR[], // "access$"
                   US__class_DOLLAR[], // "class$"
                   US__constructor_DOLLAR[], // "constructor$"
                   US__this_DOLLAR[], // "this$"
                   US__val_DOLLAR[], // "val$"

                   US_abstract[], // "abstract"
                   US_append[], // "append"
                   US_block_DOLLAR[], // "block$"
                   US_boolean[], // "boolean"
                   US_break[], // "break"
                   US_byte[], // "byte"
                   US_case[], // "case"
                   US_catch[], // "catch"
                   US_char[], // "char"
                   US_class[], // "class"
                   US_clone[], // "clone"
                   US_const[], // "const"
                   US_continue[], // "continue"
                   US_default[], // "default"
                   US_do[], // "do"
                   US_double[], // "double"
                   US_else[], // "else"
                   US_extends[], // "extends"
                   US_false[], // "false"
                   US_final[], // "final"
                   US_finally[], // "finally"
                   US_float[], // "float"
                   US_for[], // "for"
                   US_forName[], // "forName"
                   US_getMessage[], // "getMessage"
                   US_goto[], // "goto"
                   US_if[], // "if"
                   US_implements[], // "implements"
                   US_import[], // "import"
                   US_instanceof[], // "instanceof"
                   US_int[], // "int"
                   US_interface[], // "interface"
                   US_java_SL_io[], // "java/io"
                   US_java_SL_lang[], // "java/lang"
                   US_java_SL_util[], // "java/util"
                   US_length[], // "length"
                   US_long[], // "long"
                   US_native[], // "native"
                   US_new[], // "new"
                   US_null[], // "null"
                   US_package[], // "package"
                   US_private[], // "private"
                   US_protected[], // "protected"
                   US_public[], // "public"
                   US_return[], // "return"
                   US_short[], // "short"
                   US_static[], // "static"
                   US_strictfp[], // "strictfp"
                   US_super[], // "super"
                   US_switch[], // "switch"
                   US_synchronized[], // "synchronized"
                   US_this0[], // "this$0"
                   US_this[], // "this"
                   US_throw[], // "throw"
                   US_throws[], // "throws"
                   US_toString[], // "toString"
                   US_transient[], // "transient"
                   US_true[], // "true"
                   US_try[], // "try"
                   US_void[], // "void"
                   US_volatile[], // "volatile"
                   US_while[], // "while"

                   US_EOF[]; // "EOF"

    static wchar_t US_smallest_int[]; // "-2147483648"

    static char U8S_command_format[];

    static int U8S_ConstantValue_length,
               U8S_Exceptions_length,
               U8S_InnerClasses_length,
               U8S_Synthetic_length,
               U8S_Deprecated_length,
               U8S_LineNumberTable_length,
               U8S_LocalVariableTable_length,
               U8S_Code_length,
               U8S_Sourcefile_length,

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
                U8S_LP_Ljava_SL_lang_SL_String_SC_RP_Ljava_SL_lang_SL_Class_SC[], // "(Ljava/lang/String;)Ljava/lang/Class;"
                U8S_LP_Ljava_SL_lang_SL_String_SC_RP_V[], // "(Ljava/lang/String;)V"
                U8S_LP_RP_Ljava_SL_lang_SL_String_SC[], // "()Ljava/lang/String;"
                U8S_LP_RP_V[], // "()V"
                U8S_LineNumberTable[], // "LineNumberTable"
                U8S_LocalVariableTable[], // "LocalVariableTable"
                U8S_S[], // "S"
                U8S_Sourcefile[], // "Sourcefile"
                U8S_Synthetic[], // "Synthetic"
                U8S_Deprecated[], // "Deprecated"
                U8S_V[], // "V"
                U8S_Z[], // "Z"

                U8S__DO[], // "."
                U8S__DO_class[], // ".class"
                U8S__DO_java[], // ".java"
                U8S__DO_tok[], // ".tok"
                U8S__DO_u[], // ".u"
                U8S__LP[], // "("
                U8S__RP[], // ")"
                U8S__SL[], // "/"
                U8S__ST[], // "*"

                U8S_class[], // "class"
                U8S_java[], // "java"
                U8S_java_SL_lang_SL_ClassNotFoundException[], // "java/lang/ClassNotFoundException"
                U8S_java_SL_lang_SL_Class[], // "java/lang/Class"
                U8S_java_SL_lang_SL_InternalError[], // "java/lang/InternalError"
                U8S_java_SL_lang_SL_NoClassDefFoundError[], // "java/lang/NoClassDefFoundError"
                U8S_java_SL_lang_SL_StringBuffer[], // "java/lang/StringBuffer"
                U8S_java_SL_lang_SL_Throwable[], // "java/lang/Throwable"
                U8S_null[], // "null"
                U8S_quit[], // "quit"
                U8S_this[], // "this"

                U8S_LP_LB_C_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "([C)Ljava/lang/StringBuffer;"
                U8S_LP_C_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(C)Ljava/lang/StringBuffer;"
                U8S_LP_Z_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(Z)Ljava/lang/StringBuffer;"
                U8S_LP_I_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(I)Ljava/lang/StringBuffer;"
                U8S_LP_J_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(J)Ljava/lang/StringBuffer;"
                U8S_LP_F_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(F)Ljava/lang/StringBuffer;"
                U8S_LP_D_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(D)Ljava/lang/StringBuffer;"
                U8S_LP_Ljava_SL_lang_SL_String_SC_RP_Ljava_SL_lang_SL_StringBuffer_SC[], // "(Ljava/lang/String;)Ljava/lang/StringBuffer;"
                U8S_LP_Ljava_SL_lang_SL_Object_SC_RP_Ljava_SL_lang_SL_StringBuffer_SC[]; // "(Ljava/lang/Object;)Ljava/lang/StringBuffer;"

    static char U8S_smallest_int[],      // "-2147483648"
                U8S_smallest_long_int[]; // "-9223372036854775808"
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
// Convert an unsigned Long integer to its character string representation in decimal.
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
// Convert a signed or unsigned Long integer to its character string representation in octal.
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
// Convert a signed or unsigned Long integer to its character string representation in hexadecimal.
//
class LongToHexString
{
public:
    LongToHexString(BaseLong &);

    char *String() { return str + 2; } // +2 to skip initial "0x"
    char *StringWithBase() { return str; }
    int Length()   { return (&info[TAIL_INDEX]) - str; }

private:
    enum { TAIL_INDEX = 1 + 1 + 16 }; // 1 for initial '0', +1 for 'x', + 16 significant digits

    char info[TAIL_INDEX + 1], // +1 for '\0'
         *str;
};


//
// Convert a signed Long integer to its character string representation in decimal.
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


#if defined(GNU_LIBC5)
    extern size_t wcslen(wchar_t *);
    extern wchar_t *wcscpy(wchar_t *, wchar_t *);
    extern wchar_t *wcsncpy(wchar_t *, wchar_t *, int);
    extern wchar_t *wcscat(wchar_t *, wchar_t *);
    extern int wcscmp(wchar_t *, wchar_t *);
    extern int wcsncmp(wchar_t *, wchar_t *, int);
#endif

#if defined(CYGWIN)
    extern wchar_t *wcscpy(wchar_t *, wchar_t *);
    extern wchar_t *wcscat(wchar_t *, wchar_t *);
    extern int wcsncmp(wchar_t *, wchar_t *, int);
#endif


//
// Convert an double to its character string representation.
//
class IEEEdouble;
class DoubleToString
{
public:
    DoubleToString(IEEEdouble &);

    char *String() { return str; }
    int Length()   { return length; }

private:
    enum
    {
        MAXIMUM_PRECISION = 16,
        MAXIMUM_STR_LENGTH = 1 + MAXIMUM_PRECISION + 1 + 4 // +1 for sign, +16 significant digits +1 for ".", +4 for exponent
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
    FloatToString(IEEEfloat &);

    char *String() { return str; }
    int Length()   { return length; }

private:
    enum
    {
        MAXIMUM_PRECISION = 8,
        MAXIMUM_STR_LENGTH = 1 + MAXIMUM_PRECISION + 1 + 4 // +1 for sign, +8 significant digits +1 for ".", +4 for exponent
    };

    char str[MAXIMUM_STR_LENGTH + 1]; // +1 for '\0'
    int length;
};

//
// If the system runs out of memory, this function is invoked.
//
void SetNewHandler();
#ifdef MICROSOFT
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
extern FILE *SystemFopen(char *name, char *mode);
extern size_t SystemFread(char *ptr, size_t element_size, size_t count, FILE *stream);
extern int SystemIsDirectory(char *name);


#ifdef UNIX
#define UNIX_FILE_SYSTEM
#endif
extern int SystemMkdirhier(char *);

extern char * strcat3(const char *, const char *, const char *);
extern char * wstring2string(wchar_t * in);

//
// The symbol used in this environment for separating argument in a system string. E.g., in a unix system
// directories specified in the CLASSPATH are separated by a ':', whereas in win95 it is ';'.
//
extern char PathSeparator();
extern int SystemMkdir(char *);
extern int SystemMkdirhier(char *);

extern char * strcat3(const char *, const char *, const char *);
extern char * wstring2string(wchar_t * in);

//
// Some compilers do not correctly predefine the primitive type "bool"
// and its possible values: "false" and "true"
//
#ifndef TYPE_bool
//======================================================================
// We define the type "bool" and the constants "false" and "true".
// The type bool as well as the constants false and true are expected
// to become standard C++. When that happens, these declarations should
// be removed.
//======================================================================
typedef unsigned char bool;
enum { false = 0, true = 1 };
#endif

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
    void SetExpandWchar(bool val = true) { expand_wchar = val; }
    bool ExpandWchar() { return expand_wchar; }

    Ostream &operator<<(wchar_t ch)
    {
        if (ch >> 8 == 0)
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
                    int d = ch % 16;
                    switch(d)
                    {
                        case 10:
                            str[i] = U_A;
                            break;
                        case 11:
                            str[i] = U_B;
                            break;
                        case 12:
                            str[i] = U_C;
                            break;
                        case 13:
                            str[i] = U_D;
                            break;
                        case 14:
                            str[i] = U_E;
                            break;
                        case 15:
                            str[i] = U_F;
                            break;
                        default:
                            str[i] = U_0 + d;
                            break;
                    }
                    ch /= 16;
                }
                *os << str[0];
                *os << str[1];
                *os << str[2];
                *os << str[3];
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
        *os << c;
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

    long setf(long setbits)
    {
        return os -> setf(setbits);
    }

    Ostream &operator<<(ios &(*f)(ios&))
    {
        (*f)(*os);
        return *this;
    }
};

extern Ostream Coutput;

//
// From now on, DO NOT USE cout and cerr !!!
//
#define cout Please_Do_Not_Use_cout_Directly_But_use_an_instance_of_Ostream_with_cout_as_argument
#define cerr Please_Do_Not_Use_cerr_Directly_But_use_an_instance_of_Ostream_with_cerr_as_argument

#endif // #ifndef config_INCLUDED
