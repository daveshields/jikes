// $Id: config.cpp,v 1.22 1999/09/13 14:21:14 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include "long.h"

Ostream &Ostream::operator<<(LongInt a)
{
    char str[25];

    if (os -> flags() & os -> dec)
         a.DecString(str);
    else if (os -> flags() & os -> oct)
         a.OctString(str, os -> flags() & os -> showbase);
    else if (os -> flags() & os -> hex)
         a.HexString(str, os -> flags() & os -> showbase);
    else
    {
         os -> flush();
         assert(! "know how to print signed long value in specified format yet !!!");
    }

    *os << str;

    return *this;
}

Ostream &Ostream::operator<<(ULongInt a)
{
    char str[25];

    if (os -> flags() & os -> dec)
        a.DecString(str);
    else if (os -> flags() & os -> oct)
         a.OctString(str, os -> flags() & os -> showbase);
    else if (os -> flags() & os -> hex)
         a.HexString(str, os -> flags() & os -> showbase);
    else
    {
         os -> flush();
         assert(! "know how to print unsigned long value in specified format yet !!!");
    }

    *os << str;

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
        StringConstant::US_RuntimeException[] = {U_R, U_u, U_n, U_t, U_i, U_m, U_e, U_E, U_x, U_c, U_e, U_p, U_t, U_i, U_o, U_n, U_NU}, // RuntimeException
        StringConstant::US_Serializable[] = {U_S, U_e, U_r, U_i, U_a, U_l, U_i, U_z, U_a, U_b, U_l, U_e, U_NU}, // Serializable
        StringConstant::US_Short[] = {U_S, U_h, U_o, U_r, U_t, U_NU}, // Short
        StringConstant::US_StringBuffer[] = {U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_NU}, // StringBuffer
        StringConstant::US_String[] = {U_S, U_t, U_r, U_i, U_n, U_g, U_NU}, // String
        StringConstant::US_TYPE[] = {U_T, U_Y, U_P, U_E, U_NU}, // "TYPE"
        StringConstant::US_Throwable[] = {U_T, U_h, U_r, U_o, U_w, U_a, U_b, U_l, U_e, U_NU}, // Throwable
        StringConstant::US_Void[] = {U_V, U_o, U_i, U_d, U_NU}, // Void
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

char StringConstant::U8S_command_format[] = "use: jikes [-classpath path][-d dir][-debug][-depend][-deprecation]"
                                            "[-g][-nowarn][-nowrite][-O][-verbose][-Xstdout]"
                                            "[+1.0][++][+B][+D][+E][+F][+K][+M][+P][+T][+U][+Z]"
                                            " file.java...";

char StringConstant::U8S_B[] = {U_B,U_NU}, // "B"
     StringConstant::U8S_C[] = {U_C,U_NU}, // "C"
     StringConstant::U8S_Code[] = {U_C,U_o,U_d,U_e,U_NU}, // "Code"
     StringConstant::U8S_ConstantValue[] = {U_C,U_o,U_n,U_s,U_t,U_a,U_n,U_t,U_V,U_a,U_l,U_u,U_e,U_NU}, // "ConstantValue"
     StringConstant::U8S_D[] = {U_D,U_NU}, // "D"
     StringConstant::U8S_Exceptions[] = {U_E,U_x,U_c,U_e,U_p,U_t,U_i,U_o,U_n,U_s,U_NU}, // "Exceptions"
     StringConstant::U8S_F[] = {U_F,U_NU}, // "F"
     StringConstant::U8S_I[] = {U_I,U_NU}, // "I"
     StringConstant::U8S_InnerClasses[] = {U_I,U_n,U_n,U_e,U_r,U_C,U_l,U_a,U_s,U_s,U_e,U_s,U_NU}, // "InnerClasses"
     StringConstant::U8S_J[] = {U_J,U_NU},  // "J"
     StringConstant::U8S_LP_Ljava_SL_lang_SL_String_SC_RP_Ljava_SL_lang_SL_Class_SC[] = {U_LP,U_L,U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_S,U_t,U_r,U_i,U_n,U_g,U_SC,U_RP,U_L,U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_C,U_l,U_a,U_s,U_s,U_SC,U_NU}, // "(Ljava/lang/String;)Ljava/lang/Class;"
     StringConstant::U8S_LP_Ljava_SL_lang_SL_String_SC_RP_V[] = {U_LP,U_L,U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_S,U_t,U_r,U_i,U_n,U_g,U_SC,U_RP,U_V,U_NU}, // "(Ljava/lang/String;)V"
     StringConstant::U8S_LP_RP_Ljava_SL_lang_SL_String_SC[] = {U_LP,U_RP,U_L,U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_S,U_t,U_r,U_i,U_n,U_g,U_SC,U_NU}, // "()_Ljava/lang/String;"
     StringConstant::U8S_LP_RP_V[] = {U_LP,U_RP,U_V,U_NU}, // "()V"
     StringConstant::U8S_LineNumberTable[] = {U_L,U_i,U_n,U_e,U_N,U_u,U_m,U_b,U_e,U_r,U_T,U_a,U_b,U_l,U_e,U_NU}, // "LineNumberTable"
     StringConstant::U8S_LocalVariableTable[] = {U_L,U_o,U_c,U_a,U_l,U_V,U_a,U_r,U_i,U_a,U_b,U_l,U_e,U_T,U_a,U_b,U_l,U_e,U_NU}, // "LocalVariableTable"
     StringConstant::U8S_S[] = {U_S,U_NU}, // "S"
     StringConstant::U8S_Sourcefile[] = {U_S,U_o,U_u,U_r,U_c,U_e,U_F,U_i,U_l,U_e,U_NU}, // "Sourcefile"
     StringConstant::U8S_Synthetic[] = {U_S,U_y,U_n,U_t,U_h,U_e,U_t,U_i,U_c,U_NU}, // "Synthetic"
     StringConstant::U8S_Deprecated[] = {U_D,U_e,U_p,U_r,U_e,U_c,U_a,U_t,U_e,U_d,U_NU}, // "Deprecated"
     StringConstant::U8S_V[] = {U_V,U_NU}, // "V"
     StringConstant::U8S_Z[] = {U_Z,U_NU}, // "Z"

     StringConstant::U8S__DO[] = {U_DO,U_NU}, // "."
     StringConstant::U8S__DO_class[] = {U_DO,U_c,U_l,U_a,U_s,U_s,U_NU}, // ".class"
     StringConstant::U8S__DO_java[] = {U_DO,U_j,U_a,U_v,U_a,U_NU}, // ".java"
     StringConstant::U8S__DO_tok[] = {U_DO,U_t,U_o,U_k,U_NU}, // ".tok"
     StringConstant::U8S__DO_u[] = {U_DO,U_u,U_NU}, // ".u"
     StringConstant::U8S__LP[] = {U_LP,U_NU}, // "("
     StringConstant::U8S__RP[] = {U_RP,U_NU}, // ")"
     StringConstant::U8S__SL[] = {U_SL,U_NU}, // "/"
     StringConstant::U8S__ST[] = {U_ST,U_NU}, // "*"

     StringConstant::U8S_class[] = {U_c,U_l,U_a,U_s,U_s,U_NU}, // "class"
     StringConstant::U8S_java[] = {U_j,U_a,U_v,U_a,U_NU}, // "java"
     StringConstant::U8S_java_SL_lang_SL_ClassNotFoundException[] = {U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_C,U_l,U_a,U_s,U_s,U_N,U_o,U_t,U_F,U_o,U_u,U_n,U_d,U_E,U_x,U_c,U_e,U_p,U_t,U_i,U_o,U_n,U_NU}, // "java/lang/ClassNotFoundException"
     StringConstant::U8S_java_SL_lang_SL_Class[] = {U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_C,U_l,U_a,U_s,U_s,U_NU}, // "java/lang/Class"
     StringConstant::U8S_java_SL_lang_SL_InternalError[] = {U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_I,U_n,U_t,U_e,U_r,U_n,U_a,U_l,U_E,U_r,U_r,U_o,U_r,U_NU}, // "java/lang/InternalError"
     StringConstant::U8S_java_SL_lang_SL_NoClassDefFoundError[] = {U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_N,U_o,U_C,U_l,U_a,U_s,U_s,U_D,U_e,U_f,U_F,U_o,U_u,U_n,U_d,U_E,U_r,U_r,U_o,U_r,U_NU}, // "java/lang/NoClassDefFoundError"
     StringConstant::U8S_java_SL_lang_SL_StringBuffer[] = {U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_S,U_t,U_r,U_i,U_n,U_g,U_B,U_u,U_f,U_f,U_e,U_r,U_NU}, // "java/lang/StringBuffer"
     StringConstant::U8S_java_SL_lang_SL_Throwable[] = {U_j,U_a,U_v,U_a,U_SL,U_l,U_a,U_n,U_g,U_SL,U_T,U_h,U_r,U_o,U_w,U_a,U_b,U_l,U_e,U_NU}, // "java/lang/Throwable"
     StringConstant::U8S_null[] = {U_n,U_u,U_l,U_l,U_NU}, // "null"
     StringConstant::U8S_quit[] = {U_q,U_u,U_i,U_t,U_NU}, // "quit"
     StringConstant::U8S_this[] = {U_t,U_h,U_i,U_s,U_NU}, // "this"

     StringConstant::U8S_LP_LB_C_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_LB, U_C, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_C_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_C, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_Z_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_Z, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_I_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_I, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_J_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_J, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_F_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_F, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_D_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_D, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_Ljava_SL_lang_SL_String_SC_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_SC, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU},
     StringConstant::U8S_LP_Ljava_SL_lang_SL_Object_SC_RP_Ljava_SL_lang_SL_StringBuffer_SC[] = {U_LP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_O, U_b, U_j, U_e, U_c, U_t, U_SC, U_RP, U_L, U_j, U_a, U_v, U_a, U_SL, U_l, U_a, U_n, U_g, U_SL, U_S, U_t, U_r, U_i, U_n, U_g, U_B, U_u, U_f, U_f, U_e, U_r, U_SC, U_NU};

char StringConstant::U8S_smallest_int[] = {U_MINUS, U_2, U_1, U_4, U_7, U_4, U_8, U_3, U_6, U_4, U_8, U_NU}; // "-2147483648"

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

//
// If the system runs out of memory, this function is invoked
//
#ifdef MICROSOFT
    int OutOfMemory(size_t)
    {
        fprintf(stderr, "***System Failure: Out of memory\n");
        exit(1);

        return 0;
    }

    void SetNewHandler()
    {
        _set_new_handler(OutOfMemory);
    }
#else
    void OutOfMemory()
    {
        fprintf(stderr, "***System Failure: Out of memory\n");
        exit(1);
    }

    void SetNewHandler()
    {
        set_new_handler(OutOfMemory);
    }
#endif

//
// When using the ICC compiler on Win95 or OS/2, we need to disable
// testing for various floating point exceptions. Default behavior
// was causing problems reading some standard class files.
//
#ifdef ICC
#include <float.h>
    void FloatingPointCheck()
    {
        _control87(EM_UNDERFLOW, EM_UNDERFLOW);
        _control87(EM_ZERODIVIDE, EM_ZERODIVIDE);
        _control87(EM_OVERFLOW, EM_OVERFLOW);
        _control87(EM_INVALID, EM_INVALID);

        return;
    }
#else
    void FloatingPointCheck() {}
#endif

#ifdef EBCDIC
//
// variants of system functions requiring EBCDIC translation
// are declared here and defined in code.cpp
//
    int SystemStat(const char *name, struct stat *stat_struct) { /* TODO: */ return 0; }
    FILE *SystemFopen(char *name, char *mode) { /* TODO: */ return NULL; }
    size_t SystemFread(char *ptr, size_t element_size, size_t count, FILE *stream,int ascii_option) { /* TODO: */ return 0; }
    int SystemIsDirectory(char *name) { /* TODO: */ return 0; }
#else
    int SystemStat(const char *name, struct stat *stat_struct)
    {
        return stat(name, stat_struct);
    }
    FILE *SystemFopen(char *name, char *mode)
    {
        return fopen(name, mode);
    }
    size_t SystemFread(char *ptr, size_t element_size, size_t count, FILE *stream, int ascii_option)
    {
        return fread(ptr, element_size, count, stream);
    }
    int SystemIsDirectory(char *name)
    {
        struct stat status;
        return (((::SystemStat(name, &status) == 0) && (status.st_mode & STAT_S_IFDIR)) ? 1 : 0);
    }
#endif


#if defined(GNU_LIBC5)
#include <sys/stat.h>
#ifndef UNIX
    int SystemMkdir(char *dirname)
    {
        return mkdir(dirname, S_IRWXU);
    }
#endif
    size_t wcslen(wchar_t *cs)
    {
        int n = 0;
        while (*cs++)
            n++;

        return n;
    }

    wchar_t *wcscpy(wchar_t *s, wchar_t *ct)
    {
        wchar_t *ptr;
        for (ptr = s; *ct; ptr++, ct++)
            *ptr = *ct;
        *ptr = U_NULL;

        return s;
    }

    wchar_t *wcsncpy(wchar_t *s, wchar_t *ct, int n)
    {
        wchar_t *ptr;
        for (ptr = s; *ct && n-- > 0; ptr++, ct++)
            *ptr = *ct;
        while (n-- > 0)
            *ptr++ = U_NULL;

        return s;
    }

    wchar_t *wcscat(wchar_t *s, wchar_t *ct)
    {
        wchar_t *ptr = s;

        while (*ptr)
            ptr++;
        wcscpy(ptr, ct);

        return s;
    }

    int wcscmp(wchar_t *cs, wchar_t *ct)
    {
        while (*cs == *ct && *cs && *ct)
        {
            cs++;
            ct++;
        }

        return (*cs == *ct ? 0 : (*cs < *ct ? -1 : 1));
    }

    int wcsncmp(wchar_t *cs, wchar_t *ct, int n)
    {
        while (*cs == *ct && *cs && *ct && n-- > 0)
        {
            cs++;
            ct++;
        }

        return (n <= 0 || *cs == *ct ? 0 : (*cs < *ct ? -1 : 1));
    }
#elif WIN32_FILE_SYSTEM
#include <direct.h>
    int SystemMkdir(char *dirname)
    {
        return mkdir(dirname);
    }
#endif

#ifdef __OS2__
// changes for OS/2 from John Price, jgprice@ozemail.com.au, 2/99
#include <direct.h>
    int SystemMkdir(char *dirname)
    {
        return mkdir(dirname);
    }
#endif

#ifdef UNIX
    char PathSeparator() { return U_COLON; } // ":"

    int SystemMkdir(char *dirname)
    {
#ifdef EBCDIC
        // must translate dirname to EBCDIC before mkdir call
        int n = strlen(dirname) + 1;
        char *ebcdic_name = new char[n];

        for (int i = 0; i <= n; i++)
             ebcdic_name[i] = dirname[i];
        int rc = mkdir(ebcdic_name, S_IRWXU | S_IRWXG | S_IRWXO);
        delete[] ebcdic_name;
        return rc;
#else
        return mkdir(dirname, S_IRWXU | S_IRWXG | S_IRWXO);
#endif
    }
#else
    char PathSeparator() { return U_SEMICOLON; } // ";"
#endif

Ostream Coutput;
