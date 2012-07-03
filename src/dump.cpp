// $Id: dump.cpp,v 1.7 1999/10/09 16:34:07 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifdef TEST

#include "config.h"
#include "javasym.h"
#include "control.h"
#include "stream.h"
#include <iostream.h>
#include <stdio.h>

static char*
      TK_notoken_STRING      = "TK_notoken",
      TK_Identifier_STRING[] = "TK_Identifier",
      TK_abstract_STRING[] = "TK_abstract",
      TK_boolean_STRING[] = "TK_boolean",
      TK_break_STRING[] = "TK_break",
      TK_byte_STRING[] = "TK_byte",
      TK_case_STRING[] = "TK_case",
      TK_catch_STRING[] = "TK_catch",
      TK_char_STRING[] = "TK_char",
      TK_class_STRING[] = "TK_class",
      TK_const_STRING[] = "TK_const",
      TK_continue_STRING[] = "TK_continue",
      TK_default_STRING[] = "TK_default",
      TK_do_STRING[] = "TK_do",
      TK_double_STRING[] = "TK_double",
      TK_else_STRING[] = "TK_else",
      TK_extends_STRING[] = "TK_extends",
      TK_false_STRING[] = "TK_false",
      TK_final_STRING[] = "TK_final",
      TK_finally_STRING[] = "TK_finally",
      TK_float_STRING[] = "TK_float",
      TK_for_STRING[] = "TK_for",
      TK_goto_STRING[] = "TK_goto",
      TK_if_STRING[] = "TK_if",
      TK_implements_STRING[] = "TK_implements",
      TK_import_STRING[] = "TK_import",
      TK_instanceof_STRING[] = "TK_instanceof",
      TK_int_STRING[] = "TK_int",
      TK_interface_STRING[] = "TK_interface",
      TK_long_STRING[] = "TK_long",
      TK_native_STRING[] = "TK_native",
      TK_new_STRING[] = "TK_new",
      TK_null_STRING[] = "TK_null",
      TK_package_STRING[] = "TK_package",
      TK_private_STRING[] = "TK_private",
      TK_protected_STRING[] = "TK_protected",
      TK_public_STRING[] = "TK_public",
      TK_return_STRING[] = "TK_return",
      TK_short_STRING[] = "TK_short",
      TK_static_STRING[] = "TK_static",
      TK_super_STRING[] = "TK_super",
      TK_switch_STRING[] = "TK_switch",
      TK_synchronized_STRING[] = "TK_synchronized",
      TK_this_STRING[] = "TK_this",
      TK_throw_STRING[] = "TK_throw",
      TK_throws_STRING[] = "TK_throws",
      TK_transient_STRING[] = "TK_transient",
      TK_true_STRING[] = "TK_true",
      TK_try_STRING[] = "TK_try",
      TK_void_STRING[] = "TK_void",
      TK_volatile_STRING[] = "TK_volatile",
      TK_while_STRING[] = "TK_while",
      TK_IntegerLiteral_STRING[] = "TK_IntegerLiteral",
      TK_LongLiteral_STRING[] = "TK_LongLiteral",
      TK_FloatingPointLiteral_STRING[] = "TK_FloatingPointLiteral",
      TK_DoubleLiteral_STRING[] = "TK_DoubleLiteral",
      TK_CharacterLiteral_STRING[] = "TK_CharacterLiteral",
      TK_StringLiteral_STRING[] = "TK_StringLiteral",
      TK_PLUS_PLUS_STRING[] = "TK_PLUS_PLUS",
      TK_MINUS_MINUS_STRING[] = "TK_MINUS_MINUS",
      TK_EQUAL_EQUAL_STRING[] = "TK_EQUAL_EQUAL",
      TK_LESS_EQUAL_STRING[] = "TK_LESS_EQUAL",
      TK_GREATER_EQUAL_STRING[] = "TK_GREATER_EQUAL",
      TK_NOT_EQUAL_STRING[] = "TK_NOT_EQUAL",
      TK_LEFT_SHIFT_STRING[] = "TK_LEFT_SHIFT",
      TK_RIGHT_SHIFT_STRING[] = "TK_RIGHT_SHIFT",
      TK_UNSIGNED_RIGHT_SHIFT_STRING[] = "TK_UNSIGNED_RIGHT_SHIFT",
      TK_PLUS_EQUAL_STRING[] = "TK_PLUS_EQUAL",
      TK_MINUS_EQUAL_STRING[] = "TK_MINUS_EQUAL",
      TK_MULTIPLY_EQUAL_STRING[] = "TK_MULTIPLY_EQUAL",
      TK_DIVIDE_EQUAL_STRING[] = "TK_DIVIDE_EQUAL",
      TK_AND_EQUAL_STRING[] = "TK_AND_EQUAL",
      TK_OR_EQUAL_STRING[] = "TK_OR_EQUAL",
      TK_XOR_EQUAL_STRING[] = "TK_XOR_EQUAL",
      TK_REMAINDER_EQUAL_STRING[] = "TK_REMAINDER_EQUAL",
      TK_LEFT_SHIFT_EQUAL_STRING[] = "TK_LEFT_SHIFT_EQUAL",
      TK_RIGHT_SHIFT_EQUAL_STRING[] = "TK_RIGHT_SHIFT_EQUAL",
      TK_UNSIGNED_RIGHT_SHIFT_EQUAL_STRING[] = "TK_UNSIGNED_RIGHT_SHIFT_EQUAL",
      TK_OR_OR_STRING[] = "TK_OR_OR",
      TK_AND_AND_STRING[] = "TK_AND_AND",
      TK_PLUS_STRING[] = "TK_PLUS",
      TK_MINUS_STRING[] = "TK_MINUS",
      TK_NOT_STRING[] = "TK_NOT",
      TK_REMAINDER_STRING[] = "TK_REMAINDER",
      TK_XOR_STRING[] = "TK_XOR",
      TK_AND_STRING[] = "TK_AND",
      TK_MULTIPLY_STRING[] = "TK_MULTIPLY",
      TK_OR_STRING[] = "TK_OR",
      TK_TWIDDLE_STRING[] = "TK_TWIDDLE",
      TK_DIVIDE_STRING[] = "TK_DIVIDE",
      TK_GREATER_STRING[] = "TK_GREATER",
      TK_LESS_STRING[] = "TK_LESS",
      TK_LPAREN_STRING[] = "TK_LPAREN",
      TK_RPAREN_STRING[] = "TK_RPAREN",
      TK_LBRACE_STRING[] = "TK_LBRACE",
      TK_RBRACE_STRING[] = "TK_RBRACE",
      TK_LBRACKET_STRING[] = "TK_LBRACKET",
      TK_RBRACKET_STRING[] = "TK_RBRACKET",
      TK_SEMICOLON_STRING[] = "TK_SEMICOLON",
      TK_QUESTION_STRING[] = "TK_QUESTION",
      TK_COLON_STRING[] = "TK_COLON",
      TK_COMMA_STRING[] = "TK_COMMA",
      TK_DOT_STRING[] = "TK_DOT",
      TK_EQUAL_STRING[] = "TK_EQUAL",
      TK_ERROR_STRING[] = "TK_ERROR",
      TK_EOF_STRING[] = "TK_EOF",
      TK_EOL_STRING[] = "TK_EOL";

static char *token_type(unsigned char kind)
{
    switch(kind)
    {
    case TK_Identifier: return TK_Identifier_STRING;
    case TK_abstract: return TK_abstract_STRING;
    case TK_boolean: return TK_boolean_STRING;
    case TK_break: return TK_break_STRING;
    case TK_byte: return TK_byte_STRING;
    case TK_case: return TK_case_STRING;
    case TK_catch: return TK_catch_STRING;
    case TK_char: return TK_char_STRING;
    case TK_class: return TK_class_STRING;
    case TK_const: return TK_const_STRING;
    case TK_continue: return TK_continue_STRING;
    case TK_default: return TK_default_STRING;
    case TK_do: return TK_do_STRING;
    case TK_double: return TK_double_STRING;
    case TK_else: return TK_else_STRING;
    case TK_extends: return TK_extends_STRING;
    case TK_false: return TK_false_STRING;
    case TK_final: return TK_final_STRING;
    case TK_finally: return TK_finally_STRING;
    case TK_float: return TK_float_STRING;
    case TK_for: return TK_for_STRING;
    case TK_goto: return TK_goto_STRING;
    case TK_if: return TK_if_STRING;
    case TK_implements: return TK_implements_STRING;
    case TK_import: return TK_import_STRING;
    case TK_instanceof: return TK_instanceof_STRING;
    case TK_int: return TK_int_STRING;
    case TK_interface: return TK_interface_STRING;
    case TK_long: return TK_long_STRING;
    case TK_native: return TK_native_STRING;
    case TK_new: return TK_new_STRING;
    case TK_null: return TK_null_STRING;
    case TK_package: return TK_package_STRING;
    case TK_private: return TK_private_STRING;
    case TK_protected: return TK_protected_STRING;
    case TK_public: return TK_public_STRING;
    case TK_return: return TK_return_STRING;
    case TK_short: return TK_short_STRING;
    case TK_static: return TK_static_STRING;
    case TK_super: return TK_super_STRING;
    case TK_switch: return TK_switch_STRING;
    case TK_synchronized: return TK_synchronized_STRING;
    case TK_this: return TK_this_STRING;
    case TK_throw: return TK_throw_STRING;
    case TK_throws: return TK_throws_STRING;
    case TK_transient: return TK_transient_STRING;
    case TK_true: return TK_true_STRING;
    case TK_try: return TK_try_STRING;
    case TK_void: return TK_void_STRING;
    case TK_volatile: return TK_volatile_STRING;
    case TK_while: return TK_while_STRING;
    case TK_IntegerLiteral: return TK_IntegerLiteral_STRING;
    case TK_LongLiteral: return TK_LongLiteral_STRING;
    case TK_FloatingPointLiteral: return TK_FloatingPointLiteral_STRING;
    case TK_DoubleLiteral: return TK_DoubleLiteral_STRING;
    case TK_CharacterLiteral: return TK_CharacterLiteral_STRING;
    case TK_StringLiteral: return TK_StringLiteral_STRING;
    case TK_PLUS_PLUS: return TK_PLUS_PLUS_STRING;
    case TK_MINUS_MINUS: return TK_MINUS_MINUS_STRING;
    case TK_EQUAL_EQUAL: return TK_EQUAL_EQUAL_STRING;
    case TK_LESS_EQUAL: return TK_LESS_EQUAL_STRING;
    case TK_GREATER_EQUAL: return TK_GREATER_EQUAL_STRING;
    case TK_NOT_EQUAL: return TK_NOT_EQUAL_STRING;
    case TK_LEFT_SHIFT: return TK_LEFT_SHIFT_STRING;
    case TK_RIGHT_SHIFT: return TK_RIGHT_SHIFT_STRING;
    case TK_UNSIGNED_RIGHT_SHIFT: return TK_UNSIGNED_RIGHT_SHIFT_STRING;
    case TK_PLUS_EQUAL: return TK_PLUS_EQUAL_STRING;
    case TK_MINUS_EQUAL: return TK_MINUS_EQUAL_STRING;
    case TK_MULTIPLY_EQUAL: return TK_MULTIPLY_EQUAL_STRING;
    case TK_DIVIDE_EQUAL: return TK_DIVIDE_EQUAL_STRING;
    case TK_AND_EQUAL: return TK_AND_EQUAL_STRING;
    case TK_OR_EQUAL: return TK_OR_EQUAL_STRING;
    case TK_XOR_EQUAL: return TK_XOR_EQUAL_STRING;
    case TK_REMAINDER_EQUAL: return TK_REMAINDER_EQUAL_STRING;
    case TK_LEFT_SHIFT_EQUAL: return TK_LEFT_SHIFT_EQUAL_STRING;
    case TK_RIGHT_SHIFT_EQUAL: return TK_RIGHT_SHIFT_EQUAL_STRING;
    case TK_UNSIGNED_RIGHT_SHIFT_EQUAL: return TK_UNSIGNED_RIGHT_SHIFT_EQUAL_STRING;
    case TK_OR_OR: return TK_OR_OR_STRING;
    case TK_AND_AND: return TK_AND_AND_STRING;
    case TK_PLUS: return TK_PLUS_STRING;
    case TK_MINUS: return TK_MINUS_STRING;
    case TK_NOT: return TK_NOT_STRING;
    case TK_REMAINDER: return TK_REMAINDER_STRING;
    case TK_XOR: return TK_XOR_STRING;
    case TK_AND: return TK_AND_STRING;
    case TK_MULTIPLY: return TK_MULTIPLY_STRING;
    case TK_OR: return TK_OR_STRING;
    case TK_TWIDDLE: return TK_TWIDDLE_STRING;
    case TK_DIVIDE: return TK_DIVIDE_STRING;
    case TK_GREATER: return TK_GREATER_STRING;
    case TK_LESS: return TK_LESS_STRING;
    case TK_LPAREN: return TK_LPAREN_STRING;
    case TK_RPAREN: return TK_RPAREN_STRING;
    case TK_LBRACE: return TK_LBRACE_STRING;
    case TK_RBRACE: return TK_RBRACE_STRING;
    case TK_LBRACKET: return TK_LBRACKET_STRING;
    case TK_RBRACKET: return TK_RBRACKET_STRING;
    case TK_SEMICOLON: return TK_SEMICOLON_STRING;
    case TK_QUESTION: return TK_QUESTION_STRING;
    case TK_COLON: return TK_COLON_STRING;
    case TK_COMMA: return TK_COMMA_STRING;
    case TK_DOT: return TK_DOT_STRING;
    case TK_EQUAL: return TK_EQUAL_STRING;
    case TK_ERROR: return TK_ERROR_STRING;
    case TK_EOF: return TK_EOF_STRING;
    case TK_EOL: return TK_EOL_STRING;
    default:            return TK_notoken_STRING;
    }
}

void LexStream::Dump()
{
    FILE *tokfile;
    char *tokfile_name = new char[FileNameLength() + 5]; // +1 for '\0' +4 for length(".tok")
    strcpy(tokfile_name, FileName());
    strcat(tokfile_name, StringConstant::U8S__DO_tok);

    if ((tokfile = ::SystemFopen(tokfile_name, "w")) == NULL)
    {
        Coutput << "*** Cannot open file " << tokfile_name << "\n";
        return;
    }

    RereadInput();

    SetUpComments();

    LexStream::TokenIndex tok = 0;
    for (LexStream::CommentIndex com = FirstComment(tok); com < NumComments() && PrecedingToken(com) == tok; com++)
    {
        fprintf(tokfile, "*%5d ", com);
        // print file name
        fprintf(tokfile, "%s",FileName());
        fprintf(tokfile, ", line %d.%d: ",
                         FindLine(comments[com].location),
                         FindColumn(comments[com].location));
        for (wchar_t *s = CommentString(com); *s != U_NULL; s++)
            fprintf(tokfile, "%c", *s);
        fprintf(tokfile, "\n");
    }

    do
    {
        tok = Gettoken();

        fprintf(tokfile, "%6d ", tok);
        fprintf(tokfile, " %s",FileName());
        fprintf(tokfile, ", %cline %d.%d: %s %s  ",
                         (AfterEol(tok) ? '*' : ' '),
                         Line(tok),
                         Column(tok),
                         token_type(Kind(tok)),
                         (IsDeprecated(Previous(tok)) ? "(d)" : " "));
        for (wchar_t *s = NameString(tok); *s != U_NULL; s++)
            fprintf(tokfile, "%c", *s);
        fprintf(tokfile, "\n");

        for (LexStream::CommentIndex com = FirstComment(tok); com < NumComments() && PrecedingToken(com) == tok; com++)
        {
            fprintf(tokfile, "*%5d ",com);
        fprintf(tokfile, " %s",FileName());
            fprintf(tokfile, ", line %d.%d: ",
                             FindLine(comments[com].location),
                             FindColumn(comments[com].location));
            for (wchar_t *s = CommentString(com); *s != U_NULL; s++)
            fprintf(tokfile, "%c", *s);
            fprintf(tokfile, "\n");
        }
    } while (Kind(tok) != TK_EOF);

    DestroyInput();
    fprintf(tokfile, "\n");
#ifdef UNIQUE_NAMES
    fprintf(tokfile, "\nThe unique names are:\n\n");
    for (int i = 0; i < control.name_table.symbol_pool.length(); i++)
    {
        fprintf(tokfile, "%4d ", i);
        for (wchar_t *s = control.name_table.symbol_pool[i].name(); *s != U_NULL; s++)
            fprintf(tokfile, "%c", *s);
        fprintf(tokfile, "\n");
    }
#endif

    if (tokfile)
        fclose(tokfile);

    delete [] tokfile_name;

    return;
}

#endif
