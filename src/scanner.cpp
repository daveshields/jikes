// $Id: scanner.cpp,v 1.11 2000/01/06 08:24:30 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include "scanner.h"
#include "control.h"
#include "error.h"

int (*Scanner::scan_keyword[13]) (wchar_t *p1) =
{
    ScanKeyword0,
    ScanKeyword0,
    ScanKeyword2,
    ScanKeyword3,
    ScanKeyword4,
    ScanKeyword5,
    ScanKeyword6,
    ScanKeyword7,
    ScanKeyword8,
    ScanKeyword9,
    ScanKeyword10,
    ScanKeyword0,
    ScanKeyword12
};


//
// The constructor initializes all utility variables.
//
Scanner::Scanner(Control &control_) : control(control_)
{
    //
    // If this assertion fails, the Token structure in stream.h must be redesigned !!!
    //
    assert(NUM_TERMINALS < 128);

    //
    // -------------------------------------------------------------------------------
    // We are pulling this code out because we are tired of defending it. We
    // tought it was obvious that either $ should not have been used for compiler
    // generated variables or that users should not be allowed to use in variable names...
    // -------------------------------------------------------------------------------
    //
    // For version 1.1 or above a $ may not be used as part of an identifier name
    // unless the user specifically requests that it be allowed.
    //
    //    if (! control.option.dollar)
    //        Code::SetBadCode(U_DOLLAR);
    //

    //
    // CLASSIFY_TOKEN is a mapping from each character into a
    // classification routine that is invoked when that character
    // is the first character encountered in a token.
    //
    for (int c = 0; c < 128; c++)
    {
        if (Code::IsAlpha(c))
             classify_token[c] = &Scanner::ClassifyId;
        else if (Code::IsDigit(c))
             classify_token[c] = &Scanner::ClassifyNumericLiteral;
        else classify_token[c] = &Scanner::ClassifyBadToken;
    }
    classify_token[128] = &Scanner::ClassifyNonAsciiUnicode;

    classify_token[U_a] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_b] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_c] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_d] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_e] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_f] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_g] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_i] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_l] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_n] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_p] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_r] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_s] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_t] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_v] = &Scanner::ClassifyIdOrKeyword;
    classify_token[U_w] = &Scanner::ClassifyIdOrKeyword;

    classify_token[U_SINGLE_QUOTE]       = &Scanner::ClassifyCharLiteral;
    classify_token[U_DOUBLE_QUOTE]       = &Scanner::ClassifyStringLiteral;

    classify_token[U_PLUS]               = &Scanner::ClassifyPlus;
    classify_token[U_MINUS]              = &Scanner::ClassifyMinus;
    classify_token[U_EXCLAMATION]        = &Scanner::ClassifyNot;
    classify_token[U_PERCENT]            = &Scanner::ClassifyMod;
    classify_token[U_CARET]              = &Scanner::ClassifyXor;
    classify_token[U_AMPERSAND]          = &Scanner::ClassifyAnd;
    classify_token[U_STAR]               = &Scanner::ClassifyStar;
    classify_token[U_BAR]                = &Scanner::ClassifyOr;
    classify_token[U_TILDE]              = &Scanner::ClassifyComplement;
    classify_token[U_SLASH]              = &Scanner::ClassifySlash;
    classify_token[U_GREATER]            = &Scanner::ClassifyGreater;
    classify_token[U_LESS]               = &Scanner::ClassifyLess;
    classify_token[U_LEFT_PARENTHESIS]   = &Scanner::ClassifyLparen;
    classify_token[U_RIGHT_PARENTHESIS]  = &Scanner::ClassifyRparen;
    classify_token[U_LEFT_BRACE]         = &Scanner::ClassifyLbrace;
    classify_token[U_RIGHT_BRACE]        = &Scanner::ClassifyRbrace;
    classify_token[U_LEFT_BRACKET]       = &Scanner::ClassifyLbracket;
    classify_token[U_RIGHT_BRACKET]      = &Scanner::ClassifyRbracket;
    classify_token[U_SEMICOLON]          = &Scanner::ClassifySemicolon;
    classify_token[U_QUESTION]           = &Scanner::ClassifyQuestion;
    classify_token[U_COLON]              = &Scanner::ClassifyColon;
    classify_token[U_COMMA]              = &Scanner::ClassifyComma;
    classify_token[U_DOT]                = &Scanner::ClassifyPeriod;
    classify_token[U_EQUAL]              = &Scanner::ClassifyEqual;

    return;
}


//
// Associate a lexical stream with this file
//
void Scanner::Initialize(FileSymbol *file_symbol)
{
    lex = new LexStream(control, file_symbol);
    lex -> Reset();

    current_token_index = lex -> GetNextToken(0); // Get 0th token !
    current_token = &(lex -> token_stream[current_token_index]);
    current_token -> SetKind(0);

    if (control.option.comments)
    {
        LexStream::Comment *current_comment = &(lex -> comment_stream.Next()); // add 0th comment !
        current_comment -> string = NULL;
        current_comment -> length = 0;
        current_comment -> previous_token = -1; // No token precedes this comment
        current_comment -> location = 0;
    }

    lex -> line_location.Next() = 0; // mark starting location of line # 0

    return;
}


//
// This is one of the main entry point for the Java lexical analyser.
// Its input is the name of a regular text file. Its output is a stream
// of tokens.
//
void Scanner::SetUp(FileSymbol *file_symbol)
{
    Initialize(file_symbol);
    lex -> CompressSpace();
    file_symbol -> lex_stream = lex;

    return;
}


//
// This is one of the main entry point for the Java lexical analyser.
// Its input is the name of a regular text file. Its output is a stream
// of tokens.
//
void Scanner::Scan(FileSymbol *file_symbol)
{
    Initialize(file_symbol);

    lex -> ReadInput();

    cursor = lex -> InputBuffer();
    if (cursor)
    {
        Scan();

        lex -> CompressSpace();

        //
        //
        //
        if (control.option.dump_errors)
        {
            lex -> SortMessages();
            for (int i = 0; i < lex -> bad_tokens.Length(); i++)
                lex -> PrintEmacsMessage(i);
            Coutput.flush();
        }
        lex -> DestroyInput(); // get rid of input buffer
    }
    else
    {
        delete lex;
        lex = NULL;
    }

    file_symbol -> lex_stream = lex;

    return;
}


//
// Scan the InputBuffer() and process all tokens and comments.
//
void Scanner::Scan()
{
    wchar_t *input_buffer_tail = &cursor[lex -> InputBufferLength()];

    //
    // CURSOR is assumed to point to the next character to be scanned.
    // Using CURSOR,we jump to the proper classification function
    // which scans and classifies the token and returns the location of
    // the character immediately following it.
    //
    do
    {
        SkipSpaces();

        //
        // Allocate space for next token and set its location.
        //
        current_token_index = lex -> GetNextToken(cursor - lex -> InputBuffer());
        current_token = &(lex -> token_stream[current_token_index]);

        (this ->* classify_token[*cursor < 128 ? *cursor : 128])();
    } while (cursor < input_buffer_tail);

    //
    // Add a a gate after the last line.
    //
    lex -> line_location.Next() = input_buffer_tail - lex -> InputBuffer();

    //
    // If the brace_stack is not empty, then there are unmatched left
    // braces in the input. Each unmatched left brace should point to
    // the EOF token as a substitute for a matching right brace.
    //
    assert(current_token_index == lex -> token_stream.Length() - 1);

    for (LexStream::TokenIndex left_brace = brace_stack.Top(); left_brace; left_brace = brace_stack.Top())
    {
        lex -> token_stream[left_brace].SetRightBrace(current_token_index);
        brace_stack.Pop();
    }

    return;
}


//
// CURSOR points to the starting position of a comment.  Scan the
// the comment and return the location of the character immediately
// following it. CURSOR is advanced accordingly.
//
void Scanner::ScanStarComment()
{
    LexStream::Comment *current_comment = (control.option.comments ? &(lex -> comment_stream.Next()) : new LexStream::Comment());
    current_comment -> string = NULL;
    current_comment -> previous_token = current_token_index; // the token that precedes this comment
    current_comment -> location = cursor - lex -> InputBuffer();

    cursor += 2;

    //
    // If this comment starts with the prefix "/**" then, it may be a document
    // comment. Check whether or not it contains the deprecated tag and if so,
    // mark the token preceeding it.
    //
    if (*cursor == U_STAR)
    {
        for (;;)
        {
            while (*cursor != U_STAR && (! Code::IsNewline(*cursor)) && *cursor != U_CTL_Z)
            {
                if (cursor[0] == U_AT &&
                    cursor[1] == U_d &&
                    cursor[2] == U_e &&
                    cursor[3] == U_p &&
                    cursor[4] == U_r &&
                    cursor[5] == U_e &&
                    cursor[6] == U_c &&
                    cursor[7] == U_a &&
                    cursor[8] == U_t &&
                    cursor[9] == U_e &&
                    cursor[10] == U_d)
                {
                    current_token -> SetDeprecated(); // the token that precedes this comment
                }
                cursor++;
            }

            if (*cursor == U_STAR) // Potential comment closer
            {
                while (*++cursor == U_STAR)
                    ;
                if (*cursor == U_SLASH)
                {
                    cursor++;
                    current_comment -> length = (cursor - lex -> InputBuffer()) - current_comment -> location;
                    if (! control.option.comments)
                        delete current_comment;
                    return;
                }
            }
            else if (Code::IsNewline(*cursor)) // Record new line
            {
                cursor++;
                lex -> line_location.Next() = cursor - lex -> InputBuffer();
            }
            else break;
        }
    }
    else
    {
        for (;;)
        {
            while (*cursor != U_STAR && (! Code::IsNewline(*cursor)) && *cursor != U_CTL_Z)
                cursor++;

            if (*cursor == U_STAR) // Potential comment closer
            {
                while (*++cursor == U_STAR)
                    ;
                if (*cursor == U_SLASH)
                {
                    cursor++;
                    current_comment -> length = (cursor - lex -> InputBuffer()) - current_comment -> location;
                    if (! control.option.comments)
                        delete current_comment;
                    return;
                }
            }
            else if (Code::IsNewline(*cursor)) // Record new line
            {
                cursor++;
                lex -> line_location.Next() = cursor - lex -> InputBuffer();
            }
            else break;
        }
    }

    lex -> bad_tokens.Next().Initialize(StreamError::UNTERMINATED_COMMENT,
                                        current_comment -> location,
                                        (unsigned) (cursor - lex -> InputBuffer()) - 1);

    current_comment -> length = (cursor - lex -> InputBuffer()) - current_comment -> location;

    if (! control.option.comments)
        delete current_comment;

    return;
}


//
//
//
void Scanner::ScanSlashComment()
{
    if (control.option.comments)
    {
        LexStream::Comment *current_comment = &(lex -> comment_stream.Next());
        current_comment -> string = NULL;
        current_comment -> previous_token = current_token_index;  // the token that precedes this comment
        current_comment -> location = cursor - lex -> InputBuffer();
        for (cursor += 2; ! Code::IsNewline(*cursor); cursor++)  // skip all until \n
            ;
        current_comment -> length = (cursor - lex -> InputBuffer()) - current_comment -> location;
    }
    else
    {
        for (cursor += 2; ! Code::IsNewline(*cursor); cursor++)  // skip all until \n
            ;
    }

    return;
}


//
// This procedure is invoked to skip useless spaces in the input.
// It assumes upon entry that CURSOR points to the next character to
// be scanned.  Before returning it sets CURSOR to the location of the
// first non-space character following its initial position.
//
inline void Scanner::SkipSpaces()
{
    do
    {
        while (Code::IsSpaceButNotNewline(*cursor))
            cursor++;
        while (Code::IsNewline(*cursor))        // starting a new line?
        {
            cursor++;
            lex -> line_location.Next() = cursor - lex -> InputBuffer();
            while (Code::IsSpaceButNotNewline(*cursor))
                cursor++;
        }

        while (*cursor == U_SLASH)
        {
            if (cursor[1] == U_STAR)
                 ScanStarComment();
            else if (cursor[1] == U_SLASH)
                 ScanSlashComment();
            else break;
        }
    } while (Code::IsSpace(*cursor));

    return;
}


/**********************************************************************/
/**********************************************************************/
/**                                                                  **/
/**                           scan_keyword(i):                       **/
/**                                                                  **/
/**********************************************************************/
/**********************************************************************/
/**                                                                  **/
/** Scan an identifier of length I and determine if it is a keyword. **/
/**                                                                  **/
/**********************************************************************/
/**********************************************************************/
int Scanner::ScanKeyword0(wchar_t *p1)
{
    return TK_Identifier;
}

int Scanner::ScanKeyword2(wchar_t *p1)
{
    if (p1[0] == U_d && p1[1] == U_o)
        return TK_do;
    else if (p1[0] == U_i && p1[1] == U_f)
        return TK_if;

    return TK_Identifier;
}

int Scanner::ScanKeyword3(wchar_t *p1)
{
    switch(*p1)
    {
        case U_f:
            if (p1[1] == U_o && p1[2] == U_r)
                return TK_for;
            break;
        case U_i:
            if (p1[1] == U_n && p1[2] == U_t)
                return TK_int;
            break;
        case U_n:
            if (p1[1] == U_e && p1[2] == U_w)
                return TK_new;
            break;
        case U_t:
            if (p1[1] == U_r && p1[2] == U_y)
                return TK_try;
            break;
    }

    return TK_Identifier;
}

int Scanner::ScanKeyword4(wchar_t *p1)
{
    switch (*p1)
    {
        case U_b:
            if (p1[1] == U_y && p1[2] == U_t && p1[3] == U_e)
                return TK_byte;
            break;
        case U_c:
            if (p1[1] == U_a && p1[2] == U_s && p1[3] == U_e)
                return TK_case;
            else if (p1[1] == U_h && p1[2] == U_a && p1[3] == U_r)
                return TK_char;
            break;
        case U_e:
            if (p1[1] == U_l && p1[2] == U_s && p1[3] == U_e)
                return TK_else;
            break;
        case U_g:
            if (p1[1] == U_o && p1[2] == U_t && p1[3] == U_o)
                return TK_goto;
            break;
        case U_l:
            if (p1[1] == U_o && p1[2] == U_n && p1[3] == U_g)
                return TK_long;
            break;
        case U_n:
            if (p1[1] == U_u && p1[2] == U_l && p1[3] == U_l)
                return TK_null;
            break;
        case U_t:
            if (p1[1] == U_h && p1[2] == U_i && p1[3] == U_s)
                return TK_this;
            else if (p1[1] == U_r && p1[2] == U_u && p1[3] == U_e)
                return TK_true;
            break;
        case U_v:
            if (p1[1] == U_o && p1[2] == U_i && p1[3] == U_d)
                return TK_void;
            break;
    }

    return TK_Identifier;
}

int Scanner::ScanKeyword5(wchar_t *p1)
{
    switch (*p1)
    {
        case U_b:
            if (p1[1] == U_r && p1[2] == U_e &&
                p1[3] == U_a && p1[4] == U_k)
                return TK_break;
            break;
        case U_c:
            if (p1[1] == U_a && p1[2] == U_t &&
                p1[3] == U_c && p1[4] == U_h)
                return TK_catch;
            else if (p1[1] == U_l && p1[2] == U_a &&
                     p1[3] == U_s && p1[4] == U_s)
                return TK_class;
            else if (p1[1] == U_o && p1[2] == U_n &&
                     p1[3] == U_s && p1[4] == U_t)
                return TK_const;
            break;
        case U_f:
            if (p1[1] == U_a && p1[2] == U_l &&
                p1[3] == U_s && p1[4] == U_e)
                return TK_false;
            else if (p1[1] == U_i && p1[2] == U_n &&
                     p1[3] == U_a && p1[4] == U_l)
                return TK_final;
            else if (p1[1] == U_l && p1[2] == U_o &&
                     p1[3] == U_a && p1[4] == U_t)
                return TK_float;
            break;
        case U_s:
            if (p1[1] == U_h && p1[2] == U_o &&
                p1[3] == U_r && p1[4] == U_t)
                return TK_short;
            else if (p1[1] == U_u && p1[2] == U_p &&
                     p1[3] == U_e && p1[4] == U_r)
                return TK_super;
            break;
        case U_t:
            if (p1[1] == U_h && p1[2] == U_r &&
                p1[3] == U_o && p1[4] == U_w)
                return TK_throw;
            break;
        case U_w:
            if (p1[1] == U_h && p1[2] == U_i &&
                p1[3] == U_l && p1[4] == U_e)
                return TK_while;
            break;
    }

    return TK_Identifier;
}

int Scanner::ScanKeyword6(wchar_t *p1)
{
    switch (*p1)
    {
        case U_d:
            if (p1[1] == U_o && p1[2] == U_u &&
                     p1[3] == U_b && p1[4] == U_l && p1[5] == U_e)
                return TK_double;
            break;
        case U_i:
            if (p1[1] == U_m && p1[2] == U_p &&
                p1[3] == U_o && p1[4] == U_r && p1[5] == U_t)
                return TK_import;
            break;
        case U_n:
            if (p1[1] == U_a && p1[2] == U_t &&
                p1[3] == U_i && p1[4] == U_v && p1[5] == U_e)
                return TK_native;
            break;
        case U_p:
            if (p1[1] == U_u && p1[2] == U_b &&
                p1[3] == U_l && p1[4] == U_i && p1[5] == U_c)
                return TK_public;
            break;
        case U_r:
            if (p1[1] == U_e && p1[2] == U_t &&
                p1[3] == U_u && p1[4] == U_r && p1[5] == U_n)
                return TK_return;
            break;
        case U_s:
            if (p1[1] == U_t && p1[2] == U_a &&
                p1[3] == U_t && p1[4] == U_i && p1[5] == U_c)
                    return TK_static;
            else if (p1[1] == U_w && p1[2] == U_i &&
                     p1[3] == U_t && p1[4] == U_c && p1[5] == U_h)
                return TK_switch;
            break;
        case U_t:
            if (p1[1] == U_h && p1[2] == U_r &&
                p1[3] == U_o && p1[4] == U_w && p1[5] == U_s)
                return TK_throws;
            break;
    }

    return TK_Identifier;
}

int Scanner::ScanKeyword7(wchar_t *p1)
{
    switch(*p1)
    {
        case U_b:
            if (p1[1] == U_o && p1[2] == U_o && p1[3] == U_l &&
                p1[4] == U_e && p1[5] == U_a && p1[6] == U_n)
                return TK_boolean;
        case U_d:
            if (p1[1] == U_e && p1[2] == U_f && p1[3] == U_a &&
                p1[4] == U_u && p1[5] == U_l && p1[6] == U_t)
                return TK_default;
            break;
        case U_e:
            if (p1[1] == U_x && p1[2] == U_t && p1[3] == U_e &&
                p1[4] == U_n && p1[5] == U_d && p1[6] == U_s)
                return TK_extends;
            break;
        case U_f:
            if (p1[1] == U_i && p1[2] == U_n && p1[3] == U_a &&
                p1[4] == U_l && p1[5] == U_l && p1[6] == U_y)
                return TK_finally;
            break;
        case U_p:
            if (p1[1] == U_a && p1[2] == U_c && p1[3] == U_k &&
                p1[4] == U_a && p1[5] == U_g && p1[6] == U_e)
                return TK_package;
            else if (p1[1] == U_r && p1[2] == U_i && p1[3] == U_v &&
                     p1[4] == U_a && p1[5] == U_t && p1[6] == U_e)
                return TK_private;
            break;
    }

    return TK_Identifier;
}

int Scanner::ScanKeyword8(wchar_t *p1)
{
    switch(*p1)
    {
        case U_a:
            if (p1[1] == U_b && p1[2] == U_s &&
                p1[3] == U_t && p1[4] == U_r &&
                p1[5] == U_a && p1[6] == U_c && p1[7] == U_t)
                 return TK_abstract;
            break;
        case U_c:
            if (p1[1] == U_o && p1[2] == U_n &&
                p1[3] == U_t && p1[4] == U_i &&
                p1[5] == U_n && p1[6] == U_u && p1[7] == U_e)
                 return TK_continue;
            break;
        case U_s:
            if (p1[1] == U_t && p1[2] == U_r &&
                p1[3] == U_i && p1[4] == U_c &&
                p1[5] == U_t && p1[6] == U_f && p1[7] == U_p)
                 return TK_strictfp;
            break;
        case U_v:
            if (p1[1] == U_o && p1[2] == U_l &&
                p1[3] == U_a && p1[4] == U_t &&
                p1[5] == U_i && p1[6] == U_l && p1[7] == U_e)
                 return TK_volatile;
            break;
    }

    return TK_Identifier;
}

int Scanner::ScanKeyword9(wchar_t *p1)
{
    if (p1[0] == U_i && p1[1] == U_n && p1[2] == U_t &&
        p1[3] == U_e && p1[4] == U_r && p1[5] == U_f &&
        p1[6] == U_a && p1[7] == U_c && p1[8] == U_e)
        return TK_interface;
    else if (p1[0] == U_p && p1[1] == U_r && p1[2] == U_o &&
             p1[3] == U_t && p1[4] == U_e && p1[5] == U_c &&
             p1[6] == U_t && p1[7] == U_e && p1[8] == U_d)
        return TK_protected;
    else if (p1[0] == U_t && p1[1] == U_r && p1[2] == U_a &&
             p1[3] == U_n && p1[4] == U_s && p1[5] == U_i &&
             p1[6] == U_e && p1[7] == U_n && p1[8] == U_t)
        return TK_transient;

    return TK_Identifier;
}

int Scanner::ScanKeyword10(wchar_t *p1)
{
    if (p1[0] == U_i && p1[1] == U_m && p1[2] == U_p &&
        p1[3] == U_l && p1[4] == U_e && p1[5] == U_m &&
        p1[6] == U_e && p1[7] == U_n && p1[8] == U_t && p1[9] == U_s)
        return TK_implements;
    else if (p1[0] == U_i && p1[1] == U_n && p1[2] == U_s &&
             p1[3] == U_t && p1[4] == U_a && p1[5] == U_n &&
             p1[6] == U_c && p1[7] == U_e && p1[8] == U_o && p1[9] == U_f)
        return TK_instanceof;

    return TK_Identifier;
}

int Scanner::ScanKeyword12(wchar_t *p1)
{
    if (p1[0] == U_s && p1[1] == U_y && p1[2] == U_n &&
        p1[3] == U_c && p1[4] == U_h && p1[5] == U_r &&
        p1[6] == U_o && p1[7] == U_n && p1[8] == U_i &&
        p1[9] == U_z && p1[10] == U_e&& p1[11] == U_d)
        return TK_synchronized;

    return TK_Identifier;
}

/**********************************************************************/
/*                           CHECK_OctalLiteral:                      */
/**********************************************************************/
/* Verify that an octal token is legal. If not, issue a message.      */
/**********************************************************************/
inline void Scanner::CheckOctalLiteral(wchar_t *cursor, wchar_t *tail)
{
    if (cursor[0] == U_0 && cursor[1] != U_x && cursor[1] != U_X)
    {
        wchar_t *p;
        for (p = cursor + 1; p < tail; p++)
        {
            if (*p == U_8 || *p == U_9)
                break;
        }

        if (p < tail)
            lex -> bad_tokens.Next().Initialize(StreamError::BAD_OCTAL_CONSTANT,
                                                (unsigned) (cursor - lex -> InputBuffer()),
                                                (unsigned) (tail - lex -> InputBuffer()) - 1);
    }

    return;
}


/**********************************************************************/
/*                      ClassifyCharLiteral:                          */
/**********************************************************************/
/* This procedure is invoked to scan a character literal or a large   */
/* character literal. A large character literal is preceded by the    */
/* letter L (capital L). After the character literal has been scanned */
/* and classified, it is entered in the table without its closing     */
/* quote but with the opening quote (preceded by L if it's a large    */
/* character literal).                                                */
/**********************************************************************/
void Scanner::ClassifyCharLiteral()
{
    current_token -> SetKind(TK_CharacterLiteral);

    wchar_t *ptr = cursor + 1;

    while (*ptr != U_SINGLE_QUOTE && (! Code::IsNewline(*ptr)))
    {
        if (*ptr++ == U_BACKSLASH)   // In any case, skip the character
        {                            // If it was a backslash,
            if (! Code::IsNewline(*ptr)) // if the next char is not eol, skip it.
                ptr++;
        }
    }

    int len = ptr - cursor;
    if (*ptr == U_SINGLE_QUOTE)
    {
        if (len == 1)
            lex -> bad_tokens.Next().Initialize(StreamError::EMPTY_CHARACTER_CONSTANT,
                                                current_token -> Location(),
                                                (unsigned) (ptr - lex -> InputBuffer()));
        ptr++;
    }
    else
    {
        if (len == 1) /* Definitely, an isolated quote */
            current_token -> SetKind(0);
        lex -> bad_tokens.Next().Initialize(StreamError::UNTERMINATED_CHARACTER_CONSTANT,
                                            current_token -> Location(),
                                            (unsigned) (ptr - lex -> InputBuffer()) - 1);
    }

    current_token -> SetSymbol(control.char_table.FindOrInsertLiteral(cursor, ptr - cursor));

    cursor = ptr;
    return;
}


/**********************************************************************/
/*                     CLASSIFY_STRINGLITERAL:                        */
/**********************************************************************/
/* This procedure is invoked to scan a string literal or a large      */
/* string literal. A large string literal is preceded by the letter   */
/* L (capital L). After the string literal has been scanned and       */
/* classified, it is entered in the table without its closing double  */
/* quote but with the opening quote (preceded by L if it's a large    */
/* string literal).                                                   */
/**********************************************************************/
void Scanner::ClassifyStringLiteral()
{
    current_token -> SetKind(TK_StringLiteral);

    wchar_t *ptr = cursor + 1;

    while (*ptr != U_DOUBLE_QUOTE && (! Code::IsNewline(*ptr)))
    {
        if (*ptr++ == U_BACKSLASH)   // In any case, skip the character
        {                            // If it was a backslash,
            if (! Code::IsNewline(*ptr)) // if the next char is not eol, skip it.
                ptr++;
        }
    }

    if (*ptr == U_DOUBLE_QUOTE)
        ptr++;
    else
    {
        if ((ptr - cursor) == 1) /* Definitely, an isolated double quote */
            current_token -> SetKind(0);
        lex -> bad_tokens.Next().Initialize(StreamError::UNTERMINATED_STRING_CONSTANT,
                                            current_token -> Location(),
                                            (unsigned) (ptr - lex -> InputBuffer()) - 1);
    }

    current_token -> SetSymbol(control.string_table.FindOrInsertLiteral(cursor, ptr - cursor));

    cursor = ptr;
    return;
}


/**********************************************************************/
/*                     CLASSIFYIDORKEYWORD:                        */
/**********************************************************************/
/* This procedure is invoked when CURSOR points to one of the         */
/* following characters:                                              */
/*                                                                    */
/*      'a'                                                           */
/*      'b'                                                           */
/*      'c'                                                           */
/*      'd'                                                           */
/*      'e'                                                           */
/*      'f'                                                           */
/*      'g'                                                           */
/*      'i'                                                           */
/*      'l'                                                           */
/*      'n'                                                           */
/*      'o'                                                           */
/*      'p'                                                           */
/*      'r'                                                           */
/*      's'                                                           */
/*      't'                                                           */
/*      'v'                                                           */
/*      'w'                                                           */
/*                                                                    */
/* It scans the identifier and checks whether or not it is a keyword. */
/*                                                                    */
/* NOTE that the use of that check is a time-optimization that is not */
/* required for correctness.                                          */
/**********************************************************************/
void Scanner::ClassifyIdOrKeyword()
{
    wchar_t *ptr = cursor + 1;

    while (Code::IsAlnum(*ptr))
        ptr++;
    int len = ptr - cursor;

    current_token -> SetKind(len < 13 ? (scan_keyword[len])(cursor) : TK_Identifier);
    if (current_token -> Kind() == TK_Identifier)
    {
        current_token -> SetSymbol(control.FindOrInsertName(cursor, len));
        for (int i = 0; i < control.option.keyword_map.Length(); i++)
        {
            if (control.option.keyword_map[i].length == len && wcsncmp(cursor, control.option.keyword_map[i].name, len) == 0)
                current_token -> SetKind(control.option.keyword_map[i].key);
        }
    }
    else if (current_token -> Kind() == TK_class || current_token -> Kind() == TK_interface)
    {
        //
        // This type keyword is not nested. When we encounter an occurrence of the keyword
        // class or interface that is not enclosed in at least one set of braces, we keep track
        // of it by adding it to a list.
        //
        if (brace_stack.Size() == 0)
            lex -> type_index.Next() = current_token_index;
    }

    cursor = ptr;

    return;
}

/**********************************************************************/
/*                             CLASSIFY_ID:                           */
/**********************************************************************/
/* This procedure is invoked when CURSOR points to an alphabetic      */
/* character other than the ones identified above or '$' or '_'.      */
/* A token that starts with one of these letters is an identifier.    */
/**********************************************************************/
void Scanner::ClassifyId()
{
    wchar_t *ptr = cursor + 1;

    while (Code::IsAlnum(*ptr))
        ptr++;

    int len = ptr - cursor;

    current_token -> SetKind(TK_Identifier);
    current_token -> SetSymbol(control.FindOrInsertName(cursor, len));

    for (int i = 0; i < control.option.keyword_map.Length(); i++)
    {
        if (control.option.keyword_map[i].length == len && wcsncmp(cursor, control.option.keyword_map[i].name, len) == 0)
            current_token -> SetKind(control.option.keyword_map[i].key);
    }

    cursor = ptr;
    return;
}


/**********************************************************************/
/*                     CLASSIFY_NUMERICLITERAL:                       */
/**********************************************************************/
/* This procedure is invoked when CURSOR points directly to one of    */
/* the characters below or to a '.' followed by one of the characters */
/* below:                                                             */
/*                                                                    */
/*        case '0': case '1': case '2': case '3': case '4':           */
/*        case '5': case '6': case '7': case '8': case '9':           */
/*                                                                    */
/* Such a token is classified as a numeric literal:                   */
/*                                                                    */
/*   TK_LongLiteral, TK_IntegerLiteral,                               */
/*   TK_DOUBLELiteral, TK_FloatingPointLiteral                        */
/**********************************************************************/
void Scanner::ClassifyNumericLiteral()
{
    /******************************************************************/
    /* Scan the initial sequence of digits if any.                    */
    /******************************************************************/
    wchar_t *ptr;
    for (ptr = cursor; Code::IsDigit(*ptr); ptr++)
        ;

    /******************************************************************/
    /* We now take an initial crack at classifying the numeric token. */
    /* we have four cases to consider.                                */
    /*                                                                */
    /* 1) If the initial (perhaps an empty) sequence of digits is     */
    /*    followed by a period ('.'), we have a floating-constant.    */
    /*    We scan the sequence of digits (if any) that follows the    */
    /*    period.                                                     */
    /*                                                                */
    /* 2) Otherwise, we hava an integer literal.                      */
    /*                                                                */
    /*    If the initial (can't be empty) sequence of digits start    */
    /*    with "0x" or "0X" we have a hexadecimal constant:           */
    /*    continue scanning all hex-digits that follow the 'x'.       */
    /******************************************************************/
    if (*ptr == U_DOT)
    {
        current_token -> SetKind(TK_DoubleLiteral);
        for (ptr++; Code::IsDigit(*ptr); ptr++)
            ;
    }
    else
    {
        current_token -> SetKind(TK_IntegerLiteral);
        if (*cursor == U_0 && (cursor[1] == U_x || cursor[1] == U_X))
        {
            ptr = cursor + 2;
            if (isxdigit(*ptr))
            {
                for (ptr++; isxdigit(*ptr); ptr++)
                    ;
            }
            else lex -> bad_tokens.Next().Initialize(StreamError::INVALID_HEX_CONSTANT,
                                                     current_token -> Location(),
                                                     (unsigned) (ptr - lex -> InputBuffer()) - 1);
        }
    }

    /******************************************************************/
    /* If the initial numeric token is followed by an exponent, then  */
    /* it is a floating-constant. If that's the case, the literal is  */
    /* reclassified ant the exponent is scanned.                      */
    /*                                                                */
    /* NOTE that as 'E' and 'e' are legitimate hexadecimal digits, we */
    /* don't have to worry about a hexadecimal constant being used as */
    /* the prefix of a floating-constant. E.g., 0x123e12 is tokenized */
    /* as a single hexadecimal digit. The string 0x123e+12 gets       */
    /* broken down as the hex number 0x123e, the operator '+' and the */
    /* decimal constant 12.                                           */
    /******************************************************************/
    if (*ptr == U_e || *ptr == U_E)
    {
        current_token -> SetKind(TK_DoubleLiteral);

        ptr++; /* Skip the 'e' or 'E' */

        if (*ptr == U_PLUS || *ptr == U_MINUS)
            ptr++; /* Skip the '+' or '-' */

        if (Code::IsDigit(*ptr))
        {
            for (ptr++; Code::IsDigit(*ptr); ptr++)
                ;
        }
        else lex -> bad_tokens.Next().Initialize(StreamError::INVALID_FLOATING_CONSTANT_EXPONENT,
                                                 current_token -> Location(),
                                                 (unsigned) (ptr - lex -> InputBuffer()) - 1);
    }

    /******************************************************************/
    /* A numeric constant may be suffixed by a letter that further    */
    /* qualifies what kind of a constant it is. We check for these    */
    /* suffixes here.                                                 */
    /******************************************************************/
    int len;

    if (*ptr == U_f || *ptr == U_F)
    {
        ptr++;
        len = ptr - cursor;
        current_token -> SetSymbol(control.float_table.FindOrInsertLiteral(cursor, len));
        current_token -> SetKind(TK_FloatingPointLiteral);
    }
    else if (*ptr == U_d || *ptr == U_D)
    {
        ptr++;
        len = ptr - cursor;
        current_token -> SetSymbol(control.double_table.FindOrInsertLiteral(cursor, len));
        current_token -> SetKind(TK_DoubleLiteral);
    }
    else if (current_token -> Kind() == TK_IntegerLiteral)
    {
        if (*ptr == U_l || *ptr == U_L)
        {
            ptr++; /* Skip the 'l' or 'L' */
            len = ptr - cursor;
            current_token -> SetSymbol(control.long_table.FindOrInsertLiteral(cursor, len));
            current_token -> SetKind(TK_LongLiteral);
        }
        else
        {
            len = ptr - cursor;
            current_token -> SetSymbol(control.int_table.FindOrInsertLiteral(cursor, len));
        }

        CheckOctalLiteral(cursor, ptr);
    }
    else
    {
        len = ptr - cursor;
        current_token -> SetSymbol(control.double_table.FindOrInsertLiteral(cursor, len));
        current_token -> SetKind(TK_DoubleLiteral);
    }

    /******************************************************************/
    /* We now have scanned the complete token and it has been properly*/
    /* classified. CURSOR points to its first character in the buffer */
    /* and PTR points to the character immediately following it. We   */
    /* insert the name into the name table and if the token is an     */
    /* octal constant, we check that all the digits in its name are   */
    /* in the range 0-7.                                              */
    /******************************************************************/

    cursor = ptr;
    return;
}


/**********************************************************************/
/*                         CLASSIFY_COLON:                            */
/**********************************************************************/
void Scanner::ClassifyColon()
{
    current_token -> SetKind(TK_COLON);

    cursor++;

    return;
}


/**********************************************************************/
/*                          CLASSIFY_PLUS:                            */
/**********************************************************************/
void Scanner::ClassifyPlus()
{
    cursor++;

    if (*cursor == U_PLUS)
    {
        cursor++;
        current_token -> SetKind(TK_PLUS_PLUS);
    }
    else if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_PLUS_EQUAL);
    }
    else current_token -> SetKind(TK_PLUS);

    return;
}


/**********************************************************************/
/*                         CLASSIFY_MINUS:                            */
/**********************************************************************/
void Scanner::ClassifyMinus()
{
    cursor++;

    if (*cursor == U_MINUS)
    {
        cursor++;
        current_token -> SetKind(TK_MINUS_MINUS);
    }
    else if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_MINUS_EQUAL);
    }
    else current_token -> SetKind(TK_MINUS);

    return;
}


/**********************************************************************/
/*                          CLASSIFY_STAR:                            */
/**********************************************************************/
void Scanner::ClassifyStar()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_MULTIPLY_EQUAL);
    }
    else current_token -> SetKind(TK_MULTIPLY);

    return;
}


/**********************************************************************/
/*                         CLASSIFY_SLASH:                            */
/**********************************************************************/
void Scanner::ClassifySlash()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_DIVIDE_EQUAL);
    }
    else current_token -> SetKind(TK_DIVIDE);

    return;
}


/**********************************************************************/
/*                         CLASSIFY_LESS:                             */
/**********************************************************************/
void Scanner::ClassifyLess()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_LESS_EQUAL);
    }
    else if (*cursor == U_LESS)
    {
        cursor++;

        if (*cursor == U_EQUAL)
        {
            cursor++;
            current_token -> SetKind(TK_LEFT_SHIFT_EQUAL);
        }
        else current_token -> SetKind(TK_LEFT_SHIFT);
    }
    else current_token -> SetKind(TK_LESS);

    return;
}


/**********************************************************************/
/*                        CLASSIFY_GREATER:                           */
/**********************************************************************/
void Scanner::ClassifyGreater()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_GREATER_EQUAL);
    }
    else if (*cursor == U_GREATER)
    {
        cursor++;

        if (*cursor == U_EQUAL)
        {
            cursor++;
            current_token -> SetKind(TK_RIGHT_SHIFT_EQUAL);
        }
        else if (*cursor == U_GREATER)
        {
            cursor++;

            if (*cursor == U_EQUAL)
            {
                cursor++;
                current_token -> SetKind(TK_UNSIGNED_RIGHT_SHIFT_EQUAL);
            }
            else current_token -> SetKind(TK_UNSIGNED_RIGHT_SHIFT);
        }
        else current_token -> SetKind(TK_RIGHT_SHIFT);
    }
    else current_token -> SetKind(TK_GREATER);

    return;
}


/**********************************************************************/
/*                          CLASSIFY_AND:                             */
/**********************************************************************/
void Scanner::ClassifyAnd()
{
    cursor++;

    if (*cursor == U_AMPERSAND)
    {
        cursor++;
        current_token -> SetKind(TK_AND_AND);
    }
    else if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_AND_EQUAL);
    }
    else current_token -> SetKind(TK_AND);

    return;
}


/**********************************************************************/
/*                          CLASSIFY_OR:                              */
/**********************************************************************/
void Scanner::ClassifyOr()
{
    cursor++;

    if (*cursor == U_BAR)
    {
        cursor++;
        current_token -> SetKind(TK_OR_OR);
    }
    else if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_OR_EQUAL);
    }
    else current_token -> SetKind(TK_OR);

    return;
}


/**********************************************************************/
/*                          CLASSIFY_XOR:                             */
/**********************************************************************/
void Scanner::ClassifyXor()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_XOR_EQUAL);
    }
    else current_token -> SetKind(TK_XOR);

    return;
}


/**********************************************************************/
/*                          CLASSIFY_NOT:                             */
/**********************************************************************/
void Scanner::ClassifyNot()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_NOT_EQUAL);
    }
    else current_token -> SetKind(TK_NOT);

    return;
}


/**********************************************************************/
/*                         CLASSIFY_EQUAL:                            */
/**********************************************************************/
void Scanner::ClassifyEqual()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_EQUAL_EQUAL);
    }
    else current_token -> SetKind(TK_EQUAL);

    return;
}


/**********************************************************************/
/*                          CLASSIFY_MOD:                             */
/**********************************************************************/
void Scanner::ClassifyMod()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_REMAINDER_EQUAL);
    }
    else current_token -> SetKind(TK_REMAINDER);

    return;
}


/**********************************************************************/
/*                         CLASSIFY_PERIOD:                           */
/**********************************************************************/
void Scanner::ClassifyPeriod()
{
    if (Code::IsDigit(cursor[1])) // Is period immediately followed by digit?
        ClassifyNumericLiteral();
    else
    {
        current_token -> SetKind(TK_DOT);

        cursor++;
    }

    return;
}


/**********************************************************************/
/*                         CLASSIFY_SEMICOLON:                        */
/**********************************************************************/
void Scanner::ClassifySemicolon()
{
    current_token -> SetKind(TK_SEMICOLON);

    cursor++;

    return;
}


/**********************************************************************/
/*                           CLASSIFY_COMMA:                          */
/**********************************************************************/
void Scanner::ClassifyComma()
{
    current_token -> SetKind(TK_COMMA);

    cursor++;

    return;
}


/**********************************************************************/
/*                           CLASSIFY_LBRACE:                         */
/**********************************************************************/
void Scanner::ClassifyLbrace()
{
    //
    // Instead of setting the symbol for a left brace, we keep track of it.
    // When we encounter its matching right brace, we use the symbol field
    // to identify its counterpart.
    //
    brace_stack.Push(current_token_index);

    current_token -> SetKind(TK_LBRACE);

    cursor++;

    return;
}


/**********************************************************************/
/*                           CLASSIFY_RBRACE:                         */
/**********************************************************************/
void Scanner::ClassifyRbrace()
{
    //
    // When a left brace in encountered, it is pushed into the brace_stack.
    // When its matching right brace in encountered, we pop the left brace
    // and make it point to its matching right brace.
    //
    LexStream::TokenIndex left_brace = brace_stack.Top();
    if (left_brace) // This right brace is matched by a left one
    {
        lex -> token_stream[left_brace].SetRightBrace(current_token_index);
        brace_stack.Pop();
    }

    current_token -> SetKind(TK_RBRACE);

    cursor++;

    return;
}


/**********************************************************************/
/*                           CLASSIFY_LPAREN:                         */
/**********************************************************************/
void Scanner::ClassifyLparen()
{
    current_token -> SetKind(TK_LPAREN);

    cursor++;

    return;
}


/**********************************************************************/
/*                           CLASSIFY_RPAREN:                         */
/**********************************************************************/
void Scanner::ClassifyRparen()
{
    current_token -> SetKind(TK_RPAREN);

    cursor++;

    return;
}


/**********************************************************************/
/*                          CLASSIFY_LBRACKET:                        */
/**********************************************************************/
void Scanner::ClassifyLbracket()
{
    current_token -> SetKind(TK_LBRACKET);

    cursor++;

    return;
}


/**********************************************************************/
/*                          CLASSIFY_RBRACKET:                        */
/**********************************************************************/
void Scanner::ClassifyRbracket()
{
    current_token -> SetKind(TK_RBRACKET);

    cursor++;

    return;
}


/**********************************************************************/
/*                         CLASSIFY_COMPLEMENT:                       */
/**********************************************************************/
void Scanner::ClassifyComplement()
{
    current_token -> SetKind(TK_TWIDDLE);

    cursor++;

    return;
}


/**********************************************************************/
/*                        CLASSIFY_BAD_TOKEN:                         */
/**********************************************************************/
void Scanner::ClassifyBadToken()
{
    if (++cursor < &lex -> InputBuffer()[lex -> InputBufferLength()]) // not the terminating character?
    {
         current_token -> SetKind(0);
         current_token -> SetSymbol(control.FindOrInsertName(cursor - 1, 1));

         lex -> bad_tokens.Next().Initialize(StreamError::BAD_TOKEN,
                                             current_token -> Location(),
                                             current_token -> Location());
    }
    else
    {
        current_token -> SetKind(TK_EOF);
    }

    return;
}


/**********************************************************************/
/*                        CLASSIFY_QUESTION:                          */
/**********************************************************************/
/**********************************************************************/
void Scanner::ClassifyQuestion()
{
    current_token -> SetKind(TK_QUESTION);

    cursor++;

    return;
}


/**********************************************************************/
/*                     CLASSIFY_NONASCIIUNICODE:                      */
/**********************************************************************/
void Scanner::ClassifyNonAsciiUnicode()
{
    if (Code::IsAlpha(*cursor)) // Some kind of non-ascii unicode letter
        ClassifyId();
    else 
        ClassifyBadToken();
    return;
}


