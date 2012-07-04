// $Id: scanner.cpp,v 1.27 2002/05/16 21:51:04 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "scanner.h"
#include "control.h"
#include "error.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

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
Scanner::Scanner(Control &control_) : control(control_),
                                      dollar_warning_given(false)
{
    //
    // If this assertion fails, the Token structure in stream.h must be
    // redesigned !!!
    //
    assert(NUM_TERMINALS < 128);

    //
    // If this assertion fails, then gencode.java is at fault.
    //
    assert(Code::CodeCheck());

    //
    // ----------------------------------------------------------------------
    // We are pulling this code out because we are tired of defending it. We
    // tought it was obvious that either $ should not have been used for
    // compiler generated variables or that users should not be allowed to
    // use in variable names...
    // ----------------------------------------------------------------------
    //
    // Uncommenting this makes the use of $ a hard lexical error.  However,
    // when this is not an error, we will issue a warning in ClassifyId below.
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
}


//
// Associate a lexical stream with this file.
//
void Scanner::Initialize(FileSymbol *file_symbol)
{
    lex = new LexStream(control, file_symbol);
    lex -> Reset();

    current_token_index = lex -> GetNextToken(0); // Get 0th token.
    current_token = &(lex -> token_stream[current_token_index]);
    current_token -> SetKind(0);

#ifdef JIKES_DEBUG
    if (control.option.debug_comments)
    {
        // Add 0th comment.
        LexStream::Comment *current_comment = &(lex -> comment_stream.Next());
        current_comment -> string = NULL;
        current_comment -> length = 0;
        // No token precedes this comment.
        current_comment -> previous_token = -1;
        current_comment -> location = 0;
    }
#endif // JIKES_DEBUG

    lex -> line_location.Next() = 0; // Mark starting location of line # 0
}


//
// This is one of the main entry point for the Java lexical analyser. Its
// input is the name of a regular text file. Its output is a stream of tokens.
//
void Scanner::SetUp(FileSymbol *file_symbol)
{
    Initialize(file_symbol);
    lex -> CompressSpace();
    file_symbol -> lex_stream = lex;
}


//
// This is one of the main entry point for the Java lexical analyser. Its
// input is the name of a regular text file. Its output is a stream of tokens.
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

        if (control.option.dump_errors)
        {
            lex -> SortMessages();
            for (int i = 0; i < lex -> bad_tokens.Length(); i++)
                JikesAPI::getInstance()->reportError(&(lex->bad_tokens[i]));
        }
        lex -> DestroyInput(); // get rid of input buffer
    }
    else
    {
        delete lex;
        lex = NULL;
    }

    file_symbol -> lex_stream = lex;
}


//
// Scan the InputBuffer() and process all tokens and comments.
//
void Scanner::Scan()
{
    input_buffer_tail = &cursor[lex -> InputBufferLength()];

    //
    // CURSOR is assumed to point to the next character to be scanned.
    // Using CURSOR, we jump to the proper classification function
    // which scans and classifies the token and returns the location of
    // the character immediately following it.
    //
    do
    {
        SkipSpaces();

        //
        // Allocate space for next token and set its location.
        //
        current_token_index = lex -> GetNextToken(cursor -
                                                  lex -> InputBuffer());
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

    for (LexStream::TokenIndex left_brace = brace_stack.Top();
         left_brace; left_brace = brace_stack.Top())
    {
        lex -> token_stream[left_brace].SetRightBrace(current_token_index);
        brace_stack.Pop();
    }
}


//
// CURSOR points to the starting position of a comment.  Scan the
// the comment and return the location of the character immediately
// following it. CURSOR is advanced accordingly.
//
void Scanner::ScanStarComment()
{
    unsigned location = cursor - lex -> InputBuffer();
#ifdef JIKES_DEBUG
    LexStream::Comment *current_comment = (control.option.debug_comments
                                           ? &(lex -> comment_stream.Next())
                                           : new LexStream::Comment());
    current_comment -> string = NULL;
    // The token that precedes this comment.
    current_comment -> previous_token = current_token_index;
    current_comment -> location = location;
#endif // JIKES_DEBUG

    cursor += 2;

    //
    // If this comment starts with the prefix "/**" then it is a document
    // comment. Check whether or not it contains the deprecated tag and if so,
    // mark the token preceeding it. The @deprecated tag must appear at the
    // beginning of a line. According to Sun,
    // http://java.sun.com/j2se/1.4/docs/tooldocs/win32/javadoc.html#comments,
    // this means ignoring whitespace, *, and /** patterns. But in practice,
    // javac doesn't quite implement it this way, completely ignoring /**
    // separators, and rejecting \f and \t after *<space>*.
    // This implementation also ignores /**, but treats whitespace correctly.
    //
    // Note that we exploit, where possible, the fact that the stream is
    // doctored to always end in U_LINE_FEED, U_CTRL_Z; but remember this
    // sequence can legally occur before the stream end as well.
    //
    if (*cursor == U_STAR)
    {
        enum
        {
            HEADER,
            STAR,
            REMAINDER
        } state = HEADER;
        while (cursor != input_buffer_tail)
        {
            switch (*cursor++)
            {
            case U_CARRIAGE_RETURN:
                assert(false && "The stream should have converted \\r to \\n");
            case U_LINE_FEED:
                // Record new line.
                lex -> line_location.Next() = cursor - lex -> InputBuffer();
                // fallthrough
            case U_SPACE:
            case U_FORM_FEED:
            case U_HORIZONTAL_TAB:
                if (state != REMAINDER)
                    state = HEADER;
                break;
            case U_STAR:
                if (state != REMAINDER || *cursor == U_SLASH)
                    state = STAR;
                break;
            case U_SLASH:
                if (state == STAR)
                {
#ifdef JIKES_DEBUG
                    current_comment -> length = ((cursor -
                                                  lex -> InputBuffer()) -
                                                 current_comment -> location);
                    if (! control.option.debug_comments)
                        delete current_comment;
#endif // JIKES_DEBUG
                    return;
                }
                // fallthrough
            default:
                if (state != REMAINDER)
                {
                    state = REMAINDER;
                    if (cursor[-1] == U_AT &&
                        cursor[0] == U_d &&
                        cursor[1] == U_e &&
                        cursor[2] == U_p &&
                        cursor[3] == U_r &&
                        cursor[4] == U_e &&
                        cursor[5] == U_c &&
                        cursor[6] == U_a &&
                        cursor[7] == U_t &&
                        cursor[8] == U_e &&
                        cursor[9] == U_d)
                    {
                        // Mark the token that precedes this comment.
                        current_token -> SetDeprecated();
                    }
                }
            }
        }
    }
    else
    {
        while (cursor != input_buffer_tail)
        {
            if (*cursor == U_STAR) // Potential comment closer.
            {
                while (*++cursor == U_STAR)
                    ;
                if (*cursor == U_SLASH)
                {
                    cursor++;
#ifdef JIKES_DEBUG
                    current_comment -> length = ((cursor -
                                                  lex -> InputBuffer()) -
                                                 current_comment -> location);
                    if (! control.option.debug_comments)
                        delete current_comment;
#endif // JIKES_DEBUG
                    return;
                }
            }
            if (Code::IsNewline(*cursor++)) // Record new line.
            {
                lex -> line_location.Next() = cursor - lex -> InputBuffer();
            }
        }
    }

    //
    // If we got here, we are in an unterminated comment. Discard the
    // U_LINE_FEED and U_CTRL_Z that end the stream.
    //
    cursor -= 2;
    lex -> ReportMessage(StreamError::UNTERMINATED_COMMENT,
                         location,
                         (unsigned) (cursor - lex -> InputBuffer()) - 1);

#ifdef JIKES_DEBUG
    current_comment -> length = ((cursor - lex -> InputBuffer()) -
                                 current_comment -> location);
    if (! control.option.debug_comments)
        delete current_comment;
#endif // JIKES_DEBUG
}


//
// CURSOR points to the starting position of a comment.  Scan the
// the comment and return the location of the character immediately
// following it. CURSOR is advanced accordingly.
//
void Scanner::ScanSlashComment()
{
#ifdef JIKES_DEBUG
    if (control.option.debug_comments)
    {
        LexStream::Comment *current_comment = &(lex -> comment_stream.Next());
        current_comment -> string = NULL;
        // The token that precedes this comment.
        current_comment -> previous_token = current_token_index;
        current_comment -> location = cursor - lex -> InputBuffer();
        for (cursor += 2; ! Code::IsNewline(*cursor); cursor++)
            ;  // Skip all until \n
        current_comment -> length = ((cursor - lex -> InputBuffer()) -
                                     current_comment -> location);
        return;
    }
#endif // JIKES_DEBUG
    for (cursor += 2; ! Code::IsNewline(*cursor); cursor++)
        ; // Skip all until \n
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
        while (Code::IsNewline(*cursor))  // Starting a new line?
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
}


//
// scan_keyword(i):
// Scan an identifier of length I and determine if it is a keyword.
//
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
    switch (*p1)
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
        case U_a:
            if (p1[1] == U_s && p1[2] == U_s &&
                p1[3] == U_e && p1[4] == U_r && p1[5] == U_t)
                return TK_assert;
            break;
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
    switch (*p1)
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
    switch (*p1)
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
    if (p1[0] == U_i)
        if (p1[1] == U_m && p1[2] == U_p && p1[3] == U_l &&
            p1[4] == U_e && p1[5] == U_m && p1[6] == U_e &&
            p1[7] == U_n && p1[8] == U_t && p1[9] == U_s)
            return TK_implements;
        else if (p1[1] == U_n && p1[2] == U_s && p1[3] == U_t &&
                 p1[4] == U_a && p1[5] == U_n && p1[6] == U_c &&
                 p1[7] == U_e && p1[8] == U_o && p1[9] == U_f)
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


//
// This procedure is invoked to scan a character literal. After the character
// literal has been scanned and classified, it is entered in the table
// with both quotes stripped.
//
void Scanner::ClassifyCharLiteral()
{
    current_token -> SetKind(TK_CharacterLiteral);

    wchar_t *ptr = ++cursor;

    if (*ptr == U_SINGLE_QUOTE)
    {
        lex -> ReportMessage(StreamError::EMPTY_CHARACTER_CONSTANT,
                             current_token -> Location(),
                             (unsigned) (ptr - lex -> InputBuffer()));
        current_token -> SetKind(0);
    }
    else if (*ptr == U_BACKSLASH)
    {
        switch (*++ptr)
        {
        case U_b:
        case U_f:
        case U_n:
        case U_r:
        case U_t:
        case U_SINGLE_QUOTE:
        case U_DOUBLE_QUOTE:
        case U_BACKSLASH:
            break;
        case U_0:
        case U_1:
        case U_2:
        case U_3:
            if (! Code::IsOctalDigit(ptr[1]))
                break;
            ptr++;
            // fallthrough
        case U_4:
        case U_5:
        case U_6:
        case U_7:
            if (! Code::IsOctalDigit(ptr[1]))
                break;
            ptr++;
            break;
        case U_u:
            //
            // By now, Unicode escapes have already been flattened; and it is
            // illegal to try it twice (such as '\u005cu0000').
            //
        default:
            lex -> ReportMessage(StreamError::INVALID_ESCAPE_SEQUENCE,
                                 (unsigned) (cursor - lex -> InputBuffer()),
                                 (unsigned) (ptr - lex -> InputBuffer()));
            current_token -> SetKind(0);
        }
    }

    if (Code::IsNewline(*ptr))
    {
        if (current_token -> Kind())
        {
            lex -> ReportMessage(StreamError::UNTERMINATED_CHARACTER_CONSTANT,
                                 current_token -> Location(),
                                 (unsigned) (ptr - lex -> InputBuffer()));
            current_token -> SetKind(0);
        }
        lex -> line_location.Next() = ptr - lex -> InputBuffer() + 1;
    }
    else if (*++ptr != U_SINGLE_QUOTE)
    {
        ptr--;
        if (current_token -> Kind())
        {
            lex -> ReportMessage(StreamError::UNTERMINATED_CHARACTER_CONSTANT,
                                 current_token -> Location(),
                                 (unsigned) (ptr - lex -> InputBuffer()));
            current_token -> SetKind(0);
        }
    }

    current_token ->
        SetSymbol(control.char_table.FindOrInsertLiteral(cursor,
                                                         ptr - cursor));

    cursor = ptr + 1;
}


//
// This procedure is invoked to scan a string literal. After the string
// literal has been scanned and classified, it is entered in the table with
// both quotes stripped.
//
void Scanner::ClassifyStringLiteral()
{
    current_token -> SetKind(TK_StringLiteral);

    wchar_t *ptr = ++cursor;

    while (*ptr != U_DOUBLE_QUOTE && ! Code::IsNewline(*ptr))
    {
        if (*ptr++ == U_BACKSLASH)
        {
            switch (*ptr++)
            {
            case U_b:
            case U_f:
            case U_n:
            case U_r:
            case U_t:
            case U_SINGLE_QUOTE:
            case U_DOUBLE_QUOTE:
            case U_BACKSLASH:
            case U_0:
            case U_1:
            case U_2:
            case U_3:
            case U_4:
            case U_5:
            case U_6:
            case U_7:
                break;
            case U_u:
                //
                // By now, Unicode escapes have already been flattened; and it
                // is illegal to try it twice (such as "\u005cu0000").
                //
            default:
                lex -> ReportMessage(StreamError::INVALID_ESCAPE_SEQUENCE,
                                     (unsigned) (ptr - lex -> InputBuffer()) - 1,
                                     (unsigned) (ptr - lex -> InputBuffer()));
                current_token -> SetKind(0);
                if (Code::IsNewline(ptr[-1]))
                    ptr--; // This will break us out of the loop.
            }
        }
    }

    if (Code::IsNewline(*ptr))
    {
        if (current_token -> Kind())
            lex -> ReportMessage(StreamError::UNTERMINATED_STRING_CONSTANT,
                                 current_token -> Location(),
                                 (unsigned) (ptr - lex -> InputBuffer()));
        current_token -> SetKind(0);
        lex -> line_location.Next() = ptr - lex -> InputBuffer() + 1;
    }

    current_token ->
        SetSymbol(control.string_table.FindOrInsertLiteral(cursor,
                                                           ptr - cursor));

    cursor = ptr + 1;
}


//
// This procedure is invoked when CURSOR points to a letter which starts a
// keyword. It scans the identifier and checks whether or not it is a keyword.
// Note that the use of that check is a time-optimization that is not
// required for correctness.
//
void Scanner::ClassifyIdOrKeyword()
{
    wchar_t *ptr = cursor + 1;
    bool has_dollar = false;

    while (Code::IsAlnum(*ptr))
    {
        has_dollar = has_dollar || (*ptr == U_DS);
        ptr++;
    }
    int len = ptr - cursor;

    current_token -> SetKind(len < 13 ? (scan_keyword[len])(cursor)
                             : TK_Identifier);

    if (current_token -> Kind() == TK_assert &&
        control.option.source < JikesOption::SDK1_4)
    {
        lex -> ReportMessage(StreamError::DEPRECATED_IDENTIFIER_ASSERT,
                             current_token -> Location(),
                             (unsigned) (current_token -> Location() + len - 1));
        current_token -> SetKind(TK_Identifier);
    }
    if (has_dollar && ! dollar_warning_given)
    {
        dollar_warning_given = true;
        lex -> ReportMessage(StreamError::DOLLAR_IN_IDENTIFIER,
                             current_token -> Location(),
                             (unsigned) (current_token -> Location() + len - 1));
    }

    if (current_token -> Kind() == TK_Identifier)
    {
        current_token -> SetSymbol(control.FindOrInsertName(cursor, len));
        for (int i = 0; i < control.option.keyword_map.Length(); i++)
        {
            if (control.option.keyword_map[i].length == len &&
                wcsncmp(cursor, control.option.keyword_map[i].name, len) == 0)
            {
                current_token -> SetKind(control.option.keyword_map[i].key);
            }
        }
    }
    else if (current_token -> Kind() == TK_class ||
             current_token -> Kind() == TK_interface)
    {
        //
        // This type keyword is not nested. When we encounter an occurrence of
        // the keyword class or interface that is not enclosed in at least one
        // set of braces, we keep track of it by adding it to a list.
        //
        if (brace_stack.Size() == 0)
            lex -> type_index.Next() = current_token_index;
    }

    cursor = ptr;
}

//
// This procedure is invoked when CURSOR points to an identifier start
// which cannot start a keyword.
//
void Scanner::ClassifyId()
{
    bool has_dollar = (*cursor == U_DS);
    wchar_t *ptr = cursor + 1;

    while (Code::IsAlnum(*ptr))
    {
        has_dollar = has_dollar || (*ptr == U_DS);
        ptr++;
    }

    int len = ptr - cursor;

    if (has_dollar && ! dollar_warning_given)
    {
        dollar_warning_given = true;
        lex -> ReportMessage(StreamError::DOLLAR_IN_IDENTIFIER,
                             current_token -> Location(),
                             (unsigned) (current_token -> Location() + len - 1));
    }

    current_token -> SetKind(TK_Identifier);
    current_token -> SetSymbol(control.FindOrInsertName(cursor, len));

    for (int i = 0; i < control.option.keyword_map.Length(); i++)
    {
        if (control.option.keyword_map[i].length == len &&
            wcsncmp(cursor, control.option.keyword_map[i].name, len) == 0)
        {
            current_token -> SetKind(control.option.keyword_map[i].key);
        }
    }

    cursor = ptr;
}


//
// This procedure is invoked when CURSOR points directly to '0' - '9' or '.'.
// Such a token is classified as a numeric literal: TK_LongLiteral,
// TK_IntegerLiteral, TK_DoubleLiteral, or TK_FloatLiteral.
//
void Scanner::ClassifyNumericLiteral()
{
    //
    // Scan the initial sequence of digits, if any.
    //
    wchar_t *ptr = cursor - 1;
    while (Code::IsDigit(*++ptr));

    //
    // We now take an initial crack at classifying the numeric token.
    // We have two initial cases to consider:
    //
    // 1) If the initial (perhaps empty) sequence of digits is followed by
    //    '.', we have a floating-point constant. We scan the sequence of
    //    digits (if any) that follows the period.
    // 2) Otherwise, we have an integer literal. If the initial (non-empty)
    //    sequence of digits start with "0x" or "0X" we have a hexadecimal
    //    constant: continue scanning all hex digits that follow the 'x'. If
    //    the digits start with "0", we have an octal constant: continue
    //    scanning only octal digits. Note that a non-octal digit starts a
    //    new numeric literal, although this will cause a syntax error
    //    downstream. In other words, 019 is parsed as the literal 01 followed
    //    by the literal 9.
    //
    if (*ptr == U_DOT)
    {
        current_token -> SetKind(TK_DoubleLiteral);
        while (Code::IsDigit(*++ptr));
    }
    else
    {
        current_token -> SetKind(TK_IntegerLiteral);
        if (*cursor == U_0)
        {
            if (cursor[1] == U_x || cursor[1] == U_X)
            {
                ptr = cursor + 2;
                // Don't us isxdigit, it's not platform independent.
                if (Code::IsHexDigit(*ptr))
                {
                    while (Code::IsHexDigit(*++ptr));
                }
                else lex -> ReportMessage(StreamError::INVALID_HEX_CONSTANT,
                                          current_token -> Location(),
                                          (unsigned) (ptr - lex -> InputBuffer()));
            }
            else if (! (((*ptr == U_e || *ptr == U_E) &&
                         (Code::IsDigit(ptr[1]) ||
                          ((ptr[1] == U_PLUS || ptr[1] == U_MINUS) &&
                           Code::IsDigit(ptr[2])))) ||
                        *ptr == U_d || *ptr == U_D ||
                        *ptr == U_f || *ptr == U_F))
            {
                ptr = cursor;
                while (Code::IsOctalDigit(*++ptr));
            }
        }
    }

    //
    // If the initial numeric token is followed by an exponent, then it is a
    // floating-point constant. If that's the case, the literal is
    // reclassified and the exponent is scanned. Note that as 'E' and 'e' are
    // legitimate hexadecimal digits, we don't have to worry about a
    // hexadecimal constant being used as the prefix of a floating-point
    // constant. An exponent overrides an octal literal, as do the float and
    // double suffixes. However, a missing exponent results in splitting the
    // token (although this will always cause a downstream syntax error).
    //
    // For example, 0x123e12 is tokenized as a single hexadecimal digit, while
    // the string 0x123e+12 gets broken down as the hex number 0x123e, the
    // operator '+', and the decimal constant 12. Meanwhile, 019e+0 and 019d
    // are both tokenized as a single floating-point constant 19.0. But 1e+
    // is parsed as the integer literal 1, the identifier e, and the operator
    // '+', which causes a syntax error downstream.
    //
    if ((*ptr == U_e || *ptr == U_E) &&
        (Code::IsDigit(ptr[1]) ||
         ((ptr[1] == U_PLUS || ptr[1] == U_MINUS) &&
          Code::IsDigit(ptr[2]))))
    {
        current_token -> SetKind(TK_DoubleLiteral);
        ptr++; // Skip the 'e' or 'E'.
        if (*ptr == U_PLUS || *ptr == U_MINUS)
            ptr++; // Skip the '+' or '-'.
        while (Code::IsDigit(*++ptr));
    }

    //
    // A numeric constant may be suffixed by a letter that further qualifies
    // what kind of a constant it is. We check for these suffixes here.
    //
    int len;

    if (*ptr == U_f || *ptr == U_F)
    {
        len = ++ptr - cursor;
        current_token ->
            SetSymbol(control.float_table.FindOrInsertLiteral(cursor, len));
        current_token -> SetKind(TK_FloatLiteral);
    }
    else if (*ptr == U_d || *ptr == U_D)
    {
        len = ++ptr - cursor;
        current_token ->
            SetSymbol(control.double_table.FindOrInsertLiteral(cursor, len));
        current_token -> SetKind(TK_DoubleLiteral);
    }
    else if (current_token -> Kind() == TK_IntegerLiteral)
    {
        if (*ptr == U_l || *ptr == U_L)
        {
            len = ++ptr - cursor;
            current_token ->
                SetSymbol(control.long_table.FindOrInsertLiteral(cursor, len));
            current_token -> SetKind(TK_LongLiteral);
        }
        else
        {
            len = ptr - cursor;
            current_token ->
                SetSymbol(control.int_table.FindOrInsertLiteral(cursor, len));
        }
    }
    else
    {
        len = ptr - cursor;
        current_token ->
            SetSymbol(control.double_table.FindOrInsertLiteral(cursor, len));
        current_token -> SetKind(TK_DoubleLiteral);
    }

    cursor = ptr;
}


void Scanner::ClassifyColon()
{
    current_token -> SetKind(TK_COLON);

    cursor++;
}


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
}


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
}


void Scanner::ClassifyStar()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_MULTIPLY_EQUAL);
    }
    else current_token -> SetKind(TK_MULTIPLY);
}


void Scanner::ClassifySlash()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_DIVIDE_EQUAL);
    }
    else current_token -> SetKind(TK_DIVIDE);
}


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
}


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
}


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
}


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
}


void Scanner::ClassifyXor()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_XOR_EQUAL);
    }
    else current_token -> SetKind(TK_XOR);
}


void Scanner::ClassifyNot()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_NOT_EQUAL);
    }
    else current_token -> SetKind(TK_NOT);
}


void Scanner::ClassifyEqual()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_EQUAL_EQUAL);
    }
    else current_token -> SetKind(TK_EQUAL);
}


void Scanner::ClassifyMod()
{
    cursor++;

    if (*cursor == U_EQUAL)
    {
        cursor++;
        current_token -> SetKind(TK_REMAINDER_EQUAL);
    }
    else current_token -> SetKind(TK_REMAINDER);
}


void Scanner::ClassifyPeriod()
{
    if (Code::IsDigit(cursor[1])) // Is period immediately followed by digit?
        ClassifyNumericLiteral();
    else
    {
        current_token -> SetKind(TK_DOT);

        cursor++;
    }
}


void Scanner::ClassifySemicolon()
{
    current_token -> SetKind(TK_SEMICOLON);

    cursor++;
}


void Scanner::ClassifyComma()
{
    current_token -> SetKind(TK_COMMA);

    cursor++;
}


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
}


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
}


void Scanner::ClassifyLparen()
{
    current_token -> SetKind(TK_LPAREN);

    cursor++;
}


void Scanner::ClassifyRparen()
{
    current_token -> SetKind(TK_RPAREN);

    cursor++;
}


void Scanner::ClassifyLbracket()
{
    current_token -> SetKind(TK_LBRACKET);

    cursor++;
}


void Scanner::ClassifyRbracket()
{
    current_token -> SetKind(TK_RBRACKET);

    cursor++;
}


void Scanner::ClassifyComplement()
{
    current_token -> SetKind(TK_TWIDDLE);

    cursor++;
}


//
// Anything that doesn't fit above.
//
void Scanner::ClassifyBadToken()
{
    // Not the terminating character?
    if (++cursor < &lex -> InputBuffer()[lex -> InputBufferLength()])
    {
         current_token -> SetKind(0);
         current_token -> SetSymbol(control.FindOrInsertName(cursor - 1, 1));

         lex -> ReportMessage(StreamError::BAD_TOKEN,
                              current_token -> Location(),
                              current_token -> Location());
    }
    else
    {
        current_token -> SetKind(TK_EOF);
    }
}


void Scanner::ClassifyQuestion()
{
    current_token -> SetKind(TK_QUESTION);

    cursor++;
}


void Scanner::ClassifyNonAsciiUnicode()
{
    if (Code::IsAlpha(*cursor)) // Some kind of non-ascii unicode letter
        ClassifyId();
    else 
        ClassifyBadToken();
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif
