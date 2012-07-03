// $Id: scanner.h,v 1.4 1999/06/22 19:17:31 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef scanner_INCLUDED
#define scanner_INCLUDED

#include "config.h"
#include <limits.h>
#include <iostream.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#include "code.h"
#include "javadef.h"
#include "javasym.h"
#include "stream.h"

class Control;
class FileSymbol;

//
// The Scanner object
//
class Scanner
{
public:

    Scanner(Control &);

    ~Scanner() { }

    void SetUp(FileSymbol *);
    void Scan(FileSymbol *);

private:
    Control &control;

    LexStream* lex;
    wchar_t *cursor;

    LexStream::Token *current_token;
    LexStream::TokenIndex current_token_index;

    void Initialize(FileSymbol *);
    void Scan();

    static int (*scan_keyword[13]) (wchar_t *p1);
    static int ScanKeyword0(wchar_t *p1);
    static int ScanKeyword2(wchar_t *p1);
    static int ScanKeyword3(wchar_t *p1);
    static int ScanKeyword4(wchar_t *p1);
    static int ScanKeyword5(wchar_t *p1);
    static int ScanKeyword6(wchar_t *p1);
    static int ScanKeyword7(wchar_t *p1);
    static int ScanKeyword8(wchar_t *p1);
    static int ScanKeyword9(wchar_t *p1);
    static int ScanKeyword10(wchar_t *p1);
    static int ScanKeyword12(wchar_t *p1);

    inline void CheckOctalLiteral(wchar_t *, wchar_t *);
    inline void SkipSpaces();
    void ScanSlashComment();
    void ScanStarComment();

    class BraceStack
    {
    public:
        void Push(LexStream::TokenIndex brace) { table.Next() = brace; }
        void Pop()                             { if (table.Length() > 0) table.Reset(table.Length() - 1); }
        int  Size()                            { return table.Length(); }
        LexStream::TokenIndex Top()            { return (table.Length() > 0 ? table[table.Length() - 1] : 0); }

    private:
        Tuple<LexStream::TokenIndex> table;
    } brace_stack;

    void (Scanner::*classify_token[128 + 1])();

    void ClassifyCharLiteral();
    void ClassifyStringLiteral();
    void ClassifyIdOrKeyword();
    void ClassifyId();
    void ClassifyNumericLiteral();
    void ClassifyColon();
    void ClassifyPlus();
    void ClassifyMinus();
    void ClassifyStar();
    void ClassifyDocComment();
    void ClassifySlash();
    void ClassifyLess();
    void ClassifyGreater();
    void ClassifyAnd();
    void ClassifyOr();
    void ClassifyXor();
    void ClassifyNot();
    void ClassifyEqual();
    void ClassifyMod();
    void ClassifyPeriod();
    void ClassifySemicolon();
    void ClassifyComma();
    void ClassifyLbrace();
    void ClassifyRbrace();
    void ClassifyLparen();
    void ClassifyRparen();
    void ClassifyLbracket();
    void ClassifyRbracket();
    void ClassifyComplement();
    void ClassifyPound();
    void ClassifyBadToken();
    void ClassifyQuestion();
    void ClassifyEof();

    void ClassifyNonAsciiUnicode();
};

#endif



