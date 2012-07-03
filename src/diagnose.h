// $Id: diagnose.h,v 1.6 1999/11/03 00:46:30 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef diagnose_INCLUDED
#define diagnose_INCLUDED

#include "config.h"
#include "parser.h"

struct PrimaryRepairInfo
{
    int code,
        distance,
        buffer_position,
        misspell_index,
        symbol;
};

struct RepairCandidate
{
    int symbol;
    Location location;
};

struct StateInfo
{
    int state,
        next;
};


class ParseError : public javaprs_table
{
public:
    void Report(int msg_level, int msg_code, int name_index,
                LexStream::TokenIndex left_token, LexStream::TokenIndex right_token,
                int scope_name_index = 0);

    void SortMessages();

    ParseError(Control &control_, LexStream *lex_stream_) : control(control_),
                                                            lex_stream(lex_stream_),
                                                            errors(256)
    {}
    void PrintMessages();

private:

    Control &control;
    LexStream *lex_stream;

    struct ErrorInfo
    {
        LexStream::TokenIndex left_token,
                              right_token;
        int                   name_index;
        int                   right_string_length;
        int                   num;
        unsigned char         msg_level;
        unsigned char         msg_code;
        unsigned              scope_name_index;
    };

    Tuple<ErrorInfo> errors;

    void PrintLargeMessage(int k);
    void PrintMessage(int k);
    void PrintPrimaryMessage(int k);
    void PrintSecondaryMessage(int k);
};


class DiagnoseParser : public Parser
{
public:

    DiagnoseParser(Control &control_, LexStream *lex_stream_) : next_stack(NULL),
                                                                prev_stack(NULL),
                                                                scope_index(NULL),
                                                                scope_position(NULL),
                                                                state_pool(256),
                                                                error(control_, lex_stream_)
    {
        lex_stream = lex_stream_;
        memset(list, 0, NUM_SYMBOLS * sizeof(int));
        DiagnoseParse();

        return;
    }

    ~DiagnoseParser()
    {
        delete [] next_stack;
        delete [] prev_stack;
        delete [] scope_index;
        delete [] scope_position;
    }

private:

    int next_stack_top,
        *next_stack,

        prev_stack_top,
        *prev_stack,

        scope_stack_top,
        *scope_index,
        *scope_position;

    int list[NUM_SYMBOLS + 1];

    enum { NIL = -1 };
    Tuple<StateInfo> state_pool;
    int *state_seen; // this variable is managed entirely by the function "scope_trial"

    ParseError error;

    void DiagnoseParse();

    void ReallocateStacks();

    RepairCandidate ErrorRecovery(TokenObject error_token);
    RepairCandidate PrimaryPhase(TokenObject error_token);
    bool MergeCandidate(int state, int buffer_position);
    PrimaryRepairInfo CheckPrimaryDistance(int stack[],
                                           int stack_top,
                                           PrimaryRepairInfo repair);
    RepairCandidate PrimaryDiagnosis(PrimaryRepairInfo repair);
    int GetTermIndex(int stack[], int stack_top,
                     int tok, int buffer_position);
    int GetNtermIndex(int start, int sym, int buffer_position);
    int Misspell(int sym, TokenObject tok);
    RepairCandidate SecondaryPhase(TokenObject error_token);
    SecondaryRepairInfo MisplacementRecovery
             (int stack[],
              int stack_top,
              int last_index,
              SecondaryRepairInfo misplaced, bool stack_flag);
    SecondaryRepairInfo SecondaryRecovery
             (int stack[],
              int stack_top,
              int last_index,
              SecondaryRepairInfo repair, bool stack_flag);
    void SecondaryDiagnosis(SecondaryRepairInfo repair);

    void RepairParse(TokenObject);

    PrimaryRepairInfo ScopeTrial(int stack[], int stack_top,
                                 PrimaryRepairInfo repair);
    void ScopeTrialCheck(int stack[], int stack_top,
                         PrimaryRepairInfo& repair, int indx);
    bool SecondaryCheck(int stack[], int stack_top,
                        int buffer_position, int distance);
};

#endif
