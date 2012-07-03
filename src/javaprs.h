// $Id: javaprs.h,v 1.15 2000/07/25 11:32:33 mdejong Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef javaprs_INCLUDED
#define javaprs_INCLUDED

#define SCOPE_REPAIR
#define DEFERRED_RECOVERY
#define FULL_DIAGNOSIS
#define SPACE_TABLES

#ifdef	HAVE_NAMESPACES
namespace Jikes {	// Open namespace Jikes block
#endif

class LexStream;

class javaprs_table
{
public:
    static int original_state(int state) { return -base_check[state]; }
    static int asi(int state) { return asb[original_state(state)]; }
    static int nasi(int state) { return nasb[original_state(state)]; }
    static int in_symbol(int state) { return in_symb[original_state(state)]; }

    static const unsigned char  rhs[];
    static const   signed short check_table[];
    static const   signed short *base_check;
    static const unsigned short lhs[];
    static const unsigned short *base_action;
    static const unsigned char  term_check[];
    static const unsigned short term_action[];

    static const unsigned short asb[];
    static const unsigned char  asr[];
    static const unsigned short nasb[];
    static const unsigned short nasr[];
    static const unsigned short name_start[];
    static const unsigned char  name_length[];
    static const          char  string_buffer[];
    static const unsigned short terminal_index[];
    static const unsigned short non_terminal_index[];
    static const unsigned short scope_prefix[];
    static const unsigned short scope_suffix[];
    static const unsigned short scope_lhs[];
    static const unsigned char  scope_la[];
    static const unsigned char  scope_state_set[];
    static const unsigned short scope_rhs[];
    static const unsigned short scope_state[];
    static const unsigned short in_symb[];

    static int nt_action(int state, int sym)
    {
        return base_action[state + sym];
    }

    static int t_action(int act, int sym, LexStream *stream)
    {
        act = base_action[act];
        int i = act + sym;

        act = term_action[term_check[i] == sym ? i : act];

        if (act > LA_STATE_OFFSET)
        {
            for (TokenObject tok = stream -> Peek();
                 ;
                 tok = stream -> Next(tok))
            {
               act -= LA_STATE_OFFSET;
               sym = stream -> Kind(tok);
               i = act + sym;
               act = term_action[term_check[i] == sym ? i : act];
               if (act <= LA_STATE_OFFSET)
                   break;
            } 
        }

        return act;
    }
};

#ifdef	HAVE_NAMESPACES
}			// Close namespace Jikes block
#endif

#endif /* javaprs_INCLUDED */
