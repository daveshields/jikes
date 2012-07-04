// $Id: option.h,v 1.32 2001/09/14 05:31:34 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef option_INCLUDED
#define option_INCLUDED

#include "platform.h"
#include "code.h"
#include "tuple.h"
#include "jikesapi.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class ArgumentExpander
{
public:

    int argc;
    char **argv;

    ArgumentExpander(int, char **);

    ArgumentExpander(Tuple<char> &);

    ~ArgumentExpander()
    {
        for (int i = 0; i < argc; i++)
            delete [] argv[i];
        delete [] argv;
    }

    bool ExpandAtFileArgument(Tuple<char *> & arguments, char * file_name);
};


class KeywordMap
{
public:
    wchar_t *name;
    int length,
        key;
};


class OptionError
{
public:
    int kind;
    wchar_t *name;

    OptionError(int kind_, char *str) : kind(kind_)
    {
        int length = strlen(str);
        name = new wchar_t[length + 1];
        for (int i = 0; i < length; i++)
            name[i] = str[i];
        name[length] = U_NULL;

        return;
    }

    ~OptionError() { delete [] name; }
};

class Ostream;

class Option: public JikesOption
 {

#ifdef WIN32_FILE_SYSTEM

    char main_disk, *current_directory[128];

public:

    bool BadMainDisk() { return main_disk == 0; }

    void ResetCurrentDirectoryOnDisk(char d)
    {
        if (d != 0)
        {
            assert(current_directory[d]);
            SetCurrentDirectory(current_directory[d]);
        }
    }
    void SetMainCurrentDirectory()
    {
        SetCurrentDirectory(current_directory[main_disk]);
    }
    
    char *GetMainCurrentDirectory()
    {
        return current_directory[main_disk];
    }

    void SaveCurrentDirectoryOnDisk(char c);

#endif

public:
         
    Tuple<KeywordMap> keyword_map;
    Tuple<OptionError *> bad_options;

    int first_file_index;

    int debug_trap_op;

    bool debug_dump_lex,
         debug_dump_ast,
         debug_unparse_ast,
         debug_unparse_ast_debug,
         debug_dump_class,
         nocleanup,
         incremental,
         makefile,
         dependence_report,
         bytecode,
         full_check,
         unzip,
         dump_errors,
         errors,
         comments,
         pedantic;

    char *dependence_report_name;

    Option(ArgumentExpander &);

    ~Option();
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // option_INCLUDED

