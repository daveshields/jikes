// $Id: option.cpp,v 1.52 2001/02/24 22:19:24 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "option.h"
#include "javasym.h"
#include "error.h"
#include "case.h"

//FIXME: include stuff
//#include <ctype.h>


// FIXME: this and other # defined things need to go in platform.cpp!
#ifdef HAVE_SYS_CYGWIN_H
#include <sys/cygwin.h>
#endif

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

//
//
//
bool ArgumentExpander::ArgumentExpanded(Tuple<char *> &arguments, char *file_name)
{
    struct stat status;
    FILE *afile = fopen(file_name, "r");
    if (afile && (SystemStat(file_name, &status) == 0))
    {
        char *buffer = new char[status.st_size + 2];
        int file_size = fread(buffer, 1, status.st_size, afile);
        buffer[file_size] = '\n';
        for (int k = 0; k < file_size; k++)
        {
            // FIXME : When \r is replaced by \n, we should not need to check for U_CARRIAGE_RETURN.
            // Skip spaces and line termination characters
            while (buffer[k] == U_SPACE || buffer[k] == U_LINE_FEED || buffer[k] == U_CARRIAGE_RETURN)
                k++;
            // If we are at the end of the file, there must not have been any arguments in the file
            if (k >= file_size)
                break;

            int n;
            for (n = k + 1; ! (buffer[n] == U_SPACE || buffer[n] == U_LINE_FEED || buffer[n] == U_CARRIAGE_RETURN); n++)
                ;
            buffer[n] = U_NULL;
            char *str = new char[n - k + 1];
            strcpy(str, &buffer[k]);
            arguments.Next() = str;
            k = n;
        }
        delete [] buffer;
        fclose(afile);

        return true;
    }

    return false;
}


ArgumentExpander::ArgumentExpander(int argc_, char *argv_[])
{
    Tuple<char *> arguments(8192);
    for (int i = 0; i < argc_; i++)
    {
        char *argument = argv_[i];
        if (argument[0] != '@' || (! ArgumentExpanded(arguments, argument + 1)))
        {
            char *str = new char[strlen(argument) + 1];
            strcpy(str, argument);
            arguments.Next() = str;
        }
    }

    argc = arguments.Length();
    argv = new char*[argc];

    for (int k = 0; k < argc; k++)
        argv[k] = arguments[k];

    return;
}


ArgumentExpander::ArgumentExpander(Tuple<char> &line)
{
    Tuple<char *> arguments(8192);

    int end = 0;
    do
    {
        for (; end < line.Length() && line[end] == U_SPACE; end++)
            ;
        if (end < line.Length())
        {
            int start = end;
            for (end++; end < line.Length() && line[end] != U_SPACE; end++)
                ;
            int length = end - start;
            char *argument = new char[length + 1];
            for (int i = 0, k = start; k < end; i++, k++)
                argument[i] = line[k];
            argument[length] = U_NULL;

            if (argument[0] == '@' && ArgumentExpanded(arguments, argument + 1))
                 delete argument;
            else arguments.Next() = argument;
        }
    } while(end < line.Length());

    argc = arguments.Length();
    argv = new char*[argc];

    for (int k = 0; k < argc; k++)
        argv[k] = arguments[k];

    return;
}


#ifdef WIN32_FILE_SYSTEM
void Option::SaveCurrentDirectoryOnDisk(char c)
{
    if (! current_directory[c])
    {
        char *disk_directory = NULL,
             disk[3] = { c, U_COLON, U_NULL },
             tmp[1];

        if (SetCurrentDirectory(disk))
        {
            DWORD directory_length = GetCurrentDirectory(0, tmp);  // first, get the right size
            disk_directory = new char[directory_length + 1];       // allocate the directory
            DWORD length = GetCurrentDirectory(directory_length, disk_directory);
            if (length <= directory_length)
            {
                for (char *ptr = disk_directory; *ptr; ptr++)
                    *ptr = (*ptr != U_BACKSLASH ? *ptr : (char) U_SLASH); // turn '\' to '/'.
            }
        }

        if (! disk_directory)
        {
            disk_directory = new char[2];
            strcpy(disk_directory, StringConstant::U8S__DO);
        }

        current_directory[Case::ToAsciiLower(c)] = disk_directory;
        current_directory[Case::ToAsciiUpper(c)] = disk_directory;
    }

    return;
}
#endif


Option::Option(ArgumentExpander &arguments) :
                                              first_file_index(arguments.argc),
                                              debug_trap_op(0),
                                              debug_dump_lex(false),
                                              debug_dump_ast(false),
                                              debug_unparse_ast(false),
                                              debug_unparse_ast_debug(false),
                                              debug_dump_class(false),
                                              nocleanup(false),
                                              incremental(false),
                                              makefile(false),
                                              dependence_report(false),
                                              bytecode(true),
                                              full_check(false),
                                              unzip(false),
                                              dump_errors(false),
                                              errors(true),
                                              comments(false),
                                              pedantic(false),
                                              dependence_report_name(NULL)
{
#ifdef WIN32_FILE_SYSTEM
    for (int j = 0; j < 128; j++)
         current_directory[j] = NULL;

    char tmp[1];
    DWORD directory_length = GetCurrentDirectory(0, tmp); // first, get the right size
    char *main_current_directory = new char[directory_length + 1];   // allocate the directory
    DWORD length = GetCurrentDirectory(directory_length, main_current_directory);
    if (length > directory_length)
    {
        delete [] main_current_directory; // FIXME: missing a delete of main_current_directory ???
        main_current_directory = StringConstant::U8S__DO;
        main_disk = 0;
    }
    else
    {
        for (char *ptr = main_current_directory; *ptr; ptr++)
            *ptr = (*ptr != U_BACKSLASH ? *ptr : (char) U_SLASH); // turn '\' to '/'.
        main_disk = main_current_directory[0]; // the first character
        current_directory[Case::ToAsciiLower(main_disk)] = main_current_directory;
        current_directory[Case::ToAsciiUpper(main_disk)] = main_current_directory;
    }
    current_directory[0] = main_current_directory;
#endif

    char * classpath_buffer;
    Tuple<int> filename_index(2048);

    for (int i = 1; i < arguments.argc; i++)
    {
        if (arguments.argv[i][0] == '-')
        {
            if (strcmp(arguments.argv[i],"-classpath") == 0 && ((i + 1) < arguments.argc))
            {
                classpath = arguments.argv[++i];
		
                // Create a copy of the -classpath argument so we can modify
                //   this copy and delete it later in ~JikesOption
                classpath_buffer = new char[strlen(classpath)+1];
                strcpy(classpath_buffer, classpath);
                classpath = classpath_buffer;

#ifdef EBCDIC
                //
                //  Maintain CLASSPATH in ASCII and translate back to EBCDIC when building file name
                //
                for (int k = 0; k < strlen(classpath); k++)
                    classpath[k] = Code::ToASCII(classpath[k]);
#endif
            }
            else if (strcmp(arguments.argv[i], "-depend") == 0 || strcmp(arguments.argv[i], "-Xdepend") == 0)
                 depend = true;
#if defined(HAVE_LIB_ICU_UC) || defined(HAVE_ICONV_H)
            else if (strcmp(arguments.argv[i], "-encoding") == 0 && ((i + 1) < arguments.argc))
            {
                i++;
                encoding = new char[strlen(arguments.argv[i]) + 1];
                strcpy(encoding, arguments.argv[i]);
                if (! Stream::IsSupportedEncoding(encoding))
                {
                    bad_options.Next() =
                        new OptionError(SemanticError::UNSUPPORTED_ENCODING, encoding);
                    encoding = NULL;
                }

                continue;
            }
#endif // defined(HAVE_LIB_ICU_UC) || defined(HAVE_ICONV_H)
            else if (strcmp(arguments.argv[i],"-verbose") == 0)
                 verbose = true;
            else if (strcmp(arguments.argv[i],"-g") == 0)
                 g = true;
            else if (strcmp(arguments.argv[i], "-O") == 0)
                 O = true;
            else if (strcmp(arguments.argv[i],"-deprecation") == 0)
                 deprecation = true;
            else if (strcmp(arguments.argv[i],"-nowrite") == 0)
                 nowrite = true;
            else if (strcmp(arguments.argv[i],"-nowarn") == 0)
                 nowarn = true;
            else if (strcmp(arguments.argv[i],"-Xstdout") == 0)
                 Coutput.StandardOutput();
            else if (strcmp(arguments.argv[i], "-d") == 0 && ((i + 1) < arguments.argc))
            {
                ++i;
#if defined(UNIX_FILE_SYSTEM)
                int length = strlen(arguments.argv[i]);
                directory = new char[length + 1];
                strcpy(directory, arguments.argv[i]);
#elif defined(WIN32_FILE_SYSTEM)
                char disk = (strlen(arguments.argv[i]) >= 2 &&
                            Case::IsAsciiAlpha(arguments.argv[i][0]) &&
                            arguments.argv[i][1] == U_COLON
                                     ? arguments.argv[i][0]
                                     : 0);
                SaveCurrentDirectoryOnDisk(disk);
                if (SetCurrentDirectory(arguments.argv[i]))
                {
                    char tmp[1];
                    DWORD directory_length = GetCurrentDirectory(0, tmp); // first, get the right size
                    directory = new char[directory_length + 1]; // allocate the directory
                    DWORD length = GetCurrentDirectory(directory_length, directory);
                    if (length > directory_length)
                    {
                        delete [] directory;
                        directory = NULL;
                    }
                }

                ResetCurrentDirectoryOnDisk(disk); // reset the current directory on the disk
                SetMainCurrentDirectory();       // reset the real current directory...

                if (! directory)
                    bad_options.Next() = new OptionError(SemanticError::INVALID_DIRECTORY, arguments.argv[i]);
#endif
#ifdef EBCDIC
                //
                // need to translate directory name to ASCII
                //
                for (int k = 0; k < directory_length; k++)
                    directory[k] = Code::ToASCII(directory[k]);
#endif
                if (directory)
                {
                    for (char *ptr = directory; *ptr; ptr++)
                        *ptr = (*ptr != U_BACKSLASH ? *ptr : (char) U_SLASH); // turn '\' to '/'.
                }
            }
            else bad_options.Next() = new OptionError(SemanticError::INVALID_OPTION, arguments.argv[i]);
        }
        else if (arguments.argv[i][0] == '+')
        {
            if (strcmp(arguments.argv[i], "+A") == 0)
                 debug_dump_ast = true;
            else if (strcmp(arguments.argv[i], "+u") == 0)
                 debug_unparse_ast = true;
            else if (strcmp(arguments.argv[i], "+ud") == 0)
            {
                debug_unparse_ast = true;
                debug_unparse_ast_debug = true;
            }
#ifdef EBCDIC
            else if (strcmp(arguments.argv[i], "+ASCII") == 0)
                     ascii = true;
#endif
            else if (strcmp(arguments.argv[i], "+B") == 0)
                 bytecode = false;
            else if (strcmp(arguments.argv[i], "+c") == 0)
                 comments = true;
            else if (strcmp(arguments.argv[i], "+C") == 0)
                 debug_dump_class = true;
            else if (strcmp(arguments.argv[i], "+OLDCSO") == 0)
                 old_classpath_search_order = true;
            else if (strcmp(arguments.argv[i],"+D") == 0)
            {
                 dump_errors = true;
                 errors = false;
            }
            else if (strcmp(arguments.argv[i],"+E") == 0)
            {
                errors = false;
            }
            else if (arguments.argv[i][0] == '+' && arguments.argv[i][1] == 'K')
            {
                char *name = arguments.argv[i] + 2,
                     *image;
                for (image = name; *image && *image != '='; image++)
                    ;

                if (*image != '=')
                    bad_options.Next() = new OptionError(SemanticError::INVALID_K_OPTION, arguments.argv[i]);
                else
                {
                    int key = 0; // assume undefined
                    image++;

                    if (strcmp(image, "boolean") == 0)
                         key = TK_boolean;
                    else if (strcmp(image, "byte") == 0)
                         key = TK_byte;
                    else if (strcmp(image, "char") == 0)
                         key = TK_char;
                    else if (strcmp(image, "short") == 0)
                         key = TK_short;
                    else if (strcmp(image, "int") == 0)
                         key = TK_int;
                    else if (strcmp(image, "long") == 0)
                         key = TK_long;
                    else if (strcmp(image, "float") == 0)
                         key = TK_float;
                    else if (strcmp(image, "double") == 0)
                         key = TK_double;
                    else bad_options.Next() = new OptionError(SemanticError::INVALID_K_TARGET, image);

                    if (key != 0)
                    {
                        int i = keyword_map.NextIndex();
                        keyword_map[i].key = key;
                        keyword_map[i].length = image - name - 1;
                        keyword_map[i].name = new wchar_t[keyword_map[i].length];
                        for (int k = 0; k < keyword_map[i].length; k++)
                            keyword_map[i].name[k] = name[k];
                    }
                }
            }
            else if (strcmp(arguments.argv[i],"+F") == 0)
                 full_check = true;
            else if (strcmp(arguments.argv[i],"+M") == 0)
            {
                 makefile = true;
                 full_check = true;
            }
            else if (strncmp(arguments.argv[i], "+DR=", 4) == 0)
            {
                 makefile = true;
                 dependence_report=true;
                 full_check = true;
		 dependence_report_name =
		     new char[strlen(&arguments.argv[i][4]) + 1];
		 strcpy(dependence_report_name, &arguments.argv[i][4]);
            }
            else if (strcmp(arguments.argv[i], "+O") == 0)
            {
                 debug_trap_op = atoi(arguments.argv[i + 1]);
                 i++;
            }
            else if (strcmp(arguments.argv[i],"+P") == 0)
                 pedantic = true;
            else if (arguments.argv[i][0] == U_PLUS && arguments.argv[i][1] == U_T)
            {
                int tab_size = 0;
                char *image = arguments.argv[i] + 2,
                     *p;
                for (p = image; *p && Code::IsDigit(*p); p++)
                {
                    int digit = *p - U_0;
                    tab_size = tab_size * 10 + digit;
                }

                if (*p)
                     bad_options.Next() = new OptionError(SemanticError::INVALID_TAB_VALUE, image);
                Tab::SetTabSize(tab_size == 0 ? Tab::DEFAULT_TAB_SIZE : tab_size);
            }
            else if (strcmp(arguments.argv[i], "+L") == 0)
                 debug_dump_lex = true;
            else if (strcmp(arguments.argv[i],"+U") == 0)
            {
                 unzip = true;
                 full_check = true;
            }
            else if (strcmp(arguments.argv[i],"++") == 0)
            {
                 incremental = true;
                 full_check = true;
            }
            else if (strcmp(arguments.argv[i], "+Z") == 0)
                 zero_defect = true;
            else bad_options.Next() = new OptionError(SemanticError::INVALID_OPTION, arguments.argv[i]);
        }
        else filename_index.Next() = i;
    }

    if (! classpath)
    {
        classpath = getenv("JIKESPATH");
        if (! classpath)
            classpath = getenv("CLASSPATH");

        if (classpath)
        {
            /* Create a copy of the classpath string we can modify
               this copy without worry that it will effect the env array */
            classpath_buffer = new char[strlen(classpath)+1];
            strcpy(classpath_buffer, classpath);
            classpath = classpath_buffer;

#ifdef EBCDIC
            //
            //  Maintain CLASSPATH in ASCII and translate back to EBCDIC when building file name
            //
            for (int k = 0; k < strlen(classpath); k++)
                classpath[k] = Code::ToASCII(classpath[k]);
#endif
            while (isspace(*classpath))
                classpath++;

            if (*classpath == U_NULL) {
                delete [] classpath;
                classpath = NULL;
            }
        }

        if (! classpath)
        {
            classpath = new char[2];
            classpath[0] = '.';
            classpath[1] = U_NULL;
        }
    }
    
    // If we need to do a cygwin CLASSPATH conversion do it after the env is checked
    // so that it will work for a -classpath argument or a CLASSPATH env var.

#ifdef HAVE_CYGWIN_WIN32_TO_POSIX_PATH_LIST
    // Under Cygwin, we convert a windows style path into a unix
    // style path. A path like "C:\Cygwin\tmp;C:\Windows" is converted
    // into "/tmp:/cygdrive/c/Windows" (assuming C:\Cygwin is cygroot).
    // We can then parse it using the unix path seperator char ':'
    classpath_buffer = new char[cygwin_win32_to_posix_path_list_buf_size(classpath)];
    cygwin_win32_to_posix_path_list(classpath, classpath_buffer);
    delete [] classpath;
    classpath = classpath_buffer;
#endif

    //
    // Initially, first_file_index is set to argc. Since the array filename_index
    // contains the indices of all the input files in arguments in reverse order,
    // we reverse the order here again to get the original list...
    //
    for (int k = filename_index.Length() - 1; k >= 0; k--)
    {
        first_file_index--;

        int i = filename_index[k];
        char *temp = arguments.argv[i];
        arguments.argv[i] = arguments.argv[first_file_index];
        arguments.argv[first_file_index] = temp;
    }

    //
    // If the traditional batch output form for reporting errors is requested,
    // then wide character expansion is turned on.
    //
    if (errors && (! dump_errors))
        Coutput.SetExpandWchar();

    return;
}


Option::~Option()
{
    for (int i = 0; i < bad_options.Length(); i++)
        delete bad_options[i];

    delete [] dependence_report_name;

#ifdef WIN32_FILE_SYSTEM
    for (char c = 'a'; c <= 'z'; c++)
        delete [] current_directory[c];
#endif

    return;
}

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

