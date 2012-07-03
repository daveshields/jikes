// $Id: control.cpp,v 1.27 1999/11/03 14:49:13 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include <sys/stat.h>
#include "control.h"
#include "scanner.h"
#include "parser.h"
#include "semantic.h"
#include "error.h"
#include "bytecode.h"
#include "case.h"


Control::Control(ArgumentExpander &arguments, Option &option_) : return_code(0),
                                                                 option(option_),
                                                                 dot_classpath_index(0),
                                                                 system_table(NULL),
                                                                 system_semantic(NULL),
                                                                 semantic(1024),
                                                                 needs_body_work(1024),
                                                                 type_trash_bin(1024),
                                                                 input_java_file_set(1021),
                                                                 input_class_file_set(1021),
                                                                 expired_file_set(),
                                                                 recompilation_file_set(1021),
                                                                 int_pool(&bad_value),
                                                                 long_pool(&bad_value),
                                                                 float_pool(&bad_value),
                                                                 double_pool(&bad_value),
                                                                 Utf8_pool(&bad_value),
#ifdef TEST
                                                                 input_files_processed(0),
                                                                 class_files_read(0),
                                                                 class_files_written(0),
                                                                 line_count(0),
#endif
                                                                 Serializable_type(NULL),
                                                                 Object_type(NULL),
                                                                 Cloneable_type(NULL),
                                                                 String_type(NULL),
                                                                 Void_type(NULL),
                                                                 Boolean_type(NULL),
                                                                 Byte_type(NULL),
                                                                 Short_type(NULL),
                                                                 Character_type(NULL),
                                                                 Integer_type(NULL),
                                                                 Long_type(NULL),
                                                                 Float_type(NULL),
                                                                 Double_type(NULL),
                                                                 Class_type(NULL),
                                                                 Throwable_type(NULL),
                                                                 Exception_type(NULL),
                                                                 RuntimeException_type(NULL),
                                                                 ClassNotFoundException_type(NULL),
                                                                 Error_type(NULL),
                                                                 NoClassDefFoundError_type(NULL),
                                                                 StringBuffer_type(NULL),

                                                                 Class_forName_method(NULL),

                                                                 Throwable_getMessage_method(NULL),

                                                                 NoClassDefFoundError_InitWithString_method(NULL),

                                                                 StringBuffer_Init_method(NULL),
                                                                 StringBuffer_InitWithString_method(NULL),
                                                                 StringBuffer_toString_method(NULL),
                                                                 StringBuffer_append_char_array_method(NULL),
                                                                 StringBuffer_append_char_method(NULL),
                                                                 StringBuffer_append_boolean_method(NULL),
                                                                 StringBuffer_append_int_method(NULL),
                                                                 StringBuffer_append_long_method(NULL),
                                                                 StringBuffer_append_float_method(NULL),
                                                                 StringBuffer_append_double_method(NULL),
                                                                 StringBuffer_append_string_method(NULL),
                                                                 StringBuffer_append_object_method(NULL)
{
    ProcessGlobals();

    ProcessUnnamedPackage();

    ProcessPath();

    ProcessSystemInformation();

    //
    // Instantiate a scanner and a parser and initialize the static members for the semantic processors.
    //
    scanner = new Scanner(*this);
    parser = new Parser();
    SemanticError::StaticInitializer();

    //
    // Process all file names specified in command line
    //
    ProcessNewInputFiles(input_java_file_set, arguments, option.first_file_index);

    //
    // For each input file, copy it into the input_files array and process its package declaration.
    //
    StoragePool *ast_pool = new StoragePool(64); // how much space do we need? estimate 64 tokens.
    FileSymbol **input_files = new FileSymbol*[input_java_file_set.Size() + 1];
    int num_files = 0;
    for (FileSymbol *file_symbol = (FileSymbol *) input_java_file_set.FirstElement();
                     file_symbol;
                     file_symbol = (FileSymbol *) input_java_file_set.NextElement())
    {
        input_files[num_files++] = file_symbol;
        scanner -> Scan(file_symbol);
        if (file_symbol -> lex_stream) // did we have a successful scan!
        {
            AstPackageDeclaration *package_declaration = parser -> PackageHeaderParse(file_symbol -> lex_stream, ast_pool);
            ProcessPackageDeclaration(file_symbol, package_declaration);
            ast_pool -> Reset();
        }
    }

    //
    //
    //
    FileSymbol *main_file_clone;
    if (num_files > 0)
        main_file_clone = input_files[0] -> Clone();
    else
    {
        //
        // Some name, any name !!! We use dot_name_symbol as a bad file name because
        // no file can be named ".".
        //
        FileSymbol *file_symbol = classpath[dot_classpath_index] -> RootDirectory() -> InsertFileSymbol(dot_name_symbol);
        file_symbol -> directory_symbol = classpath[dot_classpath_index] -> RootDirectory();
        file_symbol -> SetJava();

        main_file_clone = file_symbol -> Clone();
    }

    main_file_clone -> semantic = new Semantic(*this, main_file_clone);
    system_semantic = main_file_clone -> semantic;
    scanner -> SetUp(main_file_clone);

#ifdef WIN32_FILE_SYSTEM
    //
    //
    //
    if (option.BadMainDisk())
    {
        system_semantic -> ReportSemError(SemanticError::NO_CURRENT_DIRECTORY,
                                          0,
                                          0);
    }
#endif

    //
    //
    //
    for (int o = 0; o < option.bad_options.Length(); o++)
    {
        system_semantic -> ReportSemError((SemanticError::SemanticErrorKind) option.bad_options[o] -> kind,
                                          0,
                                          0,
                                          option.bad_options[o] -> name);
    }

    if (java_util_package -> directory.Length() == 0)
    {
        system_semantic -> ReportSemError(SemanticError::PACKAGE_NOT_FOUND,
                                          0,
                                          0,
                                          StringConstant::US_java_SL_util);
    }

    //
    //
    //
    for (int l = 0; l < bad_zip_filenames.Length(); l++)
    {
        wchar_t *tail = &bad_zip_filenames[l][wcslen(bad_zip_filenames[l]) - 3];

        system_semantic -> ReportSemError(Case::StringSegmentEqual(tail, StringConstant::US__zip, 3) ||
                                          Case::StringSegmentEqual(tail, StringConstant::US__jar, 3)
                                               ? SemanticError::CANNOT_OPEN_ZIP_FILE
                                               : SemanticError::CANNOT_OPEN_PATH_DIRECTORY,
                                          0,
                                          0,
                                          bad_zip_filenames[l]);
    }

    //
    //
    //
    if (system_package -> directory.Length() == 0)
    {
        system_semantic -> ReportSemError(SemanticError::PACKAGE_NOT_FOUND,
                                          0,
                                          0,
                                          StringConstant::US_java_SL_lang);
    }

    //
    // When the -d option is specified, create the relevant
    // directories if they don't already exist.
    //
    if (option.directory)
    {
        if (! ::SystemIsDirectory(option.directory))
        {
            for (char *ptr = option.directory; *ptr; ptr++)
            {
                char delimiter = *ptr;
                if (delimiter == U_SLASH)
                {
                    *ptr = U_NULL;

                    if (! ::SystemIsDirectory(option.directory))
                        ::SystemMkdir(option.directory);

                    *ptr = delimiter;
                }
            }

            ::SystemMkdir(option.directory);

            if (! ::SystemIsDirectory(option.directory))
            {
                int length = strlen(option.directory);
                wchar_t *name = new wchar_t[length + 1];
                for (int i = 0; i < length; i++)
                    name[i] = option.directory[i];
                name[length] = U_NULL;
                system_semantic -> ReportSemError(SemanticError::CANNOT_OPEN_DIRECTORY,
                                                  0,
                                                  0,
                                                  name);
                delete [] name;
            }
        }
    }

    //
    //
    //
    for (int m = 0; m < bad_input_filenames.Length(); m++)
    {
        system_semantic -> ReportSemError(SemanticError::BAD_INPUT_FILE,
                                          0,
                                          0,
                                          bad_input_filenames[m]);
    }

    //
    //
    //
    for (int n = 0; n < unreadable_input_filenames.Length(); n++)
    {
        system_semantic -> ReportSemError(SemanticError::UNREADABLE_INPUT_FILE,
                                          0,
                                          0,
                                          unreadable_input_filenames[n]);
    }

    //
    //
    //
    if (system_semantic -> NumErrors() > 0)
    {
        system_semantic -> PrintMessages();
        return_code = system_semantic -> return_code;
    }
    else
    {
        system_semantic -> PrintMessages(); // there might be some warnings we want to print...

        //
        //
        //
        input_java_file_set.SetEmpty();
        for (int i = 0; i < num_files; i++)
        {
            FileSymbol *file_symbol = input_files[i];
            if (! input_java_file_set.IsElement(file_symbol))
                ProcessFile(file_symbol);
        }

        //
        // Clean up all the files that have just been compiled in this new batch.
        //
        for (FileSymbol *file_symbol = (FileSymbol *) input_java_file_set.FirstElement();
                         file_symbol;
                         file_symbol = (FileSymbol *) input_java_file_set.NextElement())
        {
            CleanUp(file_symbol);
        }

        //
        // If more messages were added to system_semantic, print them...
        //
        system_semantic -> PrintMessages();
        if (system_semantic -> return_code > 0 || bad_input_filenames.Length() > 0 || unreadable_input_filenames.Length() > 0)
            return_code = 1;

        //
        // If the incremental flag is on, check to see if the user wants us to recompile.
        //
        if (option.incremental)
        {
            option.depend = false; // The depend flag should only be in effect in the first pass

            for (bool recompile = IncrementalRecompilation(); recompile; recompile = IncrementalRecompilation())
            {
                return_code = 0; // reset the return code as we may compile clean in this pass
                system_semantic -> return_code = 0;

                //
                //
                //
                for (int m = 0; m < bad_input_filenames.Length(); m++)
                {
                    system_semantic -> ReportSemError(SemanticError::BAD_INPUT_FILE,
                                                      0,
                                                      0,
                                                      bad_input_filenames[m]);
                }

                //
                //
                //
                for (int n = 0; n < unreadable_input_filenames.Length(); n++)
                {
                    system_semantic -> ReportSemError(SemanticError::UNREADABLE_INPUT_FILE,
                                                      0,
                                                      0,
                                                      unreadable_input_filenames[n]);
                }

                FileSymbol *file_symbol;

                num_files = 0;
                delete [] input_files; // delete previous copy
                input_files = new FileSymbol*[recompilation_file_set.Size()];
                for (file_symbol = (FileSymbol *) recompilation_file_set.FirstElement();
                     file_symbol;
                     file_symbol = (FileSymbol *) recompilation_file_set.NextElement())
                {
                    input_java_file_set.RemoveElement(file_symbol);
                    input_files[num_files++] = file_symbol;

                    LexStream *lex_stream = file_symbol -> lex_stream;
                    if (lex_stream)
                    {
                        AstPackageDeclaration *package_declaration = parser -> PackageHeaderParse(lex_stream, ast_pool);
                        ProcessPackageDeclaration(file_symbol, package_declaration);
                        ast_pool -> Reset();
                    }
                }

                //
                // If a file has been erased, remove it from the input file set.
                //
                for (file_symbol = (FileSymbol *) expired_file_set.FirstElement();
                     file_symbol;
                     file_symbol = (FileSymbol *) expired_file_set.NextElement())
                {
                    input_java_file_set.RemoveElement(file_symbol);
                }

                //
                // Reset the global objects before recompiling this new batch.
                //
                expired_file_set.SetEmpty();
                recompilation_file_set.SetEmpty();
                type_trash_bin.Reset();

                //
                // For each file that should be recompiled, process it if it has not
                // already been dragged in by dependence.
                //
                for (int k = 0; k < num_files; k++)
                {
                    FileSymbol *file_symbol = input_files[k];
                    if (! input_java_file_set.IsElement(file_symbol))
                        ProcessFile(file_symbol);
                }

                //
                // Clean up all the files that have just been compiled in this new batch.
                //
                for (file_symbol = (FileSymbol *) input_java_file_set.FirstElement();
                     file_symbol;
                     file_symbol = (FileSymbol *) input_java_file_set.NextElement())
                {
                    // delete file_symbol
                    CleanUp(file_symbol);
                }

                //
                // If any system error or warning was detected, print it...
                //
                system_semantic -> PrintMessages();
                if (system_semantic -> return_code > 0 || bad_input_filenames.Length() > 0 || unreadable_input_filenames.Length() > 0)
                    return_code = 1;
            }
        }

        //
        // Are we supposed to generate Makefiles?
        //
        if (option.makefile)
        {
            if (option.dependence_report)
            {
                FILE *outfile = ::SystemFopen(option.dependence_report_name, "w");
                if (outfile == NULL)
                    Coutput << "*** Cannot open file " << option.dependence_report_name << "\n";
                else
                {
                    SymbolSet types_in_new_files;

                    for (FileSymbol *file_symbol = (FileSymbol *) input_java_file_set.FirstElement();
                                     file_symbol;
                                     file_symbol = (FileSymbol *) input_java_file_set.NextElement())
                    {
                        char *java_name = file_symbol -> FileName();

                        for (int j = 0; j < file_symbol -> types.Length(); j++)
                        {
                            TypeSymbol *type = file_symbol -> types[j];
#ifdef EBCDIC
                            for (char *p = java_name; *p; p++)
                                 fprintf(outfile, "%c", Code::ToEBCDIC(*p));
                            fprintf(outfile, " : ");
                            for (char *q = type -> SignatureString(); *q; q++)
                                 fprintf(outfile, "%c", Code::ToEBCDIC(*q));
                            fprintf(outfile, "\n");
#else
                            fprintf(outfile, "%s : %s\n", java_name, type -> SignatureString());
#endif

                            //
                            //
                            //
                            for (TypeSymbol *static_parent = (TypeSymbol *) type -> static_parents -> FirstElement();
                                             static_parent;
                                             static_parent = (TypeSymbol *) type -> static_parents -> NextElement())
                            {
                                if (! type -> parents -> IsElement(static_parent)) // Only a static reference to static_parent?
                                {
#ifdef EBCDIC
                                    fprintf(outfile, "   !");
                                    for (char *q = static_parent -> SignatureString(); *q; q++)
                                         fprintf(outfile, "%c", Code::ToEBCDIC(*q));
                                    fprintf(outfile, "\n");
#else
                                    fprintf(outfile, "   !%s\n", static_parent -> SignatureString());
#endif

                                    //
                                    // If the type is contained in a type that is not one of the input files, save it
                                    //
                                    if (static_parent -> file_symbol -> IsClass())
                                        types_in_new_files.AddElement(static_parent);
                                }
                            }

                            //
                            //
                            //
                            for (TypeSymbol *parent = (TypeSymbol *) type -> parents -> FirstElement();
                                             parent;
                                             parent = (TypeSymbol *) type -> parents -> NextElement())
                            {
#ifdef EBCDIC
                                fprintf(outfile, "    ");
                                for (char *q = parent -> SignatureString(); *q; q++)
                                     fprintf(outfile, "%c", Code::ToEBCDIC(*q));
                                fprintf(outfile, "\n");
#else
                                fprintf(outfile, "    %s\n", parent -> SignatureString());
#endif

                                //
                                // If the type is contained in a type that is not one of the input files, save it
                                //
                                if (parent -> file_symbol -> IsClass())
                                    types_in_new_files.AddElement(parent);
                            }
                        }
                    }

                    //
                    // Print the list of class files that are referenced.
                    //
                    for (TypeSymbol *type = (TypeSymbol *) types_in_new_files.FirstElement();
                                     type;
                                     type = (TypeSymbol *) types_in_new_files.NextElement())
                    {
                        char *class_name = type -> file_symbol -> FileName();
#ifdef EBCDIC
                        for (char *p = class_name; *p; p++)
                             fprintf(outfile, "%c", Code::ToEBCDIC(*p));
                        fprintf(outfile, " : ");
                        for (char *q = type -> SignatureString(); *q; q++)
                             fprintf(outfile, "%c", Code::ToEBCDIC(*q));
                        fprintf(outfile, "\n");
#else
                        fprintf(outfile, "%s : %s\n", class_name, type -> SignatureString());
#endif
                    }

                    fclose(outfile);
                }
            }
            else
            {
                SymbolSet *candidates = new SymbolSet(input_java_file_set.Size() + input_class_file_set.Size());
                *candidates = input_java_file_set;
                candidates -> Union(input_class_file_set);

                TypeDependenceChecker *dependence_checker = new TypeDependenceChecker((Control *) this, *candidates, type_trash_bin);
                dependence_checker -> PartialOrder();
                dependence_checker -> OutputDependences();
                delete dependence_checker;

                delete candidates;
            }
        }
    }

    delete ast_pool;

    delete main_file_clone; // delete the clone of the main source file...

#ifdef NO_LEAKS
    delete [] input_files;
#endif

    return;
}


Control::~Control()
{
#ifdef NO_LEAKS
    for (int i = 0; i < bad_zip_filenames.Length(); i++)
        delete [] bad_zip_filenames[i];

    for (int j = 0; j < bad_input_filenames.Length(); j++)
        delete [] bad_input_filenames[j];

    for (int k = 0; k < unreadable_input_filenames.Length(); k++)
        delete [] unreadable_input_filenames[k];

    for (int l = 0; l < system_directories.Length(); l++)
        delete system_directories[l];

    delete scanner;
    delete parser;
    delete system_semantic;
#endif

#ifdef TEST
    if (option.debug_dump_lex || option.debug_dump_ast || option.debug_unparse_ast)
    {
        Coutput << line_count << " source lines read\n\n"
                << class_files_read << " \".class\" files read\n"
                << class_files_written << " \".class\" files written\n"
                << input_files_processed << " \".java\" files processed\n";
    }
#endif
}


PackageSymbol *Control::ProcessPackage(wchar_t *name)
{
    int name_length = wcslen(name);
    wchar_t *package_name = new wchar_t[name_length];
    int length;
    for (length = 0; length < name_length && name[length] != U_SLASH; length++)
         package_name[length] = name[length];
    NameSymbol *name_symbol = FindOrInsertName(package_name, length);

    PackageSymbol *package_symbol = external_table.FindPackageSymbol(name_symbol);
    if (! package_symbol)
    {
        package_symbol = external_table.InsertPackageSymbol(name_symbol, NULL);
        FindPathsToDirectory(package_symbol);
    }

    while(length < name_length)
    {
        int start = ++length;
        for (int i = 0; length < name_length && name[length] != U_SLASH; i++, length++)
             package_name[i] = name[length];
        name_symbol = FindOrInsertName(package_name, length - start);
        PackageSymbol *subpackage_symbol = package_symbol -> FindPackageSymbol(name_symbol);
        if (! subpackage_symbol)
        {
            subpackage_symbol = package_symbol -> InsertPackageSymbol(name_symbol);
            FindPathsToDirectory(subpackage_symbol);
        }
        package_symbol = subpackage_symbol;
    }

    delete [] package_name;

    return package_symbol;
}


//
// When searching for a subdirectory in a zipped file, it must already be present in the
// hierarchy.
//
DirectorySymbol *Control::FindSubdirectory(PathSymbol *path_symbol, wchar_t *name, int name_length)
{
    wchar_t *directory_name = new wchar_t[name_length + 1];

    DirectorySymbol *directory_symbol = path_symbol -> RootDirectory();
    for (int start = 0, end; directory_symbol && start < name_length; start = end + 1)
    {
        end = start;
        for (int i = 0; end < name_length && name[end] != U_SLASH; i++, end++)
             directory_name[i] = name[end];
        NameSymbol *name_symbol = FindOrInsertName(directory_name, end - start);
        directory_symbol = directory_symbol -> FindDirectorySymbol(name_symbol);
    }

    delete [] directory_name;

    return directory_symbol;
}


//
// When searching for a directory in the system, if it is not already present in the hierarchy
// insert it and attempt to read it from the system...
//
#ifdef UNIX_FILE_SYSTEM
    DirectorySymbol *Control::ProcessSubdirectories(wchar_t *source_name, int source_name_length)
    {
        int name_length = (source_name_length < 0 ? 0 : source_name_length);
        char *input_name = new char[name_length + 1];
        for (int i = 0; i < name_length; i++)
            input_name[i] = source_name[i];
        input_name[name_length] = U_NULL;

        DirectorySymbol *directory_symbol = NULL;
        struct stat status;
        if ((::SystemStat(input_name, &status) == 0) && (status.st_mode & STAT_S_IFDIR))
            directory_symbol = system_table -> FindDirectorySymbol(status.st_dev, status.st_ino);

        if (! directory_symbol)
        {
            if (input_name[0] == U_SLASH) // file name starts with '/'
            {
                directory_symbol = new DirectorySymbol(FindOrInsertName(source_name, name_length), classpath[dot_classpath_index]);
                directory_symbol -> ReadDirectory();
                system_directories.Next() = directory_symbol;
                system_table -> InsertDirectorySymbol(status.st_dev, status.st_ino, directory_symbol);
            }
            else
            {
                wchar_t *name = new wchar_t[name_length + 1];
                for (int i = 0; i < name_length; i++)
                    name[i] = source_name[i];
                name[name_length] = U_NULL;

                directory_symbol = classpath[dot_classpath_index] -> RootDirectory(); // start at the dot directory.

                //
                //
                //
                wchar_t *directory_name = new wchar_t[name_length];
                int end = 0;
                for (int start = end; start < name_length; start = end)
                {
                    int length;
                    for (length = 0; end < name_length && name[end] != U_SLASH; length++, end++)
                        directory_name[length] = name[end];

                    if (length != 1 || directory_name[0] != U_DOT) // Not the current directory
                    {
                        if (length == 2 && directory_name[0] == U_DOT && directory_name[1] == U_DOT) // keep the current directory
                        {
                            if (directory_symbol -> Identity() == dot_name_symbol ||
                                directory_symbol -> Identity() == dot_dot_name_symbol)
                            {
                                DirectorySymbol *subdirectory_symbol = directory_symbol -> FindDirectorySymbol(dot_dot_name_symbol);
                                if (! subdirectory_symbol)
                                    subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(dot_dot_name_symbol);
                                directory_symbol = subdirectory_symbol;
                            }
                            else directory_symbol = directory_symbol -> owner -> DirectoryCast();
                        }
                        else
                        {
                            NameSymbol *name_symbol = FindOrInsertName(directory_name, length);
                            DirectorySymbol *subdirectory_symbol = directory_symbol -> FindDirectorySymbol(name_symbol);
                            if (! subdirectory_symbol)
                                subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(name_symbol);
                            directory_symbol = subdirectory_symbol;
                        }
                    }

                    for (end++; end < name_length && name[end] == U_SLASH; end++) // skip all extra '/'
                        ;
                }

                //
                // Insert the new directory into the system table to avoid duplicates, in case
                // the same directory is specified with a different name.
                //
                if (directory_symbol != classpath[dot_classpath_index] -> RootDirectory()) // Not the dot directory.
                {
                    system_table -> InsertDirectorySymbol(status.st_dev, status.st_ino, directory_symbol);
                    directory_symbol -> ReadDirectory();
                }

                delete [] directory_name;
                delete [] name;
            }
        }

        delete [] input_name;

        return directory_symbol;
    }
#elif defined(WIN32_FILE_SYSTEM)
    DirectorySymbol *Control::ProcessSubdirectories(wchar_t *source_name, int source_name_length)
    {
        DirectorySymbol *directory_symbol = classpath[dot_classpath_index] -> RootDirectory();

        int name_length = (source_name_length < 0 ? 0 : source_name_length);
        wchar_t *name = new wchar_t[name_length + 1];
        char *input_name = new char[name_length + 1];
        for (int i = 0; i < name_length; i++)
            input_name[i] = name[i] = source_name[i];
        input_name[name_length] = name[name_length] = U_NULL;

        if (name_length >= 2 && Case::IsAsciiAlpha(input_name[0]) && input_name[1] == U_COLON) // a disk was specified
        {
            char disk = input_name[0];
            option.SaveCurrentDirectoryOnDisk(disk);
            if (SetCurrentDirectory(input_name))
            {
                DWORD directory_length = GetCurrentDirectory(0, input_name); // first, get the right size
                char *full_directory_name = new char[directory_length + 1];  // allocate the directory
                DWORD length = GetCurrentDirectory(directory_length, full_directory_name);
                if (length <= directory_length)
                {
                    for (char *ptr = full_directory_name; *ptr; ptr++)
                        *ptr = (*ptr != U_BACKSLASH ? *ptr : (char) U_SLASH); // turn '\' to '/'.

                    char *current_directory = option.GetMainCurrentDirectory();
                    int prefix_length = strlen(current_directory);
                    int start = (prefix_length <= length &&
                                 Case::StringSegmentEqual(current_directory, full_directory_name, prefix_length) &&
                                 (full_directory_name[prefix_length] == U_SLASH || full_directory_name[prefix_length] == U_NULL)
                                                ? prefix_length + 1
                                                : 0);

                    if (start > length)
                         name_length = 0;
                    else if (start <= length) // note that we can assert that (start != length)
                    {
                        delete [] name;
                        name_length = length - start;
                        name = new wchar_t[name_length + 1];
                        for (int k = 0, i = start; i < length; i++, k++)
                            name[k] = full_directory_name[i];
                        name[name_length] = U_NULL;
                    }
                }

                delete [] full_directory_name;
            }

            option.ResetCurrentDirectoryOnDisk(disk);  // reset the current directory on this disk
            option.SetMainCurrentDirectory(); // reset the real current directory...
        }

        //
        //
        //
        int end;
        if (name_length > 2 && Case::IsAsciiAlpha(name[0]) && name[1] == U_COLON && name[2] == U_SLASH)
            end = 3;
        else
        {
            for (end = 0; end < name_length && name[end] == U_SLASH; end++) // keep all extra leading '/'
                ;
        }

        //
        //
        //
        wchar_t *directory_name = new wchar_t[name_length];
        int length;
        if (end > 0)
        {
            for (length = 0; length < end; length++)
                directory_name[length] = name[length];
            NameSymbol *name_symbol = FindOrInsertName(directory_name, length);
            DirectorySymbol *subdirectory_symbol = directory_symbol -> FindDirectorySymbol(name_symbol);
            if (! subdirectory_symbol)
                subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(name_symbol);
            directory_symbol = subdirectory_symbol;
        }

        //
        //
        //
        for (int start = end; start < name_length; start = end)
        {
            for (length = 0; end < name_length && name[end] != U_SLASH; length++, end++)
                directory_name[length] = name[end];

            if (length != 1 || directory_name[0] != U_DOT) // Not the current directory
            {
                if (length == 2 && directory_name[0] == U_DOT && directory_name[1] == U_DOT) // keep the current directory
                {
                    if (directory_symbol -> Identity() == dot_name_symbol || directory_symbol -> Identity() == dot_dot_name_symbol)
                    {
                        DirectorySymbol *subdirectory_symbol = directory_symbol -> FindDirectorySymbol(dot_dot_name_symbol);
                        if (! subdirectory_symbol)
                            subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(dot_dot_name_symbol);
                        directory_symbol = subdirectory_symbol;
                    }
                    else directory_symbol = directory_symbol -> owner -> DirectoryCast();
                }
                else
                {
                    NameSymbol *name_symbol = FindOrInsertName(directory_name, length);
                    DirectorySymbol *subdirectory_symbol = directory_symbol -> FindDirectorySymbol(name_symbol);
                    if (! subdirectory_symbol)
                        subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(name_symbol);
                    directory_symbol = subdirectory_symbol;
                }
            }

            for (end++; end < name_length && name[end] == U_SLASH; end++) // skip all extra '/'
                ;
        }

        directory_symbol -> ReadDirectory();

        delete [] directory_name;
        delete [] name;
        delete [] input_name;

        return directory_symbol;
    }
#endif


void Control::ProcessNewInputFiles(SymbolSet &file_set, ArgumentExpander &arguments, int first_index)
{
    for (int i = 0; i < bad_input_filenames.Length(); i++)
        delete [] bad_input_filenames[i];
    bad_input_filenames.Reset();

    for (int k = 0; k < unreadable_input_filenames.Length(); k++)
        delete [] unreadable_input_filenames[k];
    unreadable_input_filenames.Reset();

    //
    // Process all file names specified in command line
    //
    for (int j = first_index; j < arguments.argc; j++)
    {
        char *file_name = arguments.argv[j];
        int file_name_length = strlen(file_name);

        wchar_t *name = new wchar_t[file_name_length + 1];
        for (int i = 0; i < file_name_length; i++)
            name[i] = (file_name[i] != U_BACKSLASH ? file_name[i] : (wchar_t) U_SLASH); // change backslash to forward slash.
        name[file_name_length] = U_NULL;

        //
        // File must be of the form xxx.java where xxx is a
        // character string consisting of at least one character.
        //
        if (file_name_length < FileSymbol::java_suffix_length ||
            (! FileSymbol::IsJavaSuffix(&file_name[file_name_length - FileSymbol::java_suffix_length])))
            bad_input_filenames.Next() = name;
        else
        {
            FileSymbol *file_symbol = FindOrInsertJavaInputFile(name, file_name_length - FileSymbol::java_suffix_length);

            if (! file_symbol)
                unreadable_input_filenames.Next() = name;
            else
            {
                delete [] name;
                file_set.AddElement(file_symbol);
            }
        }
    }

    return;
}


FileSymbol *Control::FindOrInsertJavaInputFile(DirectorySymbol *directory_symbol, NameSymbol *file_name_symbol)
{
    FileSymbol *file_symbol = NULL;

    int length = file_name_symbol -> Utf8NameLength() + FileSymbol::java_suffix_length;
    char *java_name = new char[length + 1]; // +1 for \0
    strcpy(java_name, file_name_symbol -> Utf8Name());
    strcat(java_name, FileSymbol::java_suffix);

    DirectoryEntry *entry = directory_symbol -> FindEntry(java_name, length);
    if (entry)
    {
        file_symbol = directory_symbol -> FindFileSymbol(file_name_symbol);

        if (! file_symbol)
        {
            file_symbol = directory_symbol -> InsertFileSymbol(file_name_symbol);
            file_symbol -> directory_symbol = directory_symbol;
            file_symbol -> SetJava();
        }

        file_symbol -> mtime = entry -> Mtime();
    }

    delete [] java_name;

    return file_symbol;
}


FileSymbol *Control::FindOrInsertJavaInputFile(wchar_t *name, int name_length)
{
    FileSymbol *file_symbol = NULL;

    //
    // The name has been preprocessed so that if it contains any
    // slashes it is a forward slash. In the loop below we look
    // for the occurrence of the first slash (if any) that separates
    // the file name from its directory name.
    //
    DirectorySymbol *directory_symbol;
    NameSymbol *file_name_symbol;
#ifdef UNIX_FILE_SYSTEM
    int len;
    for (len = name_length - 1; len >= 0 && name[len] != U_SLASH; len--)
        ;
    directory_symbol = ProcessSubdirectories(name, len);
    file_name_symbol = FindOrInsertName(&name[len + 1], name_length - (len + 1));
#elif defined(WIN32_FILE_SYSTEM)
    int len;
    for (len = name_length - 1; len >= 0 && name[len] != U_SLASH && name[len] != U_COLON; len--)
        ;
    directory_symbol = ProcessSubdirectories(name, (name[len] == U_COLON ? len + 1 : len));
    file_name_symbol = FindOrInsertName(&name[len + 1], name_length - (len + 1));
#endif

    for (int i = 1; i < classpath.Length(); i++)
    {
        if (i == dot_classpath_index) // the current directory (.).
        {
            file_symbol = FindOrInsertJavaInputFile(directory_symbol, file_name_symbol);
            if (file_symbol)
                break;
        }
        else if (classpath[i] -> IsZip())
        {
            DirectorySymbol *directory_symbol = FindSubdirectory(classpath[i], name, len);
            if (directory_symbol)
            {
                file_symbol = directory_symbol -> FindFileSymbol(file_name_symbol);
                if (file_symbol && file_symbol -> IsJava())
                     break;
                else file_symbol = NULL;
            }
        }
    }

    //
    // If the file was found, return it; otherwise, in case the (.) directory was not specified in the
    // classpath, search for the file in it...
    //
    return (file_symbol ? file_symbol : FindOrInsertJavaInputFile(directory_symbol, file_name_symbol));
}


PackageSymbol *Control::FindOrInsertPackage(LexStream *lex_stream, AstExpression *name)
{
    PackageSymbol *package;

    AstFieldAccess* field_access = name -> FieldAccessCast();
    if (field_access)
    {
        package = FindOrInsertPackage(lex_stream, field_access -> base);
        NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
        PackageSymbol *subpackage = package -> FindPackageSymbol(name_symbol);
        if (! subpackage)
            subpackage = package -> InsertPackageSymbol(name_symbol);
        package = subpackage;
    }
    else
    {
        AstSimpleName *simple_name = (AstSimpleName *) name;
        NameSymbol *name_symbol = lex_stream -> NameSymbol(simple_name -> identifier_token);
        package = external_table.FindPackageSymbol(name_symbol);
        if (! package)
            package = external_table.InsertPackageSymbol(name_symbol, NULL);
    }

    FindPathsToDirectory(package);

    return package;
}


void Control::ProcessFile(FileSymbol *file_symbol)
{
    ProcessHeaders(file_symbol);

    //
    // As long as there are new bodies, ...
    //
    for (int i = 0; i < needs_body_work.Length(); i++)
    {
        assert(semantic.Length() == 0);

        ProcessBodies(needs_body_work[i]);
    }
    needs_body_work.Reset();

    return;
}


void Control::ProcessHeaders(FileSymbol *file_symbol)
{
    input_java_file_set.AddElement(file_symbol);

    bool initial_invocation = (semantic.Length() == 0);

    if (option.verbose)
    {
        Coutput << "[read "
                << file_symbol -> FileName()
                << "]\n";
    }

    if (! file_symbol -> lex_stream)
         scanner -> Scan(file_symbol);
    else file_symbol -> lex_stream -> Reset();

    if (file_symbol -> lex_stream) // do we have a successful scan!
    {
        if (! file_symbol -> compilation_unit)
            file_symbol -> compilation_unit = parser -> HeaderParse(file_symbol -> lex_stream);
        //
        // If we have a compilation unit, analyze it, process its types.
        //
        if (file_symbol -> compilation_unit)
        {
            assert(! file_symbol -> semantic);

            if (! file_symbol -> package)
                ProcessPackageDeclaration(file_symbol, file_symbol -> compilation_unit -> package_declaration_opt);
            file_symbol -> semantic = new Semantic(*this, file_symbol);
            semantic.Next() = file_symbol -> semantic;
            file_symbol -> semantic -> ProcessTypeNames();
        }
    }

    if (initial_invocation)
        ProcessMembers();

    return;
}


void Control::ProcessMembers()
{
    Tuple<TypeSymbol *> partially_ordered_types(1024);
    SymbolSet needs_member_work(101);
    TypeCycleChecker cycle_checker(partially_ordered_types);
    TopologicalSort topological_sorter(needs_member_work, partially_ordered_types);

    int start = 0;
    while (start < semantic.Length())
    {
        needs_member_work.SetEmpty();

        do
        {
            //
            // Check whether or not there are cycles in this new batch of types.
            // Create a partial order of the types (cycles are ordered arbitrarily)
            // and place the result in partially_ordered_types.
            //
            cycle_checker.PartialOrder(semantic, start);
            start = semantic.Length(); // next starting point

            //
            // Process the extends and implements clauses.
            //
            for (int j = 0; j < partially_ordered_types.Length(); j++)
            {
                TypeSymbol *type = partially_ordered_types[j];
                needs_member_work.AddElement(type);
                type -> ProcessTypeHeaders();
                type -> semantic_environment -> sem -> types_to_be_processed.AddElement(type);
            }
        } while (start < semantic.Length());

        //
        // Patially order the collection of types in needs_member_work and place the result
        // in partially_ordered_types. This reordering is based on the complete "supertype"
        // information computed in ProcessTypeHeaders.
        //
        topological_sorter.Sort();
        for (int i = 0; i < partially_ordered_types.Length(); i++)
        {
            TypeSymbol *type = partially_ordered_types[i];
            needs_body_work.Next() = type;
            type -> ProcessMembers();
        }
    }

    semantic.Reset();

    return;
}



void Control::ProcessBodies(TypeSymbol *type)
{
    Semantic *sem = type -> semantic_environment -> sem;

    if (type -> declaration && (! sem -> compilation_unit -> BadCompilationUnitCast()))
    {
        AstInterfaceDeclaration *interface_declaration = type -> declaration -> InterfaceDeclarationCast();
        AstClassDeclaration *class_declaration = type -> declaration -> ClassDeclarationCast();

#ifdef WIN32_FILE_SYSTEM
        if (! type -> file_symbol -> IsZip())
        {
            int length = type -> Utf8NameLength() + FileSymbol::class_suffix_length;
            char *classfile_name = new char[length + 1]; // +1 for "\0"
            strcpy(classfile_name, type -> Utf8Name());
            strcat(classfile_name, FileSymbol::class_suffix);

            DirectorySymbol *directory = type -> file_symbol -> OutputDirectory();
            DirectoryEntry *entry = directory -> FindCaseInsensitiveEntry(classfile_name, length);

            //
            // If no entry exists for the classfile name, add it as we are about to create
            // such a file. If an entry is found and it is not identical (in a case-sensitive test)
            // to the name of the type, issue an appropriate message.
            //
            if (! entry)
                directory -> InsertEntry(classfile_name, length);
            else if (strcmp(classfile_name, entry -> name) != 0)
            {
                wchar_t *entry_name = new wchar_t[entry -> length + 1];
                for (int i = 0; i < length; i++)
                    entry_name[i] = entry -> name[i];
                entry_name[entry -> length] = U_NULL;
                LexStream::TokenIndex identifier_token = (interface_declaration ? interface_declaration -> identifier_token
                                                                                : class_declaration -> identifier_token);

                sem -> ReportSemError(SemanticError::FILE_FILE_CONFLICT,
                                      identifier_token,
                                      identifier_token,
                                      type -> Name(),
                                      entry_name,
                                      directory -> Name());

                delete [] entry_name;
            }

            delete [] classfile_name;
        }
#endif

        if (interface_declaration)
        {
            if (! parser -> InitializerParse(sem -> lex_stream, interface_declaration))
                sem -> compilation_unit -> kind = Ast::BAD_COMPILATION; // recall that syntax errors were detected
            else
            {
                type -> CompleteSymbolTable();
                if (! parser -> BodyParse(sem -> lex_stream, interface_declaration))
                     sem -> compilation_unit -> kind = Ast::BAD_COMPILATION; // mark that syntax errors were detected
                else type -> ProcessExecutableBodies();
            }
        }
        else
        {
            if (! parser -> InitializerParse(sem -> lex_stream, class_declaration -> class_body))
                sem -> compilation_unit -> kind = Ast::BAD_COMPILATION; // recall that syntax errors were detected
            else
            {
                type -> CompleteSymbolTable();
                if (! parser -> BodyParse(sem -> lex_stream, class_declaration -> class_body))
                     sem -> compilation_unit -> kind = Ast::BAD_COMPILATION; // mark that syntax errors were detected.
                else type -> ProcessExecutableBodies();
            }
        }

        if ((sem -> NumErrors() == 0) && (! sem -> compilation_unit -> BadCompilationUnitCast()))
        {
            Tuple<TypeSymbol *> *types = new Tuple<TypeSymbol *>(1024);
            types -> Next() = type;

            for (int j = 0; j < type -> num_anonymous_types(); j++)
            {
                types -> Next() = type -> AnonymousType(j);
            }

            if (type -> local)
            {
                for (TypeSymbol *local_type = (TypeSymbol *) type -> local -> FirstElement();
                                 local_type;
                                 local_type = (TypeSymbol *) type -> local -> NextElement())
                {
                    types -> Next() = local_type;
                }
            }

            if (type -> non_local)
            {
                for (TypeSymbol *non_local_type = (TypeSymbol *) type -> non_local -> FirstElement();
                                 non_local_type;
                                 non_local_type = (TypeSymbol *) type -> non_local -> NextElement())
                {
                    types -> Next() = non_local_type;
                }
            }

            //
            // If we are supposed to generate code, do so now !!!
            //
            if (option.bytecode)
            {
                for (int k = 0; k < types -> Length(); k++)
                {
                    TypeSymbol *type = (*types)[k];

                    type -> file_symbol -> SetFileNameLiteral((Control *) this); // make sure the literal is available for bytecode

                    ByteCode *code = new ByteCode(type);
                    code -> GenerateCode();

                    delete code;
                }
            }

            //
            // If no error was detected while generating code, then
            // start cleaning up.
            //
            if (! option.nocleanup)
            {
            if (sem -> NumErrors() == 0)
            {
                for (int k = 0; k < types -> Length(); k++)
                {
                    TypeSymbol *type = (*types)[k];

                    delete type -> semantic_environment;
                    type -> semantic_environment = NULL;

                    if (type -> ACC_INTERFACE())
                    {
                        AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) type -> declaration;
                        interface_declaration -> semantic_environment = NULL;
                    }
                    else
                    {
                        AstClassDeclaration *class_declaration = type -> declaration -> ClassDeclarationCast();

                        if (class_declaration)
                            class_declaration -> semantic_environment = NULL;
                    }
                }
            }

            delete types;
        }
    }
    }

    sem -> types_to_be_processed.RemoveElement(type);
    if (! option.nocleanup)
    if (sem -> types_to_be_processed.Size() == 0) // all types belonging to this compilation unit have been processed.
        CleanUp(sem -> source_file_symbol);

    return;
}


//
// Introduce the main package and the current package.
// This procedure is invoked directly only while doing
// an incremental compilation.
//
void Control::ProcessPackageDeclaration(FileSymbol *file_symbol, AstPackageDeclaration *package_declaration)
{
    file_symbol -> package = (package_declaration
                                     ? FindOrInsertPackage(file_symbol -> lex_stream, package_declaration -> name)
                                     : unnamed_package);

    for (int i = 0; i < file_symbol -> lex_stream -> NumTypes(); i++)
    {
        LexStream::TokenIndex identifier_token = file_symbol -> lex_stream -> Next(file_symbol -> lex_stream -> Type(i));
        if (file_symbol -> lex_stream -> Kind(identifier_token) == TK_Identifier)
        {
            NameSymbol *name_symbol = file_symbol -> lex_stream -> NameSymbol(identifier_token);
            if (! file_symbol -> package -> FindTypeSymbol(name_symbol))
            {
                TypeSymbol *type = file_symbol -> package -> InsertOuterTypeSymbol(name_symbol);
                type -> file_symbol = file_symbol;
                type -> outermost_type = type;
                type -> supertypes_closure = new SymbolSet;
                type -> subtypes = new SymbolSet;
                type -> SetOwner(file_symbol -> package);
                type -> SetSignature(*this);
                type -> MarkSourcePending();

                //
                // If this type is contained in the unnamed package add it to the set
                // unnamed_package_types if a type of similar name was not already there.
                //
                if ((! package_declaration) && (unnamed_package_types.Image(type -> Identity()) == NULL))
                    unnamed_package_types.AddElement(type);
            }
        }
    }

    return;
}


void Control::CleanUp(FileSymbol *file_symbol)
{
    Semantic *sem = file_symbol -> semantic;

    if (sem)
    {
#ifdef TEST
        if (option.debug_dump_lex)
        {
            sem -> lex_stream -> Reset(); // rewind input and ...
            sem -> lex_stream -> Dump();  // dump it!
        }
        if (option.debug_dump_ast)
            sem -> compilation_unit -> Print(*sem -> lex_stream);
        if (option.debug_unparse_ast)
        {
            if (option.debug_unparse_ast_debug)
              {
                // which of these is correct?
                sem -> compilation_unit -> debug_unparse = true;
                Ast::debug_unparse = true;
              }
            sem -> compilation_unit -> Unparse(*sem -> lex_stream,
                                               "unparsed/");
        }
#endif
        sem -> PrintMessages();
        if (sem -> return_code > 0)
            return_code = 1;

        file_symbol -> CleanUp();
    }

    return;
}
