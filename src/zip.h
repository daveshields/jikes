// $Id: zip.h,v 1.11 2001/09/14 05:31:34 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef zip_INCLUDED
#define zip_INCLUDED

#include "platform.h"

#include "tuple.h"
#include "unzip.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class Control;
class Zip;
class DirectorySymbol;
class FileSymbol;
class NameSymbol;


class ZipFile : public Unzip
{
public:

    ZipFile(FileSymbol *);
    ~ZipFile();

private:
    char *buffer;

    u1 GetU1();
    u2 GetU2();
    u4 GetU4();
    void Skip(u4 length);

#ifdef UNIX_FILE_SYSTEM
        FILE *zipfile;
        static int (*uncompress_file[10]) (FILE *, char *, long);
    public:
        inline char *Buffer() { return buffer; }
#elif defined(WIN32_FILE_SYSTEM)
        char *file_buffer;
        static int (*uncompress_file[10]) (char *, char *, long);
    public:
        inline char *Buffer() { return (buffer ? buffer : file_buffer); }
#endif
};


class Zip
{
public:
    Zip(Control &, char *);
    ~Zip();

    bool IsValid() { return magic == 0x06054b50; }

    DirectorySymbol *RootDirectory() { return root_directory; }

private:
    friend class ZipFile;

    Control &control;

    u4 magic;

    DirectorySymbol *root_directory;

    char *zipbuffer,
         *buffer_ptr;

    u1 GetU1();
    u2 GetU2();
    u4 GetU4();
    void Skip(u4 length);

    void ReadDirectory();

    NameSymbol *ProcessFilename(char *, int);
    DirectorySymbol *ProcessSubdirectoryEntries(DirectorySymbol *, char *, int);
    void ProcessDirectoryEntry();

#ifdef UNIX_FILE_SYSTEM
    FILE *zipfile;
#elif defined(WIN32_FILE_SYSTEM)
    HANDLE zipfile, mapfile;
#endif
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // zip_INCLUDED

