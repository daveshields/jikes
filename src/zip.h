// $Id: zip.h,v 1.5 1999/07/06 14:00:40 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef zip_INCLUDED
#define zip_INCLUDED

#ifdef WIN32_FILE_SYSTEM
#include <windows.h>
#endif

#include "config.h"
#include <stddef.h>
#include <stdio.h>
#include "tuple.h"
#include "unzip.h"

class Control;
class Zip;
class DirectorySymbol;
class FileSymbol;


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
#endif /* zip_INCLUDED */

