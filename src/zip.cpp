// $Id: zip.cpp,v 1.6 1999/08/26 15:34:11 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include <assert.h>
#include <iostream.h>
#include <string.h>
#include "control.h"
#include "zip.h"
#include "symbol.h"

//************************************************************************************************
//
// The ZipFile methods follow
//
//************************************************************************************************
#ifdef UNIX_FILE_SYSTEM
    int (*ZipFile::uncompress_file[10]) (FILE *, char *, long) =
    {
        UncompressFile0,
        UncompressFile1,
        UncompressFile2,
        UncompressFile3,
        UncompressFile4,
        UncompressFile5,
        UncompressFile6,
        UncompressFile7,
        UncompressFile8,
        UncompressFile9
    };

    inline u1 ZipFile::GetU1()
    {
        return getc(zipfile);
    }

    inline void ZipFile::Skip(u4 length)
    {
        for (u4 i = 0; i < length; i++)
             getc(zipfile);
    }

#elif defined(WIN32_FILE_SYSTEM)

    int (*ZipFile::uncompress_file[10]) (char *, char *, long) =
    {
        UncompressFile0,
        UncompressFile1,
        UncompressFile2,
        UncompressFile3,
        UncompressFile4,
        UncompressFile5,
        UncompressFile6,
        UncompressFile7,
        UncompressFile8,
        UncompressFile9
    };

    inline u1 ZipFile::GetU1()
    {
        return *file_buffer++;
    }

    inline void ZipFile::Skip(u4 length)
    {
        file_buffer += length;
    }
#endif


inline u2 ZipFile::GetU2()
{
    u4 val = GetU1();
    val |= (((u4) GetU1()) << 8);

    return val;
}


inline u4 ZipFile::GetU4()
{
    u4 val = GetU1();
    val |= (((u4) GetU1()) << 8);
    val |= (((u4) GetU1()) << 16);
    val |= (((u4) GetU1()) << 24);

    return val;
}


ZipFile::ZipFile(FileSymbol *file_symbol) : buffer(NULL)
{
    Zip *zip = file_symbol -> Zipfile();

    assert(zip -> IsValid());

#ifdef UNIX_FILE_SYSTEM
    zipfile = zip -> zipfile;
    int rc = fseek(zipfile, file_symbol -> offset, SEEK_SET);

    assert(rc == 0);

#elif defined(WIN32_FILE_SYSTEM)
    file_buffer = &zip -> zipbuffer[file_symbol -> offset];
#endif

    Skip(8); // u4 magic                     = GetU4();
             // u2 version_needed_to_extract = GetU2();
             // u2 general_purpose_bits      = GetU2();
    u2 compression_method                    = GetU2();
    Skip(16); // u2 time                     = GetU2();
              // u2 date                     = GetU2();
              // u4 crc32                    = GetU4();
              // u4 compressed_size          = GetU4();
              // u4 uncompressed_size        = GetU4();
    u2 filename_length                       = GetU2();
    u2 extra_field_length                    = GetU2();
    Skip(filename_length + extra_field_length);

#ifdef UNIX_FILE_SYSTEM
    this -> buffer = new char[file_symbol -> uncompressed_size];
    if (! uncompress_file[compression_method < 9 ? compression_method : 9](zipfile, this -> buffer, file_symbol -> uncompressed_size))
    {
        delete [] this -> buffer;
        this -> buffer = NULL;
    }
#elif defined(WIN32_FILE_SYSTEM)
    if (compression_method > 0)
    {
        this -> buffer = new char[file_symbol -> uncompressed_size];
        if (! uncompress_file[compression_method < 9 ? compression_method : 9](file_buffer,
                                                                               this -> buffer,
                                                                               file_symbol -> uncompressed_size))
        {
            delete [] this -> buffer;
            this -> buffer = NULL;
            this -> file_buffer = NULL;
        }
    }
#endif

    return;
}


ZipFile::~ZipFile()
{
    delete [] buffer;
}


//************************************************************************************************
//
// The Zip methods follow:
//
//************************************************************************************************
inline u1 Zip::GetU1()
{
    return *buffer_ptr++;
}


inline u2 Zip::GetU2()
{
    u4 val = GetU1();
    val |= (((u4) GetU1()) << 8);

    return val;
}


inline u4 Zip::GetU4()
{
    u4 val = GetU1();
    val |= (((u4) GetU1()) << 8);
    val |= (((u4) GetU1()) << 16);
    val |= (((u4) GetU1()) << 24);

    return val;
}


inline void Zip::Skip(u4 length)
{
    buffer_ptr += length;
}


inline DirectorySymbol *Zip::ProcessSubdirectoryEntries(DirectorySymbol *directory_symbol, char *name, int name_length)
{
    wchar_t *directory_name = new wchar_t[name_length];

    for (int start = 0, end; start < name_length; start = end + 1)
    {
        end = start;
        for (int i = 0; end < name_length && name[end] != U_SLASH; i++, end++)
             directory_name[i] = name[end];
        NameSymbol *name_symbol = control.FindOrInsertName(directory_name, end - start);
        DirectorySymbol *subdirectory_symbol = directory_symbol -> FindDirectorySymbol(name_symbol);
        if (! subdirectory_symbol)
            subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(name_symbol);
        directory_symbol = subdirectory_symbol;
    }

    delete [] directory_name;

    return directory_symbol;
}


inline NameSymbol *Zip::ProcessFilename(char *name, int name_length)
{
    wchar_t *input_filename = new wchar_t[name_length];
    for (int i = 0; i < name_length; i++)
        input_filename[i] = name[i];
    NameSymbol *name_symbol = control.FindOrInsertName(input_filename, name_length);

    delete [] input_filename;

    return name_symbol;
}


inline void Zip::ProcessDirectoryEntry()
{
    Skip(8); // u2 version_made_by           = GetU2();
             // u2 version_needed_to_extract = GetU2();
             // u2 general_purpose_bits      = GetU2();
             // u2 compression_method        = GetU2();
    u2 last_mod_file_time                    = GetU2();
    u2 last_mod_file_date                    = GetU2();
    Skip(4); // u4 crc32                     = GetU4();
    Skip(4); // u4 compressed_size           = GetU4();
    u4 uncompressed_size                     = GetU4();
    u2 file_name_length                      = GetU2();
    u2 extra_field_length                    = GetU2();
    u2 file_comment_length                   = GetU2();
    Skip(8); // u2 disk_number_start         = GetU2();
             // u2 internal_file_attributes  = GetU2();
             // u4 external_file_attributes  = GetU4();
    u4 relative_offset_of_local_header       = GetU4();

    u4 date_time = ((u4) last_mod_file_date) << 16 | last_mod_file_time;
    char *name = buffer_ptr;

    Skip(file_name_length + extra_field_length + file_comment_length);

    //
    // Note that we need to process all subdirectory entries that appear in the zip file, and not
    // just the ones that contain java and class files. Recall that in java the dot notation is
    // used in specifying a package. Therefore, in processing a qualified-name that represents
    // a package, we need to recognize each name as a subpackage. E.g., when processing
    // "java.lang", we need to recognize "java" as a package before looking for "lang"...
    //
    DirectorySymbol *directory_symbol = root_directory; // start at the "." directory.
    if (name[file_name_length - 1] == U_SLASH)
        ProcessSubdirectoryEntries(directory_symbol, name, file_name_length - 1);  // -1 to remove last '/'
    else
    {
        bool java_file = (file_name_length >= FileSymbol::java_suffix_length &&
                          FileSymbol::IsJavaSuffix(&name[file_name_length - FileSymbol::java_suffix_length])),
             class_file = (file_name_length >= FileSymbol::class_suffix_length &&
                           FileSymbol::IsClassSuffix(&name[file_name_length - FileSymbol::class_suffix_length]));

        if (java_file || class_file)
        {
            int name_length = file_name_length - (java_file ? FileSymbol::java_suffix_length : FileSymbol::class_suffix_length);
            int i;
            for (i = name_length - 1; i >= 0 && name[i] != U_SLASH; i--)
                ;
            if (i > 0) // directory specified?
                directory_symbol = ProcessSubdirectoryEntries(directory_symbol, name, i);
            NameSymbol *name_symbol = ProcessFilename(&name[i + 1], name_length - (i + 1));

            //
            // Search for a file of that name in the directory. If one is not found, then insert ...
            // Otherwise, either a class file of that name was previously processed and now we found
            // a java file with the same name or vice-versa... In that case keep (or replace with ) the
            // the file with the most recent date stamp.
            //
            FileSymbol *file_symbol = directory_symbol -> FindFileSymbol(name_symbol);
            if (! file_symbol)
            {
                file_symbol = directory_symbol -> InsertFileSymbol(name_symbol);

                file_symbol -> directory_symbol = directory_symbol;
                if (java_file)
                     file_symbol -> SetJava();
                else file_symbol -> SetClassOnly();

                file_symbol -> uncompressed_size = uncompressed_size;
                file_symbol -> offset = relative_offset_of_local_header;
                file_symbol -> date_time = date_time;
            }
            else if (file_symbol -> date_time < date_time)
            {
                if (java_file)
                     file_symbol -> SetJava();
                else file_symbol -> SetClass();

                file_symbol -> uncompressed_size = uncompressed_size;
                file_symbol -> offset = relative_offset_of_local_header;
                file_symbol -> date_time = date_time;
            }
        }
    }

    return;
}


Zip::Zip(Control &control_, char *zipfile_name) : control(control_),
                                                  magic(0),
                                                  zipbuffer(NULL)
{
#ifdef UNIX_FILE_SYSTEM
    zipfile = ::SystemFopen(zipfile_name, "rb");
    if (zipfile)
    {
        int rc = fseek(zipfile, -22, SEEK_END);
        if (rc == 0);
        {
            zipbuffer = new char[22];
            buffer_ptr = zipbuffer;
            fread(buffer_ptr, sizeof(char), 22, zipfile);

            magic = GetU4();
        }
    }
#elif defined(WIN32_FILE_SYSTEM)
    zipfile = CreateFile(zipfile_name, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
    if (zipfile != INVALID_HANDLE_VALUE)
    {
        mapfile = CreateFileMapping(zipfile, NULL, PAGE_READONLY, 0, 0, NULL);
        zipbuffer = (mapfile == INVALID_HANDLE_VALUE ? NULL : (char *) MapViewOfFile(mapfile, FILE_MAP_READ, 0, 0, 0));
        if (zipbuffer)
        {
            buffer_ptr = &zipbuffer[GetFileSize(zipfile, NULL) - 22];
            magic = GetU4();
        }
    }
#endif

    ReadDirectory();

    return;
}


Zip::~Zip()
{
#ifdef UNIX_FILE_SYSTEM
    delete [] zipbuffer;
    if (zipfile)
        fclose(zipfile);
#elif defined(WIN32_FILE_SYSTEM)
    if (zipfile != INVALID_HANDLE_VALUE)
    {
        if (mapfile != INVALID_HANDLE_VALUE)
        {
            if (zipbuffer)
                UnmapViewOfFile(zipbuffer);
            CloseHandle(mapfile);
        }
        CloseHandle(zipfile);
    }
#endif

    delete root_directory;
}


//
// Upon successful termination of this function, IsValid() should yield true.
// I.e., we should be able to assert that (magic == 0x06054b50)
//
void Zip::ReadDirectory()
{
    root_directory = new DirectorySymbol(control.dot_name_symbol, NULL);

    if (IsValid())
    {
        Skip(8); // u2 number_of_this_disk              = GetU2();
                 // u2 number_of_the_disk_with_the_star = GetU2();
                 // u2 start_of_the_central_directory   = GetU2();
                 // u2 total_number_of_entries_in_the_  = GetU2();
        u4 central_directory_size                       = GetU4();

#ifdef UNIX_FILE_SYSTEM
        int rc = fseek(zipfile, -((int) central_directory_size + 22), SEEK_END);

        assert(rc == 0);

        delete [] zipbuffer;
        zipbuffer = new char[central_directory_size + 22];
        buffer_ptr = zipbuffer;
        fread(buffer_ptr, sizeof(char), central_directory_size + 22, zipfile);
#elif defined(WIN32_FILE_SYSTEM)
        buffer_ptr -= (central_directory_size + 16);
#endif
        for (magic = GetU4(); magic == 0x02014b50; magic = GetU4())
             ProcessDirectoryEntry();
    }

    return;
}


