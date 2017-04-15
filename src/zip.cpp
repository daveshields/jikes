#include "zip.h"
#include "control.h"
#include "symbol.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

//************************************************************
//
// The ZipFile methods follow
//
//************************************************************
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

#elif defined(WIN32_FILE_SYSTEM) // ! UNIX_FILE_SYSTEM

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
#endif // WIN32_FILE_SYSTEM


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
}


ZipFile::~ZipFile()
{
    delete [] buffer;
}


//********************************************************************
//
// The Zip methods follow:
//
//********************************************************************
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
            subdirectory_symbol = directory_symbol -> InsertDirectorySymbol(name_symbol, false);
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
    // Note that we need to process all subdirectory entries
    // that appear in the zip file, and not just the ones that
    // contain java and class files. Recall that in java the
    // dot notation is used in specifying a package. Therefore,
    // in processing a qualified-name that represents a package,
    // we need to recognize each name as a subpackage. E.g.,
    // when processing "java.lang", we need to recognize "java"
    // as a package before looking for "lang"...

    // start at the "." directory.
    DirectorySymbol *directory_symbol = root_directory;
    // -1 to remove last '/'
    if (name[file_name_length - 1] == U_SLASH)
        ProcessSubdirectoryEntries(directory_symbol,
                                   name,
                                   file_name_length - 1);
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
                directory_symbol = ProcessSubdirectoryEntries(directory_symbol,
                                                              name, i);
            NameSymbol *name_symbol = ProcessFilename(&name[i + 1],
                                                      name_length - (i + 1));

            //
            // Search for a file of that name in the directory.
            // If one is not found, then insert ... Otherwise,
            // either a class file of that name was previously
            // processed and now we found a java file with the
            // same name or vice-versa... In that case keep
            // (or replace with) the file with the most recent
            // date stamp.
            //
            FileSymbol *file_symbol = directory_symbol ->
                FindFileSymbol(name_symbol);
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
}


Zip::Zip(Control &control_, char *zipfile_name) : control(control_),
                                                  magic(0),
                                                  zipbuffer(NULL)
{
#ifdef UNIX_FILE_SYSTEM
    zipfile = SystemFopen(zipfile_name, "rb");
    if (zipfile)
    {
        int rc = fseek(zipfile, -END_SIZE, SEEK_END);
        if (rc == 0)
        {
            zipbuffer = new char[END_SIZE];
            buffer_ptr = zipbuffer;
            SystemFread(buffer_ptr, sizeof(char), END_SIZE, zipfile);

            magic = GetU4();
        }
    }
#elif defined(WIN32_FILE_SYSTEM)
    zipfile = CreateFile(zipfile_name,
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         NULL,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_READONLY,
                         NULL);
    if (zipfile != INVALID_HANDLE_VALUE)
    {
        mapfile = CreateFileMapping(zipfile, NULL, PAGE_READONLY, 0, 0, NULL);
        zipbuffer = (mapfile == INVALID_HANDLE_VALUE ?
                     NULL :
                     (char *) MapViewOfFile(mapfile,
                                            FILE_MAP_READ,
                                            0, 0, 0)
                     );
        if (zipbuffer)
        {
            buffer_ptr = &zipbuffer[GetFileSize(zipfile, NULL) - END_SIZE];
            magic = GetU4();
        }
    }
#endif

    // The following was posted to the dev list, but was just
    // too good to not put in here, the next person to have to
    // deal with this crap will appreciate it. -=Chris
    //
    // From: Mo DeJong <supermo@bayarea.net>
    //
    //   Ode to a zip file:
    //
    //   I can't read it forwards
    //   I can't read it backwards
    //   I must know where to begin
    //   so I need to look in the middle
    //   to find the middle, I must know the end
    //   but I don't know where that is, so I guess
    //
    // -------------------------------------------------


    // This may or may not be a valid zip file. The zip file might have
    // a file comment so we can't be sure where the END header is located.
    // We check for the LOC header at byte 0 to make sure this is a valid
    // zip file and then scan over the file backwards in search of the
    // END header.

    if (zipbuffer != NULL && ! IsValid()) {
        u4 sig = 0;

#ifdef UNIX_FILE_SYSTEM
        int res = fseek(zipfile, 0, SEEK_SET);
        assert(res == 0);

        char *tmpbuffer = new char[LOC_SIZE];
        buffer_ptr = tmpbuffer;
        SystemFread(buffer_ptr, sizeof(char), LOC_SIZE, zipfile);
        sig = GetU4();
        delete [] tmpbuffer;
        buffer_ptr = NULL;

        if (sig == LOC_SIG)
        {
            int block_size = 8192;
            tmpbuffer = new char[block_size];
            char *holdbuffer = new char[8];
            char *index_ptr;

            res = fseek(zipfile, 0, SEEK_END);
            assert(res == 0);

            long zip_file_size = ftell(zipfile);
            int num_loops = zip_file_size / block_size;
            magic = 0;

            for (; magic == 0 && num_loops >= 0 ; num_loops--) {

                if ((ftell(zipfile) - block_size) < 0)
                {
                    block_size = ftell(zipfile);
                    res = fseek(zipfile, 0L, SEEK_SET);
                }
                else
                {
                    res = fseek(zipfile, -block_size, SEEK_CUR);
                }

                assert(res == 0);
                SystemFread(tmpbuffer, sizeof(char), block_size, zipfile);
                res = fseek(zipfile, -block_size, SEEK_CUR); // undo fread
                assert(res == 0);

                for (index_ptr = tmpbuffer + block_size - 1;
                     index_ptr >= tmpbuffer;
                     index_ptr--)
                {
                    if (*index_ptr == 'P')
                    {
                        // Check for header signature that spans buffer
                        int span = (tmpbuffer + block_size) - index_ptr;

                        if (span < 4)
                        {
                            memmove(holdbuffer+span, holdbuffer, 3);
                            memmove(holdbuffer, index_ptr, span);
                            buffer_ptr = holdbuffer;
                        }
                        else
                        {
                            buffer_ptr = index_ptr;
                        }

                        sig = GetU4();

                        if (sig == END_SIG)
                        {
                            // Found the END header, put it in zipbuffer.
                            buffer_ptr = zipbuffer;
                            fseek(zipfile, block_size-span, SEEK_CUR);
                            SystemFread(buffer_ptr, sizeof(char),
                                END_SIZE, zipfile);

                            magic = GetU4();
                            break;
                        }
                    }
                }

                // Copy first 3 bytes into holdbuffer in case sig spans
                holdbuffer[0] = tmpbuffer[0];
                holdbuffer[1] = tmpbuffer[1];
                holdbuffer[2] = tmpbuffer[2];
            }

            delete [] tmpbuffer;
            delete [] holdbuffer;
        }
#elif defined(WIN32_FILE_SYSTEM)
        buffer_ptr = &zipbuffer[0];
        sig = GetU4();

        if (sig == LOC_SIG)
        {
            buffer_ptr = &zipbuffer[GetFileSize(zipfile, NULL) - END_SIZE];
            for ( ; buffer_ptr >= zipbuffer; buffer_ptr--)
            {
                if (*buffer_ptr == 'P')
                {
                    sig = GetU4();
                    if (sig == END_SIG)
                    {
                       magic = sig;
                       break;
                    }
                    else
                       buffer_ptr -= 4;
                }
            }
        }
#endif
    }

    ReadDirectory();
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
// Each CEN header would have been read so the magic number would get reset
// when the END header is again read.
//
void Zip::ReadDirectory()
{
    // Not a sourcepath (since we don't read java files from zip files)
    root_directory = new DirectorySymbol(control.dot_name_symbol, NULL, false);

    if (IsValid())
    {
        Skip(8); // u2 number_of_this_disk              = GetU2();
                 // u2 number_of_the_disk_with_the_star = GetU2();
                 // u2 start_of_the_central_directory   = GetU2();
                 // u2 total_number_of_entries_in_the_  = GetU2();
        u4 central_directory_size                       = GetU4();

#ifdef UNIX_FILE_SYSTEM
        u4 central_directory_offset                     = GetU4();
        Skip(2); // u2 comment_length                   = GetU2();
        int rc = fseek(zipfile, central_directory_offset, SEEK_SET);

        assert(rc == 0);

        delete [] zipbuffer;
        zipbuffer = new char[central_directory_size + END_SIZE];
        buffer_ptr = zipbuffer;
        SystemFread(buffer_ptr, sizeof(char),
                    central_directory_size + END_SIZE,
                    zipfile);
#elif defined(WIN32_FILE_SYSTEM)
        Skip(6); // u4 central_directory_offset         = GetU4();
                 // u2 comment_length                   = GetU2();
        buffer_ptr -= END_SIZE + central_directory_size;
#endif
        for (magic = GetU4(); magic == CEN_SIG; magic = GetU4())
             ProcessDirectoryEntry();
    }
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

