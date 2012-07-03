/*
 * $Id: jikesapi.cpp,v 1.12 2000/07/25 11:32:33 mdejong Exp $
 */


#include "platform.h"
#include "control.h"
#include "jikesapi.h"

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

#ifdef	HAVE_NAMESPACES
using namespace Jikes;
#endif

/**
 * A default implementation of ReadObject that read from the file sysytem.
 */ 
class DefaultFileReader: public JikesAPI::FileReader
{
    public:
    
    DefaultFileReader(const char *fileName);
    virtual  ~DefaultFileReader();
    
    virtual const char     *getBuffer()      {return(buffer);}
    virtual       size_t    getBufferSize()  {return(size);}
    
    private:
    
    const char     *buffer;
    size_t    size;
// FIXME : need to move into platform.h
#ifdef   WIN32_FILE_SYSTEM
    HANDLE    srcfile;
    HANDLE    mapfile;
#endif 
};

/**
 * A default implementaion of WriteObject that writes to the file system.
 */
class DefaultFileWriter: public JikesAPI::FileWriter
{
    public:
    DefaultFileWriter(const char *fileName,size_t maxSize);
    virtual  ~DefaultFileWriter();
    
    virtual  bool      isValid();
    
    private:
    
    virtual  size_t    doWrite(const unsigned char *data,size_t size);
    
    bool      valid;
// FIXME: need to clean this up, why is this not wrapped in a platform.h function?
#ifdef UNIX_FILE_SYSTEM
    FILE     *file;
#elif defined(WIN32_FILE_SYSTEM)
    HANDLE    file;
    HANDLE    mapfile;
    u1       *string_buffer;
    size_t    dataWritten;
#endif
};

JikesOption::~JikesOption()
{
    delete [] classpath ;
    delete [] directory ;
    delete [] encoding  ;
}

JikesOption::JikesOption():
    classpath(NULL),
    directory(NULL),
    encoding(NULL),
    nowrite(false),
    deprecation(false),
    O(false),
    g(false),
    verbose(false),
    depend(false),
    nowarn(false),
    old_classpath_search_order(false),
    zero_defect(false)
{
}

JikesAPI * JikesAPI::instance = NULL;

JikesAPI::JikesAPI():option(NULL)
{
    SetNewHandler();
    FloatingPointCheck();
    instance = this;
}

JikesAPI * JikesAPI::getInstance()
{
    return instance;
}


JikesAPI::~JikesAPI()
{
    delete option;
}

char ** JikesAPI::parseOptions(int argc, char **argv)
{

    delete option;

    ArgumentExpander *args=new ArgumentExpander(argc, argv);
    Option *opt = new Option(*args);
    option = opt;
    int n=args->argc - opt->first_file_index;

    if(n <= 0)
    {
        return NULL;
    }
    else
    {
        char **res=new char*[n+1];
        for(int i=0;i<n;i++)
        {
            const char *o=args->argv[opt->first_file_index+i];
            if(o)
            {
                res[i] = new char[strlen(o)+1];
                strcpy(res[i],o);
            } else
            {
                res[i] = NULL;
                break;
            }
        }
        res[n] = NULL;
        return res;
    }
}

JikesOption *JikesAPI::getOptions()
{
    return option;
}

/**
 * Compile given list of files.
 */
int JikesAPI::compile(char **filenames)
{
    // FIXME: why dont we just create a Control on the stack here?
    Control *control = new Control(filenames , *((Option*)option));
    int return_code = control -> return_code;

    delete control;

    return return_code;
}

/**
 * This method will be called for each error reported.
 */
void JikesAPI::reportError(JikesError *error)
{
    Coutput << error->getErrorReport();
    Coutput.flush();
}

const char *JikesError::getSeverityString() 
{
    switch(getSeverity())
    {
    case JIKES_ERROR  : return "Error"   ;
    case JIKES_WARNING: return "Warning" ;
    case JIKES_CAUTION: return "Caution" ;
    default: return "Unknown";
    }
}


/*
 * Default file stat/reading and writting:
 * The following provide the base classes and the
 * default implamentations to read files from the file systems.
 */


/**
 *  By default just ask the system for stat information.
 */
int JikesAPI::stat(const char *fileName,struct stat *status)
{
    return ::SystemStat(fileName,status);
}

/**
 * By default return an object that reads from the file system.
 */
JikesAPI::FileReader *JikesAPI::read(const char *fileName)
{
    FileReader  *result  =  new DefaultFileReader(fileName);

    // NB even if a file is empty (0 bytes)
    // This will return a pointer to 0 length array
    // and should not be NULL.
    if(result && (result->getBuffer() == NULL))  
    {							    
        delete result;				
        result  = NULL;
    }
    return result;
}

/**
 * By Default return an object that reads from the file system.
 */
JikesAPI::FileWriter *JikesAPI::write(const char *fileName, size_t bytes) 
{
    FileWriter *result  = new DefaultFileWriter(fileName, bytes);
    
    if(result && (!result->isValid()))
    {
        delete result;
        result = NULL;
    }
    return result;
}

/**
 * The Write() mewthod on all WriteObject(s) makes sure that we do not
 * send too much data to the virtual function.
 */
size_t JikesAPI::FileWriter::write(const unsigned char *data,size_t size)
{
    size_t result   = 0;

    if(size <= maxSize)
    {
        result   = this->doWrite(data,size);
        maxSize  -= size;
    }
   
    return(result);
}


#ifdef UNIX_FILE_SYSTEM
// The following methods define UNIX specific methods for
// reading files from the file system. WINDOWS method follow in
// the subsequent section.


/**
 * When the ReadObject is created. read the whole file into a buffer
 * held by the object.
 */ 
DefaultFileReader::DefaultFileReader(const char *fileName)
{
    size   = 0;
    buffer = NULL;

    struct stat status;
    JikesAPI::getInstance()->stat(fileName, &status);
    size   = status.st_size;

    FILE *srcfile = ::SystemFopen(fileName, "rb");
    if (srcfile != NULL)
    {
        buffer = new char[size];
        size_t numread = ::SystemFread(const_cast<char*>(buffer), sizeof(char), size, srcfile);
        assert(numread == size);
        fclose(srcfile);
    }
}


/**
 * When the ReadObject is destroyed the release the memory buffer.
 */
DefaultFileReader::~DefaultFileReader()
{
    delete [] buffer;
}


/**
 * Open a standard FILE pointer and get ready to write.
 */
DefaultFileWriter::DefaultFileWriter(const char *fileName,size_t maxSize):
    FileWriter(maxSize)
{
    valid  = false;
    file = ::SystemFopen(fileName, "wb");
    if (file  ==  (FILE *) NULL)
        return;
    valid  = true;
}

/**
 * Close the file when the write object is destroyed.
 */
DefaultFileWriter::~DefaultFileWriter()
{
    fclose(file);
}

bool DefaultFileWriter::isValid()  {return(valid);}

/**
 * Copy the data buffer to the file.
 */
size_t DefaultFileWriter::doWrite(const unsigned char *data,size_t size)
{
    return fwrite(data, sizeof(u1),size, file);
}

#elif defined(WIN32_FILE_SYSTEM)




// Open a windows file and map the file onto processor memory.
DefaultFileReader::DefaultFileReader(const char *fileName)
{
    size   = 0;
    buffer = NULL;

    srcfile = CreateFile(fileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
    if (srcfile != INVALID_HANDLE_VALUE)
    {
        mapfile = CreateFileMapping(srcfile, NULL, PAGE_READONLY, 0, 0, NULL);
        if (mapfile != INVALID_HANDLE_VALUE)
        {
            buffer = (char *) MapViewOfFile(mapfile, FILE_MAP_READ, 0, 0, 0);
            size = (size_t)GetFileSize(srcfile, NULL);
        }
    }
}


// When the ReadObject is destroyed close all the associated files.
// and unmap the memory.
DefaultFileReader::~DefaultFileReader()
{
    if (srcfile != INVALID_HANDLE_VALUE)
    {
        if (mapfile != INVALID_HANDLE_VALUE)
        {
            if (buffer)
            {
                UnmapViewOfFile((void *) buffer);
            }
            CloseHandle(mapfile);
        }
        CloseHandle(srcfile);
    }
}


// Create a windows file and map the file onto processor memory.
DefaultFileWriter::DefaultFileWriter(const char *fileName,size_t maxSize):
    FileWriter(maxSize)
{
    valid  = false;
    dataWritten    = 0;
    file           = CreateFile(fileName, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE)
        return;

    mapfile        = CreateFileMapping(file, NULL, PAGE_READWRITE, 0, maxSize, NULL);
    if (mapfile == INVALID_HANDLE_VALUE)
        return;

    string_buffer  = (u1 *) MapViewOfFile(mapfile, FILE_MAP_WRITE, 0, 0, maxSize);
    assert(string_buffer);
    valid  = true;
}

// When the WriteObject is destroyed close all the associated files,
// Thus writting the mory to the file system.
DefaultFileWriter::~DefaultFileWriter()
{
    if (file != INVALID_HANDLE_VALUE)
    {
        if (mapfile != INVALID_HANDLE_VALUE)
        {
            UnmapViewOfFile(string_buffer);
            CloseHandle(mapfile);
        }
        CloseHandle(file);
    }
}

bool DefaultFileWriter::isValid()  
{
    return(valid);
}

// Copy the input data to the mapped memory.
size_t DefaultFileWriter::doWrite(const unsigned char *data,size_t size)
{
    memmove(&string_buffer[dataWritten], data, size * sizeof(u1));
    dataWritten += size;
    return(size);
}

#endif



