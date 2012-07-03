// $Id: stream.cpp,v 1.19 1999/11/01 03:22:20 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include <ctype.h>
#include "stream.h"
#include "code.h"
#include "zip.h"
#include "symbol.h"
#include "control.h"
#include "semantic.h"

#ifdef HAVE_LIB_ICU_UC
# include <ucnv.h>
#endif

wchar_t *LexStream::KeywordName(int kind)
{
    switch(kind)
    {
        case TK_abstract:     return StringConstant::US_abstract; break;
        case TK_boolean:      return StringConstant::US_boolean;  break;
        case TK_break:        return StringConstant::US_break;    break;
        case TK_byte:         return StringConstant::US_byte;     break;
        case TK_case:         return StringConstant::US_case; break;
        case TK_catch:        return StringConstant::US_catch; break;
        case TK_char:         return StringConstant::US_char; break;
        case TK_class:        return StringConstant::US_class; break;
        case TK_const:        return StringConstant::US_const; break;
        case TK_continue:     return StringConstant::US_continue; break;
        case TK_default:      return StringConstant::US_default; break;
        case TK_do:           return StringConstant::US_do; break;
        case TK_double:       return StringConstant::US_double; break;
        case TK_else:         return StringConstant::US_else; break;
        case TK_extends:      return StringConstant::US_extends; break;
        case TK_false:        return StringConstant::US_false; break;
        case TK_final:        return StringConstant::US_final; break;
        case TK_finally:      return StringConstant::US_finally; break;
        case TK_float:        return StringConstant::US_float; break;
        case TK_for:          return StringConstant::US_for; break;
        case TK_goto:         return StringConstant::US_goto; break;
        case TK_if:           return StringConstant::US_if; break;
        case TK_implements:   return StringConstant::US_implements; break;
        case TK_import:       return StringConstant::US_import; break;
        case TK_instanceof:   return StringConstant::US_instanceof; break;
        case TK_int:          return StringConstant::US_int; break;
        case TK_interface:    return StringConstant::US_interface; break;
        case TK_long:         return StringConstant::US_long; break;
        case TK_native:       return StringConstant::US_native; break;
        case TK_new:          return StringConstant::US_new; break;
        case TK_null:         return StringConstant::US_null; break;
        case TK_package:      return StringConstant::US_package; break;
        case TK_private:      return StringConstant::US_private; break;
        case TK_protected:    return StringConstant::US_protected; break;
        case TK_public:       return StringConstant::US_public; break;
        case TK_return:       return StringConstant::US_return; break;
        case TK_short:        return StringConstant::US_short; break;
        case TK_static:       return StringConstant::US_static; break;
        case TK_strictfp:     return StringConstant::US_strictfp; break;
        case TK_super:        return StringConstant::US_super; break;
        case TK_switch:       return StringConstant::US_switch; break;
        case TK_synchronized: return StringConstant::US_synchronized; break;
        case TK_this:         return StringConstant::US_this; break;
        case TK_throw:        return StringConstant::US_throw; break;
        case TK_throws:       return StringConstant::US_throws; break;
        case TK_transient:    return StringConstant::US_transient; break;
        case TK_true:         return StringConstant::US_true; break;
        case TK_try:          return StringConstant::US_try; break;
        case TK_void:         return StringConstant::US_void; break;
        case TK_volatile:     return StringConstant::US_volatile; break;
        case TK_while:        return StringConstant::US_while; break;

        case TK_PLUS_PLUS:                  return StringConstant::US_PLUS_PLUS; break;
        case TK_MINUS_MINUS:                return StringConstant::US_MINUS_MINUS; break;
        case TK_EQUAL_EQUAL:                return StringConstant::US_EQUAL_EQUAL; break;
        case TK_LESS_EQUAL:                 return StringConstant::US_LESS_EQUAL; break;
        case TK_GREATER_EQUAL:              return StringConstant::US_GREATER_EQUAL; break;
        case TK_NOT_EQUAL:                  return StringConstant::US_NOT_EQUAL; break;
        case TK_LEFT_SHIFT:                 return StringConstant::US_LEFT_SHIFT; break;
        case TK_RIGHT_SHIFT:                return StringConstant::US_RIGHT_SHIFT; break;
        case TK_UNSIGNED_RIGHT_SHIFT:       return StringConstant::US_UNSIGNED_RIGHT_SHIFT; break;
        case TK_PLUS_EQUAL:                 return StringConstant::US_PLUS_EQUAL; break;
        case TK_MINUS_EQUAL:                return StringConstant::US_MINUS_EQUAL; break;
        case TK_MULTIPLY_EQUAL:             return StringConstant::US_MULTIPLY_EQUAL; break;
        case TK_DIVIDE_EQUAL:               return StringConstant::US_DIVIDE_EQUAL; break;
        case TK_AND_EQUAL:                  return StringConstant::US_AND_EQUAL; break;
        case TK_OR_EQUAL:                   return StringConstant::US_OR_EQUAL; break;
        case TK_XOR_EQUAL:                  return StringConstant::US_XOR_EQUAL; break;
        case TK_REMAINDER_EQUAL:            return StringConstant::US_REMAINDER_EQUAL; break;
        case TK_LEFT_SHIFT_EQUAL:           return StringConstant::US_LEFT_SHIFT_EQUAL; break;
        case TK_RIGHT_SHIFT_EQUAL:          return StringConstant::US_RIGHT_SHIFT_EQUAL; break;
        case TK_UNSIGNED_RIGHT_SHIFT_EQUAL: return StringConstant::US_UNSIGNED_RIGHT_SHIFT_EQUAL; break;
        case TK_OR_OR:                      return StringConstant::US_OR_OR; break;
        case TK_AND_AND:                    return StringConstant::US_AND_AND; break;

        case TK_PLUS:                       return StringConstant::US_PLUS; break;
        case TK_MINUS:                      return StringConstant::US_MINUS; break;
        case TK_NOT:                        return StringConstant::US_NOT; break;
        case TK_REMAINDER:                  return StringConstant::US_REMAINDER; break;
        case TK_XOR:                        return StringConstant::US_XOR; break;
        case TK_AND:                        return StringConstant::US_AND; break;
        case TK_MULTIPLY:                   return StringConstant::US_MULTIPLY; break;
        case TK_OR:                         return StringConstant::US_OR; break;
        case TK_TWIDDLE:                    return StringConstant::US_TWIDDLE; break;
        case TK_DIVIDE:                     return StringConstant::US_DIVIDE; break;
        case TK_GREATER:                    return StringConstant::US_GREATER; break;
        case TK_LESS:                       return StringConstant::US_LESS; break;
        case TK_LPAREN:                     return StringConstant::US_LPAREN; break;
        case TK_RPAREN:                     return StringConstant::US_RPAREN; break;
        case TK_LBRACE:                     return StringConstant::US_LBRACE; break;
        case TK_RBRACE:                     return StringConstant::US_RBRACE; break;
        case TK_LBRACKET:                   return StringConstant::US_LBRACKET; break;
        case TK_RBRACKET:                   return StringConstant::US_RBRACKET; break;
        case TK_SEMICOLON:                  return StringConstant::US_SEMICOLON; break;
        case TK_QUESTION:                   return StringConstant::US_QUESTION; break;
        case TK_COLON:                      return StringConstant::US_COLON; break;
        case TK_COMMA:                      return StringConstant::US_COMMA; break;
        case TK_DOT:                        return StringConstant::US_DOT; break;
        case TK_EQUAL:                      return StringConstant::US_EQUAL; break;
        case TK_EOF:                        return StringConstant::US_EOF; break;
        default:                            break;
    }

    return StringConstant::US_EMPTY;
}


LexStream::~LexStream()
{
#ifdef TEST
    control.line_count += (file_read * (line_location.Length() - 3));
#endif

    DestroyInput();

    delete [] columns;
    delete [] comment_buffer;
    comment_buffer = NULL;
}


//
//
//
::LiteralSymbol *LexStream::LiteralSymbol(TokenIndex i)
{
    Symbol *symbol = tokens[i].additional_info.symbol;
    return (symbol && (Kind(i) != TK_LBRACE) ? symbol -> LiteralCast() : (::LiteralSymbol *) NULL);
}


//
//
//
::NameSymbol *LexStream::NameSymbol(TokenIndex i)
{
    Symbol *symbol = tokens[i].additional_info.symbol;
    return (symbol && (Kind(i) != TK_LBRACE) ? symbol -> NameCast() : (::NameSymbol *) NULL);
}


//
// Name of input file where the token appeared.
//
char *LexStream::FileName() { return file_symbol -> FileName(); }
size_t LexStream::FileNameLength() { return file_symbol -> FileNameLength(); }


void LexStream::InitializeColumns()
{
    if (! columns)
    {
        columns = new unsigned short[token_stream.Length()];

        int start = 0,
            k = 1;

        for (size_t i = 0; i < input_buffer_length; i++)
        {
            if (Code::IsNewline(input_buffer[i]))
                start = i;
            else
            {
                if (input_buffer[i] == U_HORIZONTAL_TAB)
                {
                    int offset = (i - start) - 1;
                    start -= ((Tab::TabSize() - 1) - offset % Tab::TabSize());
                }
                else if (tokens[k].Location() == i)
                {
                    int col = i - start;
                    columns[k++] = (col < USHRT_MAX ? col : 0);
                }
            }
        }
    }

    return;
}


//
//
//
void LexStream::CompressSpace()
{
    tokens = token_stream.Array();
    if (control.option.dump_errors)
        InitializeColumns();
    comments = comment_stream.Array();
    locations = line_location.Array();
    types = type_index.Array();

    return;
}


//
// Find and return the index of the first comment that immediately
// follows tok. Return 0 if there is not a comment that immediately
// follows tok.
//
LexStream::CommentIndex LexStream::FirstComment(TokenIndex tok)
{
    unsigned location = Location(tok);
    int lo = 0,
        hi = comment_stream.Length() - 1,
        i = 0;

    if (lo < hi)
    {
        do
        {
            int mid = (lo + hi) / 2;

            if (comment_stream[mid].location < location)
                 lo = mid + 1;
            else hi = mid - 1;
        } while (lo < hi);

        //
        // at this stage lo == hi
        //
        i = (comment_stream[lo].location > location ? lo : lo + 1);
    }

    return (i < comment_stream.Length() && comment_stream[i].previous_token == tok ? i : 0);
}


unsigned LexStream::FindLine(unsigned location)
{
    int lo = 0,
        hi = line_location.Length() - 1;

assert(locations);
    //
    // we can place the exit test at the bottom of the loop
    // since the line_location array will always contain at least
    // one element.
    //
    do
    {
        int mid = (lo + hi) / 2;

        if (locations[mid] == location)
            return mid;
        if (locations[mid] < location)
             lo = mid + 1;
        else hi = mid - 1;
    } while (lo < hi);

    return (locations[lo] > location ? lo - 1 : lo);
}


void LexStream::ReadInput()
{
    if (file_symbol -> buffer)
    {
        ProcessInput(file_symbol -> buffer, strlen(file_symbol -> buffer));
    }
    else if (file_symbol -> IsZip()) {
        ZipFile *zipfile = new ZipFile(file_symbol);

        if (zipfile -> Buffer() == NULL)
        {
            fprintf(stderr, "chaos: Don\'t know how to process compressed (\".java\") source in a zip file\n");
            assert(false);
        }
        else if (! file_symbol -> lex_stream) // Once the zip file is loaded, it never changes. So, we only read it the first time
        {
            file_symbol -> lex_stream = this;
            ProcessInput(zipfile -> Buffer(), file_symbol -> uncompressed_size);
        }
        delete zipfile;
    }
    else
    {
        struct stat status;
        ::SystemStat(FileName(), &status);

        file_symbol -> mtime = status.st_mtime; // actual time stamp of file read
        file_symbol -> lex_stream = this;

#ifdef UNIX_FILE_SYSTEM
        FILE *srcfile = ::SystemFopen(FileName(), "r");
        if (srcfile != NULL)
        {
            char *buffer = new char[status.st_size];
            size_t file_size = ::SystemFread(buffer, sizeof(char), status.st_size, srcfile);
            fclose(srcfile);
            ProcessInput(buffer, file_size);
            delete [] buffer;
        }
#elif defined(WIN32_FILE_SYSTEM)
#include <windows.h>
        HANDLE srcfile = CreateFile(FileName(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
        if (srcfile != INVALID_HANDLE_VALUE)
        {
            HANDLE mapfile = CreateFileMapping(srcfile, NULL, PAGE_READONLY, 0, 0, NULL);
            if (mapfile != INVALID_HANDLE_VALUE)
            {
                char *buffer = (char *) MapViewOfFile(mapfile, FILE_MAP_READ, 0, 0, 0);
                DWORD file_size = GetFileSize(srcfile, NULL);
                ProcessInput(buffer, file_size);
                if (buffer)
                    UnmapViewOfFile(buffer);
                CloseHandle(mapfile);
            }

            CloseHandle(srcfile);
        }
#endif
    }

    initial_reading_of_input = false;

    return;
}

void LexStream::RereadInput()
{
    if (input_buffer) // if input already available, do nothing
        ;
#ifdef TEST
    else if (file_symbol -> buffer)
    {
      fprintf(stderr, "chaos: Don\'t know how to RereadInput a buffer\n");
      assert(false);
    }
#endif
    else if (file_symbol -> IsZip())
    {
        ZipFile *zipfile = new ZipFile(file_symbol);

        if (zipfile -> Buffer() == NULL)
        {
            fprintf(stderr, "chaos: Don\'t know how to process compressed (\".java\") source in a zip file\n");
            assert(false);
        }
        else ProcessInput(zipfile -> Buffer(), file_symbol -> uncompressed_size);
        delete zipfile;
    }
    else
    {
        struct stat status;
        ::SystemStat(FileName(), &status);

        if (status.st_mtime == file_symbol -> mtime)
        {
#ifdef UNIX_FILE_SYSTEM
            FILE *srcfile = ::SystemFopen(FileName(), "r");
            if (srcfile != NULL)
            {
                char *buffer = new char[status.st_size];
                size_t file_size = ::SystemFread(buffer, sizeof(char), status.st_size, srcfile);
                fclose(srcfile);
                ProcessInput(buffer, file_size);
                delete [] buffer;
            }
#elif defined(WIN32_FILE_SYSTEM)
            HANDLE srcfile = CreateFile(FileName(), GENERIC_READ, FILE_SHARE_READ,
                                        NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
            if (srcfile != INVALID_HANDLE_VALUE)
            {
                HANDLE mapfile = CreateFileMapping(srcfile, NULL, PAGE_READONLY, 0, 0, NULL);
                if (mapfile != INVALID_HANDLE_VALUE)
                {
                    char *buffer = (char *) MapViewOfFile(mapfile, FILE_MAP_READ, 0, 0, 0);
                    DWORD file_size = GetFileSize(srcfile, NULL);
                    ProcessInput(buffer, file_size);
                    if (buffer)
                        UnmapViewOfFile(buffer);
                    CloseHandle(mapfile);
                }

                CloseHandle(srcfile);
            }
#endif
        }
        else
        {
            // TODO: File has changed !!!
        }
    }

    return;
}


int LexStream::hexvalue(wchar_t ch)
{
    switch(ch)
    {
    case U_a: case U_A:
        return 10;
    case U_b: case U_B:
        return 11;
    case U_c: case U_C:
        return 12;
    case U_d: case U_D:
        return 13;
    case U_e: case U_E:
        return 14;
    case U_f: case U_F:
        return 15;
    default:
        return ch - U_0;
    }
}

//
// Read filesize  characters from srcfile, convert them to unicode, and
// store them in input_buffer.
//
void LexStream::ProcessInput(char *buffer, long filesize)
{
#ifdef HAVE_LIB_ICU_UC
    LexStream::ProcessInputUnicode(buffer,filesize);
#else
    LexStream::ProcessInputAscii(buffer, filesize);
#endif
}

//
// Read file_size Ascii characters from srcfile, convert them to unicode and
// store them in input_buffer.
//
void LexStream::ProcessInputAscii(char *buffer, long filesize)
{
#ifdef TEST
    file_read++;
#endif

    input_buffer = new wchar_t[filesize + 4];
    wchar_t *input_ptr = input_buffer;
    *input_ptr = U_LINE_FEED; // add an initial '\n';

    if (buffer)
    {
        char *source_ptr = buffer,
             *source_tail = &(buffer[filesize - 1]); // point to last character read from the file.

        while(source_ptr <= source_tail)
        {
            *(++input_ptr) = (*source_ptr++) & 0x00ff; // The (& 0x00ff) guarantees that quantity is copied as unsigned value

            if (*input_ptr == U_CARRIAGE_RETURN)
            {
                *input_ptr = U_LINE_FEED;
                if (*source_ptr == U_LINE_FEED)
                    source_ptr++;
            }
            else if (*input_ptr == U_BACKSLASH)
            {
                if (*source_ptr == U_BACKSLASH)
                    *(++input_ptr) = *source_ptr++;
                else if (*source_ptr == U_u)
                {
                    char *u_ptr = source_ptr;

                    for (source_ptr++; source_ptr <= source_tail && *source_ptr == U_u; source_ptr++)
                        ;
                    *input_ptr = 0;
                    int i;
                    for (i = 0; source_ptr <= source_tail && isxdigit(*source_ptr) && i < 4; i++)
                    {
                        int multiplier[4] = {4096, 256, 16, 1};

                        char ch = *source_ptr++;
                        switch(ch)
                        {
                            case U_a: case U_A:
                                *input_ptr += (10 * multiplier[i]);
                                break;
                            case U_b: case U_B:
                                *input_ptr += (11 * multiplier[i]);
                                break;
                            case U_c: case U_C:
                                *input_ptr += (12 * multiplier[i]);
                                break;
                            case U_d: case U_D:
                                *input_ptr += (13 * multiplier[i]);
                                break;
                            case U_e: case U_E:
                                *input_ptr += (14 * multiplier[i]);
                                break;
                            case U_f: case U_F:
                                *input_ptr += (15 * multiplier[i]);
                                break;
                            default:
                                *input_ptr += ((ch - U_0) * multiplier[i]);
                        }
                    }

                    if (i != 4)
                    {
                        if (initial_reading_of_input)
                            bad_tokens.Next().Initialize(StreamError::INVALID_UNICODE_ESCAPE,
                                                         (unsigned) (input_ptr - input_buffer),
                                                         (unsigned) (input_ptr - input_buffer) + (source_ptr - u_ptr));

                        source_ptr = u_ptr;
                        *input_ptr = U_BACKSLASH;
                    }
                    else if (*input_ptr == U_CARRIAGE_RETURN)
                    {
                        *input_ptr = U_LINE_FEED;
                        if (*source_ptr == U_LINE_FEED)
                            source_ptr++;
                        else if (*source_ptr == U_BACKSLASH)
                        {
                            int i;
                            for (i = 1; (source_ptr + i) <= source_tail && source_ptr[i] == U_u; i++)
                                ;
                            if (i > 1 && (source_ptr + i + 3) <= source_tail
                                      && source_ptr[i]     == U_0
                                      && source_ptr[i + 1] == U_0
                                      && source_ptr[i + 2] == U_0
                                      && source_ptr[i + 3] == U_a) // the escape sequence of \n is \u000a
                                source_ptr += (i + 4);
                        }
                    }
                }
            }
        }

        //
        // Remove all trailing spaces
        //
        while((input_ptr > input_buffer) && Code::IsSpace(*input_ptr))
            input_ptr--;
    }

    //
    // If the very last character is not CTL_Z then add CTL_Z
    //
    if (*input_ptr != U_CTL_Z)
    {
        if (*input_ptr != U_LINE_FEED)
            *(++input_ptr) = U_LINE_FEED; // if the last character is not end-of-line, add end-of-line
        *(++input_ptr) = U_CTL_Z;         // Mark end-of-file
    }
    *(++input_ptr) = U_NULL;              // add gate

    input_buffer_length = input_ptr - input_buffer;

    return;
}

#ifdef HAVE_LIB_ICU_UC
//
// Read file_size Ascii characters from srcfile, convert them to unicode, and
// store them in input_buffer.
//
void LexStream::ProcessInputUnicode(char *buffer, long filesize)
{
#ifdef TEST
    file_read++;
#endif

#ifdef HAVE_LIB_ICU_UC
    input_buffer       = new wchar_t[filesize + 4 + 2];
    wchar_t *input_tail = input_buffer + filesize;
#else
    input_buffer = new wchar_t[filesize + 4];
#endif
    
    wchar_t *input_ptr = input_buffer;
    *input_ptr = U_LINE_FEED; // add an initial '\n';
    
    if(buffer)
    {
        int      escape_value;
        wchar_t *escape_ptr;
        const char *source_ptr = buffer,
            *source_tail = &(buffer[filesize - 1]); // point to last character read from the file.
        
        UnicodeLexerState saved_state;
        UnicodeLexerState state=RAW;
        bool oncemore=false;

#ifdef HAVE_LIB_ICU_UC
        UErrorCode err = ZERO_ERROR;
#endif

        while((source_ptr <= source_tail) || oncemore)
        {
#ifdef HAVE_LIB_ICU_UC
            // On each iteration we advance input_ptr maximun 2 postions.
            // Here we check if we are close to the end of input_buffer
            if(input_ptr>=input_tail)
            {
                // If this happen, reallocate it with some more space.
                // This is very rare case, which could happen if
                // one code page character is represened by several 
                // unicode characters. One of exaples of such
                // situation is unicode "surrogates".
                //
                // If such reallocation will be required, it will indeed
                // slow down compilation a bit.
                size_t cursize = input_ptr-input_buffer;
                size_t newsize = cursize+cursize/10; // add 10%
                wchar_t *tmp   = new wchar_t[newsize]; 
                memcpy(tmp, input_buffer, newsize*sizeof(wchar_t));
                delete input_buffer;
                input_buffer = tmp;
                input_tail = input_buffer + newsize;
                input_ptr  = input_buffer+cursize;
            }
#endif
            
            wchar_t ch;
            
            if(!oncemore)
            {
#ifdef HAVE_LIB_ICU_UC
                if(control.option.converter)
                    ch=ucnv_getNextUChar (control.option.converter,
                                          &source_ptr,
                                          source_tail,
                                          &err);
                else
                    ch=*source_ptr++;
                if(err!=ZERO_ERROR)
                    break;
#else
                ch=*source_ptr++;
#endif
            } else oncemore = false;
      
            switch(state)
            {
            case QUOTE:
                if(ch==U_BACKSLASH)
                {
                    *(++input_ptr) = U_BACKSLASH;
                    *(++input_ptr) = U_BACKSLASH;
                    state          = RAW;
                } else if(ch==U_u)
                {
                    escape_ptr = input_ptr;
                    state      = UNICODE_ESCAPE;
                } else
                {
                    *(++input_ptr )= U_BACKSLASH;
                    state          = RAW;
                    oncemore       = true;
                }
                break;
            case UNICODE_ESCAPE:
                if(isxdigit(ch))
                {
                    state=UNICODE_ESCAPE_DIGIT_0;
                    escape_value=hexvalue(ch)*16*16*16;
                } else if(ch!=U_u)
                {
                    if(initial_reading_of_input)
                        bad_tokens.Next().Initialize(StreamError::INVALID_UNICODE_ESCAPE,
                                                     (unsigned) (escape_ptr - input_buffer),
                                                     (unsigned) (input_ptr - input_buffer));
                }
                break;
            case UNICODE_ESCAPE_DIGIT_0:
                if(isxdigit(ch))
                {
                    state=UNICODE_ESCAPE_DIGIT_1;
                    escape_value+=hexvalue(ch)*16*16;
                } else  
                {
                    if(initial_reading_of_input)
                        bad_tokens.Next().Initialize(StreamError::INVALID_UNICODE_ESCAPE,
                                                     (unsigned) (escape_ptr - input_buffer),
                                                     (unsigned) (input_ptr - input_buffer));
                }
                break;
            case UNICODE_ESCAPE_DIGIT_1:
                if(isxdigit(ch))
                {
                    state=UNICODE_ESCAPE_DIGIT_2;
                    escape_value+=hexvalue(ch)*16;
                } else  
                {
                    if(initial_reading_of_input)
                        bad_tokens.Next().Initialize(StreamError::INVALID_UNICODE_ESCAPE,
                                                     (unsigned) (escape_ptr - input_buffer),
                                                     (unsigned) (input_ptr - input_buffer));
                }
                break;
            case UNICODE_ESCAPE_DIGIT_2:
                if(isxdigit(ch))
                {
                    ch       = escape_value+hexvalue(ch);
                    state    = saved_state;
                    saved_state = UNICODE_ESCAPE_DIGIT_2;
                    oncemore = true;
                } else  
                {
                    if(initial_reading_of_input)
                        bad_tokens.Next().Initialize(StreamError::INVALID_UNICODE_ESCAPE,
                                                     (unsigned) (escape_ptr - input_buffer),
                                                     (unsigned) (input_ptr - input_buffer));
                }
                break;
            case CR:
                if(ch==U_LINE_FEED)
                {
                    state = RAW;
                } else if(ch==U_BACKSLASH && saved_state != UNICODE_ESCAPE_DIGIT_2)
                {
                    saved_state = CR;
                    state       = QUOTE;
                } else
                {
                    state = RAW;
                    *(++input_ptr)=ch;                    
                }
                break;
            case RAW:
                if(ch==U_BACKSLASH && saved_state != UNICODE_ESCAPE_DIGIT_2)
                {
                    state       = QUOTE;
                } else if(ch == U_CARRIAGE_RETURN)
                {
                    state = CR;
                    *(++input_ptr) = U_LINE_FEED;
                } else
                {
                    *(++input_ptr)=ch;                    
                }
                saved_state = RAW;
                break;
            }
        }
    }

    //
    // If the very last character is not CTL_Z then add CTL_Z
    //
    if (*input_ptr != U_CTL_Z)
    {
        if (*input_ptr != U_LINE_FEED)
            *(++input_ptr) = U_LINE_FEED; // if the last character is not end-of-line, add end-of-line
        *(++input_ptr) = U_CTL_Z;         // Mark end-of-file
    }
    *(++input_ptr) = U_NULL;              // add gate
    
    input_buffer_length = input_ptr - input_buffer;

    return;
}
#endif

//
// This procedure uses a  quick sort algorithm to sort the stream ERRORS
// by their locations.
//
void LexStream::SortMessages()
{
     int lower,
         upper,
         lostack[32],
         histack[32];

     int top,
         i,
         j;
     StreamError pivot,
                 temp;

     top = 0;
     lostack[top] = 0;
     histack[top] = bad_tokens.Length() - 1;

     while(top >= 0)
     {
         lower = lostack[top];
         upper = histack[top];
         top--;

         while(upper > lower)
         {
             //
             // The array is most-likely almost sorted. Therefore,
             // we use the middle element as the pivot element.
             //
             i = (lower + upper) / 2;
             pivot = bad_tokens[i];
             bad_tokens[i] = bad_tokens[lower];

             //
             // Split the array section indicated by LOWER and UPPER
             // using ARRAY(LOWER) as the pivot.
             //
             i = lower;
             for (j = lower + 1; j <= upper; j++)
             {
                 if (bad_tokens[j].start_location < pivot.start_location)
                 {
                     temp = bad_tokens[++i];
                     bad_tokens[i] = bad_tokens[j];
                     bad_tokens[j] = temp;
                 }
             }
             bad_tokens[lower] = bad_tokens[i];
             bad_tokens[i] = pivot;

             top++;
             if ((i - lower) < (upper - i))
             {
                 lostack[top] = i + 1;
                 histack[top] = upper;
                 upper = i - 1;
             }
             else
             {
                 histack[top] = i - 1;
                 lostack[top] = lower;
                 lower = i + 1;
             }
         }
     }

     return;
}


//
//
//
void LexStream::PrintMessages()
{
    //
    // If control.option.dump_errors then the error messages have already been printed
    //
    if (! control.option.dump_errors)
    {
        RereadInput();

        if (control.option.errors)
        {
            char *file_name = FileName();

            Coutput << "\nFound " << NumBadTokens() << " lexical error" << (NumBadTokens() == 1 ? "" : "s")
                    << " in \""
                    << file_name
                    << "\":";

            if (! input_buffer)
            {
                int length = FileNameLength();
                wchar_t *name = new wchar_t[length + 1];
                for (int i = 0; i < length; i++)
                    name[i] = file_name[i];
                name[length] = U_NULL;
                control.system_semantic -> ReportSemError(SemanticError::CANNOT_REOPEN_FILE,
                                                          0,
                                                          0,
                                                          name);
                delete [] name;
            }
            else
            {
                for (int i = 0; i < bad_tokens.Length(); i++)
                {
                    if (FindLine(bad_tokens[i].start_location) == FindLine(bad_tokens[i].end_location))
                         PrintSmallSource(i);
                    else PrintLargeSource(i);

                    Coutput << "\n*** Lexical Error: ";

                    PrintMessage(bad_tokens[i].kind);
                }
            }
        }
        else
        {
            for (int i = 0; i < bad_tokens.Length(); i++)
                PrintEmacsMessage(i);
        }

        DestroyInput();

        Coutput.flush();
    }

    return;
}


//
//
//
void LexStream::PrintEmacsMessage(int k)
{
    int left_line_no    = FindLine(bad_tokens[k].start_location),
        left_column_no  = FindColumn(bad_tokens[k].start_location),
        right_line_no   = FindLine(bad_tokens[k].end_location),
        right_column_no = FindColumn(bad_tokens[k].end_location);

    Coutput << FileName()
            << ':' << left_line_no  << ':' << left_column_no
            << ':' << right_line_no << ':' << right_column_no
            << ":\n    Lexical: ";

    PrintMessage(bad_tokens[k].kind);

    return;
}


//
// This procedure is invoked to print a small message that may
// only span a single line. The parameter k points to the error
// message in the error structure.
//
void LexStream::PrintSmallSource(int k)
{
    int left_line_no = FindLine(bad_tokens[k].start_location);

    Coutput << "\n\n";
    Coutput.width(6);
    Coutput << left_line_no;
    Coutput << ". ";
    for (int i = this -> LineStart(left_line_no); i <= this -> LineEnd(left_line_no); i++)
        Coutput << this -> InputBuffer()[i];

    int left_column_no = FindColumn(bad_tokens[k].start_location),
        right_column_no = FindColumn(bad_tokens[k].end_location);

    Coutput.width(left_column_no + 7);
    Coutput << "";
    if (left_column_no == right_column_no)
        Coutput << '^';
    else
    {
        int offset = 0;
        for (size_t i = bad_tokens[k].start_location; i <= bad_tokens[k].end_location; i++)
        {
            if (this -> InputBuffer()[i] > 0xff)
                offset += 5;
        }

        Coutput << '<';
        Coutput.width(right_column_no - left_column_no + offset);
        Coutput.fill('-');
        Coutput << ">";
        Coutput.fill(' ');
    }

    return;
}


//
// This procedure is invoked to print a large message that may
// span more than one line. The parameter message points to the
// starting line. The parameter k points to the error message in
// the error structure.
//
void LexStream::PrintLargeSource(int k)
{
    int left_line_no    = FindLine(bad_tokens[k].start_location),
        left_column_no  = FindColumn(bad_tokens[k].start_location),
        right_line_no   = FindLine(bad_tokens[k].end_location),
        right_column_no = FindColumn(bad_tokens[k].end_location);

    if (left_line_no == right_line_no)
    {
        if (left_line_no == 0)
            Coutput << "\n";
        else
        {
            Coutput << "\n\n";
            Coutput.width(6);
            Coutput << left_line_no << ". ";
            for (int i = this -> LineStart(left_line_no); i <= this -> LineEnd(left_line_no); i++)
                Coutput << this -> InputBuffer()[i];

            int offset = 0;
            for (size_t j = bad_tokens[k].start_location; j <= bad_tokens[k].end_location; j++)
            {
                if (this -> InputBuffer()[j] > 0xff)
                    offset += 5;
            }

            Coutput.width(left_column_no + 8);
            Coutput << "<";
            Coutput.width(right_column_no - left_column_no + offset);
            Coutput.fill('-');
            Coutput << ">";
            Coutput.fill(' ');
        }
    }
    else
    {
        Coutput << "\n\n";
        Coutput.width(left_column_no + 8);
        Coutput << "<";

        int segment_size = Tab::Wcslen(input_buffer, bad_tokens[k].start_location,
                                                     LineEnd(FindLine(bad_tokens[k].start_location)));
        Coutput.width(segment_size - 1);
        Coutput.fill('-');
        Coutput << "\n";
        Coutput.fill(' ');

        Coutput.width(6);
        Coutput << left_line_no << ". ";
        for (int i = this -> LineStart(left_line_no); i <= this -> LineEnd(left_line_no); i++)
            Coutput << this -> InputBuffer()[i];

        if (right_line_no > left_line_no + 1)
        {
            Coutput.width(left_column_no + 7);
            Coutput << " ";
            Coutput << ". . .\n";
        }

        Coutput.width(6);
        Coutput << right_line_no << ". ";

        int offset = 0;
        for (int j = this -> LineStart(right_line_no); j <= this -> LineEnd(right_line_no); j++)
        {
            wchar_t c = this -> InputBuffer()[j];
            if (c > 0xff)
                offset += 5;
            Coutput << c;
        }

        Coutput.width(8);
        Coutput << "";
        Coutput.width(right_column_no - 1 + offset);
        Coutput.fill('-');
        Coutput << ">";
        Coutput.fill(' ');
    }

    return;
}


void LexStream::PrintMessage(StreamError::StreamErrorKind kind)
{
    switch(kind)
    {
        case StreamError::BAD_TOKEN:
             Coutput << "Illegal token";
             break;
        case StreamError::BAD_OCTAL_CONSTANT:
             Coutput << "Octal constant contains invalid digit";
             break;
        case StreamError::EMPTY_CHARACTER_CONSTANT:
             Coutput << "Empty character constant";
             break;
        case StreamError::UNTERMINATED_CHARACTER_CONSTANT:
             Coutput << "Character constant not properly terminated";
             break;
        case StreamError::UNTERMINATED_COMMENT:
             Coutput << "Comment not properly terminated";
             break;
        case StreamError::UNTERMINATED_STRING_CONSTANT:
             Coutput << "String constant not properly terminated";
             break;
        case StreamError::INVALID_HEX_CONSTANT:
             Coutput << "The prefix 0x must be followed by at least one hex digit";
             break;
        case StreamError::INVALID_FLOATING_CONSTANT_EXPONENT:
             Coutput << "floating-constant exponent has no digit";
             break;
        case StreamError::INVALID_UNICODE_ESCAPE:
             Coutput << "Invalid unicode escape character";
             break;
        default:
             assert(false);
    }

    Coutput << '\n';

    return;
}

