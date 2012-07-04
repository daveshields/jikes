// $Id: stream.cpp,v 1.65 2002/07/07 20:37:54 cabbey Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "stream.h"
#include "code.h"
#include "zip.h"
#include "symbol.h"
#include "control.h"
#include "semantic.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

// Class StreamError

JikesError::JikesErrorSeverity StreamError::getSeverity() 
{ 
    // Most Lexical errors are ERRORs.
    return kind >= StreamError::DEPRECATED_IDENTIFIER_ASSERT
        ? JikesError::JIKES_WARNING
        : JikesError::JIKES_ERROR; 
}

int StreamError::getLeftLineNo() { return left_line_no; }
int StreamError::getLeftColumnNo() { return left_column_no; }
int StreamError::getRightLineNo() { return right_line_no; }
int StreamError::getRightColumnNo() { return right_column_no; }

const char *StreamError::getFileName() 
{ 
    assert(lex_stream);
    return lex_stream -> FileName();   
}

const wchar_t *StreamError::getErrorMessage() 
{
    switch (kind)
    {
    case StreamError::BAD_TOKEN:
        return L"Illegal token.";
    case StreamError::EMPTY_CHARACTER_CONSTANT:
        return L"Empty character constant.";
    case StreamError::UNTERMINATED_CHARACTER_CONSTANT:
        return L"Character constant not properly terminated.";
    case StreamError::UNTERMINATED_COMMENT:
        return L"Comment not properly terminated.";
    case StreamError::UNTERMINATED_STRING_CONSTANT:
        return L"String constant not properly terminated.";
    case StreamError::INVALID_HEX_CONSTANT:
        return L"The prefix 0x must be followed by at least one hex digit.";
    case StreamError::INVALID_UNICODE_ESCAPE:
        return L"Invalid unicode escape character.";
    case StreamError::INVALID_ESCAPE_SEQUENCE:
        return L"Invalid escape sequence.";
    case StreamError::DEPRECATED_IDENTIFIER_ASSERT:
        return L"The use of \"assert\" as an identifier is deprecated,"
            L" as it is now a keyword. Use -source 1.4 if you intended "
            L" to make use of assertions.";
    case StreamError::DOLLAR_IN_IDENTIFIER:
        return L"The use of \"$\" in an identifier, while legal, is strongly "
            L"discouraged, since it can conflict with compiler-generated "
            L"names. If you are trying to access a nested type, use \".\" "
            L"instead of \"$\".";
    default:
        assert(false);
    }

    return L"Unknown Error";
}

bool StreamError::emacs_style_report = false;

const wchar_t *StreamError::getErrorReport() 
{
    /*
     * We need to use this lazy initialization,
     * because we can't to it in Initialize() method. Reason
     * is that Find* methods are unusable until
     * LexStream::CompressSpace is called, which
     * does not happen until later after scanning is done
     * and all errors are reported.
     * (lord)
     */
    if (! initialized)
    {
        left_line_no    = lex_stream->FindLine   ( start_location );
        left_column_no  = lex_stream->FindColumn ( start_location );
        right_line_no   = lex_stream->FindLine   ( end_location   );
        right_column_no = lex_stream->FindColumn ( end_location   );
        initialized     = true;
    }

    return emacs_style_report ? emacsErrorString() : regularErrorString();
}

wchar_t *StreamError::emacsErrorString()
{
    ErrorString s;

    s << getFileName()
      << ':' << left_line_no  << ':' << left_column_no
      << ':' << right_line_no << ':' << right_column_no
      << ": Lexical: " << getErrorMessage();
    
    return s.Array();    
}


wchar_t *StreamError::regularErrorString() 
{
    ErrorString s;

    assert(lex_stream);
    if (left_line_no == right_line_no)
        PrintSmallSource(s);
    else 
        PrintLargeSource(s);
    
    s << "\n*** Lexical " << getSeverityString() << ": "
      << getErrorMessage();
        
    return s.Array();
}

//
// This procedure is invoked to print a small message that may
// only span a single line. The parameter k points to the error
// message in the error structure.
//
void StreamError::PrintSmallSource(ErrorString &s)
{
    s << "\n\n";
    s.width(6);
    s << left_line_no;
    s << ". ";
    for (int i = lex_stream->LineStart(left_line_no); i <= lex_stream->LineEnd(left_line_no); i++)
        s << lex_stream->InputBuffer()[i];

    s.width(left_column_no + 7);
    s << "";
    if (left_column_no == right_column_no)
        s << '^';
    else
    {
        int offset = 0;
        for (size_t i = start_location; i <= end_location; i++)
        {
            if (lex_stream->InputBuffer()[i] > 0xff)
                offset += 5;
        }

        s << '<';
        s.width(right_column_no - left_column_no + offset);
        s.fill('-');
        s << ">";
        s.fill(' ');
    }
}


//
// This procedure is invoked to print a large message that may
// span more than one line. The parameter message points to the
// starting line. The parameter k points to the error message in
// the error structure.
//
void StreamError::PrintLargeSource(ErrorString &s)
{
    if (left_line_no == right_line_no)
    {
        if (left_line_no == 0)
            s << "\n";
        else
        {
            s << "\n\n";
            s.width(6);
            s << left_line_no << ". ";
            for (int i = lex_stream -> LineStart(left_line_no); i <= lex_stream -> LineEnd(left_line_no); i++)
                s << lex_stream -> InputBuffer()[i];

            int offset = 0;
            for (size_t j = start_location; j <= end_location; j++)
            {
                if (lex_stream -> InputBuffer()[j] > 0xff)
                    offset += 5;
            }

            s.width(left_column_no + 8);
            s << "<";
            s.width(right_column_no - left_column_no + offset);
            s.fill('-');
            s << ">";
            s.fill(' ');
        }
    }
    else
    {
        s << "\n\n";
        s.width(left_column_no + 8);
        s << "<";
        
        int segment_size = Tab::Wcslen(lex_stream->input_buffer, start_location,
                                       lex_stream->LineEnd(lex_stream->FindLine(start_location)));
        s.width(segment_size - 1);
        s.fill('-');
        s << "\n";
        s.fill(' ');

        s.width(6);
        s << left_line_no << ". ";
        for (int i = lex_stream -> LineStart(left_line_no); i <= lex_stream -> LineEnd(left_line_no); i++)
            s << lex_stream -> InputBuffer()[i];

        if (right_line_no > left_line_no + 1)
        {
            s.width(left_column_no + 7);
            s << " ";
            s << ". . .\n";
        }

        s.width(6);
        s << right_line_no << ". ";

        int offset = 0;
        for (int j = lex_stream -> LineStart(right_line_no); j <= lex_stream -> LineEnd(right_line_no); j++)
        {
            wchar_t c = lex_stream -> InputBuffer()[j];
            if (c > 0xff)
                offset += 5;
            s << c;
        }

        s.width(8);
        s << "";
        s.width(right_column_no - 1 + offset);
        s.fill('-');
        s << ">";
        s.fill(' ');
    }
}

void StreamError::Initialize(StreamErrorKind kind_, unsigned start_location_, unsigned end_location_, LexStream *l)
{
    kind           = kind_           ;
    start_location = start_location_ ;
    end_location   = end_location_   ;
    lex_stream     = l               ;
}

StreamError::StreamError():initialized(false)
{
}


// Class Stream

Stream::Stream()
:   input_buffer(NULL),
    input_buffer_length(0)
#if defined(HAVE_LIBICU_UC)
    ,_decoder(NULL)
#elif defined(HAVE_ICONV_H)
    ,_decoder((iconv_t)-1)
#endif
{
}

Stream::~Stream()
{
    DestroyInput();
#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)
    DestroyEncoding();
#endif
}

#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

// This method will return true is the given encoding
// can be supported, it is static because we need to
// be able to query encodings without an instance.

bool Stream::IsSupportedEncoding(char* encoding)
{
    // Create a tmp object instead of duplicating
    // the code in SetEncoding and DestroyEncoding
    Stream* tmp = new Stream();
    bool supported = tmp->SetEncoding(encoding);
    delete tmp;
    return supported;
}

bool Stream::SetEncoding(char* encoding)
{
    assert(encoding);
    DestroyEncoding();

#if defined(HAVE_LIBICU_UC)
    UErrorCode err = U_ZERO_ERROR;
    _decoder = ucnv_open(encoding, &err);
#elif defined(HAVE_ICONV_H)
    _decoder = iconv_open(JIKES_ICONV_ENCODING, encoding);
#endif

    return HaveDecoder();
}

void Stream::DestroyEncoding()
{
    if (HaveDecoder())
    {
#if defined(HAVE_LIBICU_UC)
        ucnv_close(_decoder);
        _decoder = NULL;
#elif defined(HAVE_ICONV_H)
        iconv_close(_decoder);
        _decoder = (iconv_t)-1;
#endif
    }
}


// FIXME: We may want to inline this next method

// nah... I wanna get rid of this method instead.

wchar_t
Stream::DecodeNextCharacter() {
    const char *before = source_ptr;
    wchar_t next;
    error_decode_next_character = false;

#if defined(HAVE_LIBICU_UC)

    if (!HaveDecoder()) {
        return (wchar_t) *source_ptr++;
    }

    UErrorCode err = U_ZERO_ERROR;

    next=ucnv_getNextUChar(_decoder,
                          &source_ptr,
                          source_tail+1,
                          &err);

    if (U_FAILURE(err))
    {
        fprintf(stderr,"Conversion error: %s at byte %d\n", 
            u_errorName(err),
            int(before - data_buffer)
        );
        error_decode_next_character = true;
        return 0;
    }

#elif defined(HAVE_ICONV_H)

    if (!HaveDecoder()) {
        // you can't just cast a char to a wchar_t, since that would
        // sign extend the results, which if wchar_t is 4 bytes will
        // lead the parser to segfault because it calculates a table
        // offset based on the char.
        return (wchar_t) ((*source_ptr++) & 0x00FF);
    }

    wchar_t * chp = &next;
    size_t chl = sizeof(wchar_t);
    size_t   srcl = 1;

 try_it_again:

    size_t n = iconv(_decoder,
# ifdef HAVE_ERROR_CALL_ICONV_CONST
                    (char **)
# endif // HAVE_ERROR_CALL_ICONV_CONST
                    &source_ptr, &srcl,
                    (char **)&chp, &chl);

    if (n == (size_t) -1)
    {
        if (errno == EINVAL && before + srcl + 1 <= source_tail) {
            srcl++; //we're on a multibyte input and it didn't fit in srcl
            goto try_it_again; //so we increase the window if there is space
            // and try again. This is the ultimate hack. I hate it.
        }
        else
        {
            fprintf(stderr,"Charset conversion error at offset %d: ",
                    int(before - data_buffer));
            perror("");
            error_decode_next_character = true;
            return 0;
        }
    }

# if JIKES_ICONV_NEEDS_BYTE_SWAP
    char tmp;
    char *targ = (char *)chp;
#  if SIZEOF_WCHAR_T == 2
    tmp = targ[0];
    targ[0]=targ[1];
    targ[1]=tmp;
#  elif SIZEOF_WCHAR_T == 4
    tmp = targ[0];
    targ[0]=targ[3];
    targ[3]=tmp;
    tmp=targ[1];
    targ[1]=targ[2];
    targ[2]=tmp;
#  else
    assert(0 && "sizeof(wchar_t) is not one I can cope with, this should never have got past configure!!");
#  endif //sizeof(wchar_t)

# endif //byteswap

#endif //iconv

    if (before == source_ptr)
    {
        //End of conversion
        error_decode_next_character = true;
        return 0;
    }

    return next;
}

#endif // defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)


// Class LexStream

LexStream::LexStream(Control &control_, FileSymbol *file_symbol_) : file_symbol(file_symbol_),
#ifdef JIKES_DEBUG
    file_read(false),
#endif
    tokens(NULL),
    columns(NULL),
    token_stream(12, 16),
    comments(NULL),
    comment_stream(10, 8),
    locations(NULL),
    line_location(12, 8),
    initial_reading_of_input(true),
    comment_buffer(NULL),
    control(control_)
{
    StreamError::emacs_style_report=!control_.option.errors;
}

wchar_t *LexStream::KeywordName(int kind)
{
    switch (kind)
    {
        case TK_abstract:     return StringConstant::US_abstract; break;
        case TK_assert:       return StringConstant::US_assert; break;
        case TK_boolean:      return StringConstant::US_boolean; break;
        case TK_break:        return StringConstant::US_break; break;
        case TK_byte:         return StringConstant::US_byte; break;
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
#ifdef JIKES_DEBUG
    if (file_read)
        control.line_count += (line_location.Length() - 3);
#endif

    DestroyInput();

    delete [] columns;
    columns = NULL;
}


//
//
//
class LiteralSymbol *LexStream::LiteralSymbol(TokenIndex i)
{
    Symbol *symbol = tokens[i].additional_info.symbol;
    return (symbol && (Kind(i) != TK_LBRACE) ?
        symbol -> LiteralCast() :
            (class LiteralSymbol *) NULL);
}


//
//
//
class NameSymbol *LexStream::NameSymbol(TokenIndex i)
{
    Symbol *symbol = tokens[i].additional_info.symbol;
    return (symbol && (Kind(i) != TK_LBRACE) ?
        symbol -> NameCast() :
            (class NameSymbol *) NULL);
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
    tokens    = token_stream   .Array();
    comments  = comment_stream .Array();
    locations = line_location  .Array();
    types     = type_index     .Array();
    
    if (control.option.dump_errors)
        InitializeColumns();
    
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
        JikesAPI::getInstance()->stat(FileName(), &status);

        file_symbol -> mtime = status.st_mtime; // actual time stamp of file read
        file_symbol -> lex_stream = this;


        JikesAPI::FileReader  *file = JikesAPI::getInstance()->read(FileName());
        if (file)
        {
            ProcessInput(file->getBuffer(),file->getBufferSize());
            delete file;
        }
    }

    initial_reading_of_input = false;

    return;
}

void LexStream::RereadInput()
{
    if (input_buffer) // if input already available, do nothing
        ;
#ifdef JIKES_DEBUG
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
        JikesAPI::getInstance()->stat(FileName(), &status);

        if (status.st_mtime == file_symbol -> mtime)
        {
           JikesAPI::FileReader  *file = JikesAPI::getInstance()->read(FileName());
           if (file)
           {
               ProcessInput(file->getBuffer(),file->getBufferSize());
               delete file;
           }
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
    switch (ch)
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
// Store/convert filesize bytes from a file in the input_buffer.
//

#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

void LexStream::ProcessInput(const char *buffer, long filesize)
{
    LexStream::ProcessInputUnicode(buffer,filesize);
}

#else // defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

void LexStream::ProcessInput(const char *buffer, long filesize)
{
    LexStream::ProcessInputAscii(buffer, filesize);
}

void LexStream::ProcessInputAscii(const char *buffer, long filesize)
{
#ifdef JIKES_DEBUG
    file_read = true;
#endif

    wchar_t *input_ptr = AllocateInputBuffer( filesize );
    *input_ptr = U_LINE_FEED; // add an initial '\n';

    if (buffer)
    {
        InitializeDataBuffer(buffer, filesize);

        while (source_ptr <= source_tail)
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
                    const char *u_ptr = source_ptr;

                    for (source_ptr++; source_ptr <= source_tail && *source_ptr == U_u; source_ptr++)
                        ;
                    *input_ptr = 0;
                    int i;
                    for (i = 0; source_ptr <= source_tail && isxdigit(*source_ptr) && i < 4; i++)
                    {
                        int multiplier[4] = {4096, 256, 16, 1};

                        const char ch = *source_ptr++;
                        switch (ch)
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
                            ReportMessage(StreamError::INVALID_UNICODE_ESCAPE,
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
        while ((input_ptr > input_buffer) && Code::IsSpace(*input_ptr))
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

#endif // defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)



#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

void LexStream::ProcessInputUnicode(const char *buffer, long filesize)
{
    //fprintf(stderr,"LexStream::ProcessInputUnicode called.\n");
#ifdef JIKES_DEBUG
    file_read = true;
#endif

    wchar_t *input_ptr = AllocateInputBuffer( filesize );
    wchar_t *input_tail = input_ptr + filesize;
    *input_ptr = U_LINE_FEED; // add an initial '\n';

    if (buffer)
    {
        int escape_value = 0;
        wchar_t *escape_ptr = NULL;

        UnicodeLexerState saved_state = START;
        UnicodeLexerState state = START;
        bool oncemore = false;

        // If oncemore is true, ch holds the current character, otherwise
        // it is updated to the next character
        wchar_t ch = 0;

        if (control.option.encoding)
        {
            // The encoding should have been validated by now
            bool encoding_set = SetEncoding(control.option.encoding);
            assert(encoding_set);
        }

        // init data after setting the encoding
        InitializeDataBuffer(buffer, filesize);

        while (HasMoreData() || oncemore)
        {
            // On each iteration we advance input_ptr maximun 2 positions.
            // Here we check if we are close to the end of input_buffer.
            if (input_ptr >= input_tail)
            {
                // If this happens, reallocate it with some more space.
                // This is very rare case, which could happen if
                // one code page character is represented by several 
                // unicode characters. One of exaples of such
                // situation is unicode "surrogates".
                //
                // If such reallocation will be required, it will indeed
                // slow down compilation a bit.
                size_t cursize = input_ptr - input_buffer;
                size_t newsize = cursize + cursize / 10 + 4; // add 10%
                wchar_t *tmp   = new wchar_t[newsize]; 
                memcpy (tmp, input_buffer, cursize * sizeof(wchar_t));
                delete [] input_buffer;
                input_buffer = tmp;
                input_tail = input_buffer + newsize - 1;
                input_ptr  = input_buffer + cursize;
            }
            
            if (! oncemore)
            {
                ch = DecodeNextCharacter();

                if (ErrorDecodeNextCharacter())
                {
                    break;
                }
            }
            else
            {
                oncemore = false;
            }

            switch (state)
            {

            case QUOTE:
                if (ch==U_BACKSLASH)
                {
                    *(++input_ptr) = U_BACKSLASH;
                    *(++input_ptr) = U_BACKSLASH;
                    state          = RAW;
                } else if (ch==U_u)
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
                if (isxdigit(ch))
                {
                    state = UNICODE_ESCAPE_DIGIT_0;
                    escape_value = hexvalue(ch) << 12;
                }
                else if (ch != U_u)
                {
                    if (initial_reading_of_input)
                        ReportMessage(StreamError::INVALID_UNICODE_ESCAPE,
                                      (unsigned) (escape_ptr - input_buffer),
                                      (unsigned) (input_ptr - input_buffer));
                }
                break;

            case UNICODE_ESCAPE_DIGIT_0:
                if (isxdigit(ch))
                {
                    state = UNICODE_ESCAPE_DIGIT_1;
                    escape_value += hexvalue(ch) << 8;
                }
                else
                {
                    if (initial_reading_of_input)
                        ReportMessage(StreamError::INVALID_UNICODE_ESCAPE,
                                      (unsigned) (escape_ptr - input_buffer),
                                      (unsigned) (input_ptr - input_buffer));
                }
                break;

            case UNICODE_ESCAPE_DIGIT_1:
                if (isxdigit(ch))
                {
                    state = UNICODE_ESCAPE_DIGIT_2;
                    escape_value += hexvalue(ch) << 4;
                }
                else
                {
                    if (initial_reading_of_input)
                        ReportMessage(StreamError::INVALID_UNICODE_ESCAPE,
                                      (unsigned) (escape_ptr - input_buffer),
                                      (unsigned) (input_ptr - input_buffer));
                }
                break;

            case UNICODE_ESCAPE_DIGIT_2:
                if (isxdigit(ch))
                {
                    ch       = escape_value + hexvalue(ch);
                    state    = saved_state;
                    saved_state = UNICODE_ESCAPE_DIGIT_2;
                    oncemore = true;
                }
                else
                {
                    if (initial_reading_of_input)
                        ReportMessage(StreamError::INVALID_UNICODE_ESCAPE,
                                      (unsigned) (escape_ptr - input_buffer),
                                      (unsigned) (input_ptr - input_buffer));
                }
                break;

            case CR:
                if (ch == U_LINE_FEED)
                {
                    // skip line feed if it comes right after a CR.
                    state = RAW;
                } else if (ch == U_CARRIAGE_RETURN)
                {
                    // but if CR follows CR then the second CR is a
                    // line feed too (and note that state=CR still, afterwards,
                    // so that CR-CR-LF will be handled correctly). [CSA]
                    *(++input_ptr) = U_LINE_FEED;
                } else if (ch == U_BACKSLASH && saved_state != UNICODE_ESCAPE_DIGIT_2)
                {
                    saved_state = CR;
                    state       = QUOTE;
                } else
                {
                    state = RAW;
                    *(++input_ptr) = ch;                    
                }
                // clear saved_state == UNICODE_ESCAPE_DIGIT_2 status
                saved_state = CR;
                break;

            case START:
                // if for some reason converter produced or passed
                // byte order mark, it have to be ignored.
                state = RAW;
                if (ch == U_BOM || ch == U_REVERSE_BOM)
                    break; //ignore
                    
            case RAW:
                if (ch==U_BACKSLASH && saved_state != UNICODE_ESCAPE_DIGIT_2)
                {
                    state       = QUOTE;
                } else if (ch == U_CARRIAGE_RETURN)
                {
                    state = CR;
                    *(++input_ptr) = U_LINE_FEED;
                } else
                {
                    *(++input_ptr) = ch;
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
#endif // defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

void LexStream::ReportMessage(StreamError::StreamErrorKind kind,
                              unsigned start_location,
                              unsigned end_location)
{
    if (! control.option.nowarn ||
        kind < StreamError::DEPRECATED_IDENTIFIER_ASSERT)
    {
        bad_tokens.Next().Initialize(kind, start_location, end_location, this);
    }
}

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

     while (top >= 0)
     {
         lower = lostack[top];
         upper = histack[top];
         top--;

         while (upper > lower)
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

            int error_count = NumBadTokens(),
                warning_count = NumWarnTokens();
            if (error_count)
            {
                Coutput << endl << "Found " << error_count << " lexical error"
                        << (error_count == 1 ? "" : "s");
            }
            if (warning_count)
            {
                if (error_count)
                    Coutput << "and issued ";
                else
                    Coutput << endl << "Issued ";
                Coutput << warning_count << " lexical warning"
                        << (warning_count == 1 ? "" : "s");
            }
            if (error_count || warning_count)
                Coutput << " in \"" << file_name << "\":";

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
                    JikesAPI::getInstance()->reportError(&bad_tokens[i]);
                }
            }
        }
        else
        {
            for (int i = 0; i < bad_tokens.Length(); i++)
                JikesAPI::getInstance()->reportError(&bad_tokens[i]);
        }

        DestroyInput();

        Coutput.flush();
    }

    return;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

