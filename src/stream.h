// $Id: stream.h,v 1.32 2001/09/14 05:31:34 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef stream_INCLUDED
#define stream_INCLUDED

#include "platform.h"
#include "javadef.h"
#include "javasym.h"
#include "tuple.h"
#include "tab.h"
#include "lookup.h"
#include "jikesapi.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class Control    ;
class Input_info ;
class Scanner    ;
class Symbol     ;
class FileSymbol ;
class ZipFile    ;
class LexStream  ;

class StreamError : public JikesError
{
    friend class LexStream;

 public:

    StreamError();
    
    virtual const wchar_t *getErrorMessage();
    virtual const wchar_t *getErrorReport();
    
    virtual JikesErrorSeverity getSeverity();
    virtual const char *getFileName();
    
    virtual int getLeftLineNo      ();
    virtual int getLeftColumnNo    ();
    virtual int getRightLineNo     ();
    virtual int getRightColumnNo   ();
    
    enum StreamErrorKind
    {
        BAD_TOKEN,
        BAD_OCTAL_CONSTANT,
        EMPTY_CHARACTER_CONSTANT,
        UNTERMINATED_CHARACTER_CONSTANT,
        UNTERMINATED_COMMENT,
        UNTERMINATED_STRING_CONSTANT,
        INVALID_HEX_CONSTANT,
        INVALID_FLOATING_CONSTANT_EXPONENT,
        INVALID_UNICODE_ESCAPE
    };

    void Initialize(StreamErrorKind kind_, unsigned start_location_, unsigned end_location_, LexStream *);

 protected:

 private:

    unsigned        start_location ;
    unsigned        end_location   ;
    StreamErrorKind kind           ;
    
    static  bool    emacs_style_report;
    LexStream      *lex_stream;

    int left_line_no    ;
    int left_column_no  ;
    int right_line_no   ;
    int right_column_no ;

    wchar_t *regularErrorString();
    wchar_t *emacsErrorString();
    
    void PrintLargeSource(ErrorString &s);
    void PrintSmallSource(ErrorString &s);

    bool initialized;
    
};


// The stream class encapsulates details related to reading
// a stream of possibly encoded data from the file system.

class Stream
{
public:
  
    Stream();
    ~Stream();

    void DestroyInput() {
        delete [] input_buffer;
        input_buffer = NULL;
    }

    inline wchar_t* InputBuffer() { return input_buffer; }
    inline size_t InputBufferLength() { return input_buffer_length; }

    inline wchar_t* AllocateInputBuffer( size_t size ) {
        return input_buffer = new wchar_t[size + 4];
    }

#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)
    static bool IsSupportedEncoding(char* encoding);
    bool SetEncoding(char* encoding);
#endif

protected:

    wchar_t *input_buffer;
    size_t input_buffer_length;

    const char *source_ptr;    // Start of data buffer to decoded
    const char *source_tail;   // End of data buffer to be decoded
    const char *data_buffer;   // The data to be decoded

    bool  error_decode_next_character;

//private: // FIXME : Make vars private once extracted from LexStream!

#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

#if defined(HAVE_LIBICU_UC)
     UConverter * _decoder;
#elif defined(HAVE_ICONV_H)
     iconv_t _decoder;
#endif

    void DestroyEncoding();

    // Read the next wchar_t from the stream.
    // If an error occurs the ErrorDecodeNextCharacter
    // method will return true on the next call.

    wchar_t DecodeNextCharacter();

    inline bool ErrorDecodeNextCharacter() {
        bool result = error_decode_next_character;
        if (result)
            error_decode_next_character = false;
        return result;
    }

    // Returns true if an encoding has been set

    inline bool HaveDecoder() {
#if defined(HAVE_LIBICU_UC)
        return (_decoder != NULL);
#elif defined(HAVE_ICONV_H)
        return (_decoder != (iconv_t)-1);
#endif
    }

#endif // defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

    inline void InitializeDataBuffer(const char * buffer, long size) {
        data_buffer = buffer;
        source_ptr = data_buffer;
        source_tail = data_buffer + size - 1;
    }

    inline bool HasMoreData() {
        return (source_ptr <= source_tail);
    }
};


//
// LexStream holds a stream of tokens generated from an input and
// provides methods to retrieve information from the stream.
//
class LexStream : public Stream
{

    friend class StreamError;
    
 public:
    
    typedef int TypeIndex;
    typedef int TokenIndex;
    typedef int CommentIndex;
    enum { LEX_INFINITY = INT_MAX }; // the largest possible value for TokenIndex

    FileSymbol *file_symbol;

    inline TokenIndex Next(TokenIndex i)
         { return (++i < token_stream.Length() ? i : token_stream.Length() - 1); }

    inline TokenIndex Previous(TokenIndex i) { return (i <= 0 ? 0 : i - 1); }

    inline TokenIndex Peek() { return Next(index); }

    inline void Reset(TokenIndex i = 1) { index = Previous(i); }

    inline TokenIndex Gettoken() { return index = Next(index); }

    inline TokenIndex Gettoken(TokenIndex end_token)
         { return index = (index < end_token ? Next(index) : token_stream.Length() - 1); }

    inline TokenIndex Badtoken() { return 0; }

    inline unsigned Kind(TokenIndex i) { return tokens[i].Kind(); }

    inline unsigned Location(TokenIndex i) { return tokens[i].Location(); }

    inline unsigned Line(TokenIndex i) { return FindLine(tokens[i].Location()); }

    inline unsigned Column(TokenIndex i) { return columns ? columns[i] : (input_buffer ? FindColumn(tokens[i].Location()) : 0); }

    inline bool AfterEol(TokenIndex i) { return (i < 1 ? true : Line(i - 1) < Line(i)); }

    inline bool IsDeprecated(TokenIndex i) { return tokens[i].Deprecated(); }

    inline TokenIndex MatchingBrace(TokenIndex i) { return tokens[i].additional_info.right_brace; }

    wchar_t *NameString(TokenIndex i)
    {
        return (NameSymbol(i) || LiteralSymbol(i) ? tokens[i].additional_info.symbol -> Name()
                                                  : KeywordName(tokens[i].Kind()));
    }

    int NameStringLength(TokenIndex i)
    {
        return (NameSymbol(i) || LiteralSymbol(i) ? tokens[i].additional_info.symbol -> NameLength()
                                                  : wcslen(KeywordName(tokens[i].Kind())));
    }

    class LiteralSymbol *LiteralSymbol(TokenIndex);

    class NameSymbol *NameSymbol(TokenIndex);

    char *FileName();
    size_t FileNameLength();

    inline int LineLength(unsigned line_no) { return locations[line_no + 1] - locations[line_no]; }
    inline int LineStart(unsigned line_no)  { return locations[line_no]; }
    inline int LineEnd(unsigned line_no)    { return locations[line_no + 1] - 1; }

    inline int LineSegmentLength(TokenIndex i)
        { return Tab::Wcslen(input_buffer, tokens[i].Location(), LineEnd(Line(i))); }

    //
    // For a sequence of tokens in a given range find out how many large
    // characters they contain and compute the appropriate offset.
    //
    inline int WcharOffset(TokenIndex start, TokenIndex end)
    {
        int offset = 0;
        for (TokenIndex i = start; i <= end; i++)
        {
            for (wchar_t *str = NameString(i); *str; str++)
            {
                 if (*str > 0xff)
                    offset += 5;
            }
        }

        return offset;
    }

    //
    // When only an end token is supplied, the start token is assume to be the first one on the same line.
    //
    inline int WcharOffset(TokenIndex end)
    {
        TokenIndex start;
        unsigned the_line = Line(end);
        for (start = end; Line(start) == the_line; start--)
            ;
        start++;

        return WcharOffset(start, end);
    }

    CommentIndex FirstComment(TokenIndex);

    inline int NumTypes() { return type_index.Length(); }
    inline TokenIndex Type(int i) { return types[i]; }

    inline int NumTokens() { return token_stream.Length(); }

    inline int NumComments() { return comment_stream.Length(); }

    inline TokenIndex PrecedingToken(CommentIndex i) { return comments[i].previous_token; }

    inline unsigned CommentLocation(CommentIndex i)  { return comments[i].location; }

    inline wchar_t *CommentString(CommentIndex i)    { return comments[i].string; }

    inline int CommentStringLength(CommentIndex i)   { return comments[i].length; }

    inline int NumBadTokens() { return bad_tokens.Length(); }

#ifdef JIKES_DEBUG
    int file_read;
#endif

    //*
    //* Constructors and Destructor.
    //*
    LexStream(Control &control_, FileSymbol *file_symbol_);

    bool ComputeColumns()
    {
        RereadInput();
        if (input_buffer)
            InitializeColumns();

        DestroyInput();

        return (columns != NULL);
    }

    void RereadInput();
    ~LexStream();


    void DestroyInput()
    {
        Stream::DestroyInput();

        delete [] comment_buffer;
        comment_buffer = NULL;
    }

    void SortMessages();
    void PrintMessages();

    void SetUpComments()
    {
        if (comment_buffer)
            return;
        RereadInput();
        //
        // Calculate the length of the string required to save the comments.
        // Allocate the buffer, save the comments in the buffer and update their
        // respective "string" pointer.
        //
        int length = 0,
            i;

        for (i = 1; i < comment_stream.Length(); i++)
            length += (comments[i].length + 1);
        comment_buffer = new wchar_t[length];
        wchar_t *ptr = comment_buffer;
        for (i = 1; i < comment_stream.Length(); i++)
        {
            memmove(ptr, &(input_buffer[comments[i].location]), comments[i].length * sizeof(wchar_t));
            comments[i].string = ptr;
            ptr += comments[i].length;
            *ptr++ = U_NULL;
        }

        return;
    }

#ifdef JIKES_DEBUG
void Dump(); // temporary function used to dump token stream.
#endif

    //
    // Return the total size of space allocated for the tokens.
    //
    size_t TokenSpaceAllocated(void)
    {
        return token_stream.Length() * sizeof(Token);
    }

    //
    // Return the total size of space allocated for the comments.
    //
    size_t CommentSpaceAllocated(void)
    {
        return comment_stream.Length() * sizeof(Comment);
    }

private:

    int hexvalue(wchar_t ch);
    
#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)
    enum UnicodeLexerState
    {
        START,
        RAW,
        CR,
        QUOTE,
        UNICODE_ESCAPE,
        UNICODE_ESCAPE_DIGIT_0,
        UNICODE_ESCAPE_DIGIT_1,
        UNICODE_ESCAPE_DIGIT_2
    };
#endif
    
    friend class Scanner;

    class Token
    {
        //
        // It is expected that a location will be set for every token. Therefore,
        // as we are setting the location, we also reset the deprecated bit to 0.
        // If it is subsequently discovered that the token is followed by one or more
        // deprecated tags then the bit is set to 1 by an invocation of the
        // function SetDeprecated. Note that a better way to resetting all the bits in
        // "info" is to use the function ResetInfoAndSetLocation defined below, instead
        // of using SetLocation
        //
        inline void SetLocation(unsigned location) { assert(location <= 0x00FFFFFF); info = (info & 0x0000007F) | (location << 8); }

    public:
        unsigned info;
        union
        {
            Symbol   *symbol;
            TokenIndex right_brace;
        } additional_info;

        //
        // To just reset the info, this function should be invoked with a location value of 0.
        //
        inline void ResetInfoAndSetLocation(unsigned location)
        {
            assert(location <= 0x00FFFFFF);
            info = (location << 8);
            additional_info.symbol = NULL;
        }

        inline unsigned Location()                   { return (info >> 8); }
        inline void SetKind(unsigned kind)           { assert(kind <= 0x0000007F); info = (info & 0xFFFFFF80) | kind; }
        inline unsigned Kind()                       { return (info & 0x0000007F); }
        inline void SetDeprecated()                  { info |= 0x00000080; }
        inline bool Deprecated()                     { return ((info & 0x00000080) != 0); }

        inline void SetSymbol(Symbol *symbol)        { additional_info.symbol = symbol; }
        inline void SetRightBrace(TokenIndex rbrace) { additional_info.right_brace = rbrace; }
    };

    TokenIndex GetNextToken(unsigned location = 0)
    {
        TokenIndex index = token_stream.NextIndex();
        token_stream[index].ResetInfoAndSetLocation(location);

        return index;
    }

    class Comment
    {
    public:
        TokenIndex previous_token;
        unsigned   location;
        unsigned   length;
        wchar_t   *string;
    };

    Tuple<StreamError> bad_tokens;

    TokenIndex index;
    Token *tokens;
    unsigned short *columns;
    ConvertibleArray<Token> token_stream;
    Comment *comments;
    ConvertibleArray<Comment> comment_stream;
    unsigned *locations;
    ConvertibleArray<unsigned> line_location;
    TokenIndex *types;
    ConvertibleArray<TokenIndex> type_index;

    void InitializeColumns();
    void CompressSpace();

    bool initial_reading_of_input;

    wchar_t *comment_buffer;

    Control &control;

    void ReadInput();
    void ProcessInput(const char *, long);
#if defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)
    void ProcessInputUnicode(const char *, long);
#else
    void ProcessInputAscii(const char *, long);
#endif // defined(HAVE_LIBICU_UC) || defined(HAVE_ICONV_H)

    wchar_t *KeywordName(int);

    unsigned FindLine(unsigned location);

    unsigned FindColumn(unsigned location)
    {
        assert(locations);

        return Tab::Wcslen(input_buffer, locations[FindLine(location)], location);
    }
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // stream_INCLUDED

