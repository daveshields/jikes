// $Id: lookup.h,v 1.11 1999/09/17 17:48:37 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef lookup_INCLUDED
#define lookup_INCLUDED

#include "config.h"
#include <wchar.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include "tuple.h"
#include "long.h"
#include "double.h"

class Symbol;
class PackageSymbol;
class TypeSymbol;
class MethodSymbol;
class MethodShadowSymbol;
class BlockSymbol;
class VariableSymbol;
class VariableShadowSymbol;
class LabelSymbol;
class LiteralSymbol;
class NameSymbol;

class PathSymbol;
class DirectorySymbol;
class FileSymbol;

class ShadowSymbol;

class LiteralValue;
class IntLiteralValue;
class LongLiteralValue;
class FloatLiteralValue;
class DoubleLiteralValue;
class Utf8LiteralValue;

class Utf8LiteralTable;
class NameLookupTable;
class TypeLookupTable;
class LiteralLookupTable;

class AstBinaryExpression;
class AstExpression;

class Hash
{
public:
    //
    // HASH takes as argument a pointer to a character string
    // and its length which it hashes it into a location in the name
    // hash table.
    //
    inline static unsigned Function(wchar_t *head, int len)
    {
        unsigned long hash_value = head[len >> 1]; // start with center (or unique) letter
        wchar_t *tail = &head[len - 1];

        for (int i = 0; i < 5 && head < tail; i++)
        {
            unsigned k = *tail--;
            hash_value += ((k << 7) + *head++);
        }

        return hash_value;
    }

    //
    // Same as above function for a regular "char" string.
    //
    inline static unsigned Function(char *head, int len)
    {
        unsigned long hash_value = head[len >> 1]; // start with center (or unique) letter
        char *tail = &head[len - 1];

        for (int i = 0; i < 5 && head < tail; i++)
        {
            unsigned k = *tail--;
            hash_value += ((k << 7) + *head++);
        }

        return hash_value;
    }

    inline static unsigned Function(IEEEfloat value)
    {
        return value.Word();
    }

    inline static unsigned Function(IEEEdouble value)
    {
        unsigned result = value.HighWord() + value.LowWord();
        return result;
    }
};


class DirectoryEntry
{
public:
    DirectoryEntry *next;
    int length;
    char *name;

    DirectoryEntry() : name(NULL),
                       directory(NULL),
                       next(NULL),
                       length(0),
                       mtime_(0)
    {
        image = this;
    }

    virtual ~DirectoryEntry()
    {
        delete [] name;
    }


    inline void Initialize(DirectorySymbol *directory_, char *name_, int length_)
    {
        directory = directory_;
        length = length_;
        name = new char[length + 1];
        memmove(name, name_, length * sizeof(char));
        name[length] = U_NULL;

        return;
    }

    inline void Initialize(DirectoryEntry *entry, char *name_, int length_)
    {
        Initialize(entry -> directory, name_, length_);
    }

    time_t Mtime();

    bool IsDummy() { return this != image; }

    //
    // See FoldedDirectoryEntry for an explanation of the use of this function
    //
    virtual DirectoryEntry *Image() { return this; }

protected:
    DirectorySymbol *directory;
    DirectoryEntry *image;
    time_t mtime_;
};


#ifdef WIN32_FILE_SYSTEM
//
// This object is needed only for systems such as Windows NT/95/98 that
// treat filenames in a case-insensitive fashion.
//
class FoldedDirectoryEntry : public DirectoryEntry
{
public:
    FoldedDirectoryEntry(DirectoryEntry *image_) { DirectoryEntry::image = image_; }
    virtual ~FoldedDirectoryEntry() {}

    virtual DirectoryEntry *Image() { return image; }
};
#endif


class SystemTable
{
    enum
    {
        DEFAULT_HASH_SIZE = 13,
        MAX_HASH_SIZE = 1021
    };

public:

    SystemTable(int = DEFAULT_HASH_SIZE);
    virtual ~SystemTable();

    DirectorySymbol *FindDirectorySymbol(dev_t, ino_t);
    void InsertDirectorySymbol(dev_t, ino_t, DirectorySymbol *);

private:
    class Element
    {
    public:
        Element(dev_t device_, ino_t inode_, DirectorySymbol *directory_symbol_) : device(device_),
                                                                                   inode(inode_),
                                                                                   directory_symbol(directory_symbol_)
        {}

        Element *next;
        dev_t device;
        ino_t inode;
        DirectorySymbol *directory_symbol;
    };

    Tuple<Element *> directories;

    Element **base;
    int hash_size;

    static int primes[];
    int prime_index;

    int hash(dev_t device, ino_t inode) { return (device + inode) % hash_size; }

    void Rehash();
};


class DirectoryTable
{
public:
    Tuple<DirectoryEntry *> entry_pool;

    DirectoryTable(int estimate = 1024);
    ~DirectoryTable();

    DirectoryEntry *FindEntry(char *, int);
    DirectoryEntry *InsertEntry(DirectorySymbol *, char *, int);

#ifdef WIN32_FILE_SYSTEM
    //
    // See FoldedDirectoryEntry for an explanation of the use of this function
    //
    DirectoryEntry *FindCaseInsensitiveEntry(char *, int);
    void InsertCaseInsensitiveEntry(DirectoryEntry *);
#endif

private:
    enum
    {
        DEFAULT_HASH_SIZE = 1021,
        MAX_HASH_SIZE = 8191
    };

    DirectoryEntry **base;
    int hash_size;

    static int primes[];
    int prime_index;

    inline static unsigned Hash(char *head, int len) { return Hash::Function(head, len); }

    void Rehash();
};


class Symbol
{
public:
    Symbol  *next;

    enum SymbolKind
    {
         NONE,
         NAME,
         PACKAGE,
         TYPE, // class or interface
         METHOD,
         BLOCK,
         VARIABLE,
         LABEL,
         LITERAL,

         PATH,
         _DIRECTORY,
         _FILE,

         _num_kinds
    };

    SymbolKind Kind() { return _kind; }
    virtual wchar_t *Name()   { return (wchar_t *) NULL; }
    virtual int NameLength() { return 0; }
    virtual NameSymbol *Identity() { return (NameSymbol *) NULL; }

    PackageSymbol        *PackageCast()        { return (PackageSymbol *) (_kind == PACKAGE ? this : NULL); }
    TypeSymbol           *TypeCast()           { return (TypeSymbol *) (_kind == TYPE ? this : NULL); }
    MethodSymbol         *MethodCast()         { return (MethodSymbol *) (_kind == METHOD ? this : NULL); }
    BlockSymbol          *BlockCast()          { return (BlockSymbol *) (_kind == BLOCK ? this : NULL); }
    VariableSymbol       *VariableCast()       { return (VariableSymbol *) (_kind == VARIABLE ? this : NULL); }
    LabelSymbol          *LabelCast()          { return (LabelSymbol *) (_kind == LABEL ? this : NULL); }
    LiteralSymbol        *LiteralCast()        { return (LiteralSymbol *) (_kind == LITERAL ? this : NULL); }
    NameSymbol           *NameCast()           { return (NameSymbol *) (_kind == NAME ? this : NULL); }

    PathSymbol           *PathCast()           { return (PathSymbol *) (_kind == PATH ? this : NULL); }
    DirectorySymbol      *DirectoryCast()      { return (DirectorySymbol *) (_kind == _DIRECTORY ? this : NULL); }
    FileSymbol           *FileCast()           { return (FileSymbol *) (_kind == _FILE ? this : NULL); }

    virtual ~Symbol() {}

protected:
    SymbolKind _kind;
};


class LiteralValue
{
public:
    LiteralValue *next;
    int index;

    virtual ~LiteralValue() {}
};


class IntLiteralValue : public LiteralValue
{
public:
    int value;

    virtual ~IntLiteralValue() {}

    void Initialize(int value_, int index_)
    {
        value = value_;
        index = index_;
    }
};


class LongLiteralValue : public LiteralValue
{
public:
    LongInt value;

    virtual ~LongLiteralValue() {}

    void Initialize(LongInt value_, int index_)
    {
        value = value_;
        index = index_;
    }
};


class FloatLiteralValue : public LiteralValue
{
public:
    IEEEfloat value;

    virtual ~FloatLiteralValue() {}

    void Initialize(IEEEfloat value_, int index_)
    {
        value = value_;
        index = index_;
    }
};


class DoubleLiteralValue : public LiteralValue
{
public:
    IEEEdouble value;

    virtual ~DoubleLiteralValue() {}

    void Initialize(IEEEdouble value_, int index_)
    {
        value = value_;
        index = index_;
    }
};


class Utf8LiteralValue : public LiteralValue
{
public:
    char *value;
    int  length;

    Utf8LiteralValue() : value(NULL)
    {}

    virtual ~Utf8LiteralValue()
    {
        delete [] value;
    }

    void Initialize(char *value_, int length_, unsigned hash_address_, int index_)
    {
        length = length_;
        value = new char[length + 1];
        memmove(value, value_, length * sizeof(char));
        value[length] = U_NULL;

        hash_address = hash_address_;
        index = index_;
    }

private:

    friend class Utf8LiteralTable;

    unsigned hash_address;
};


class NameSymbol : public Symbol
{
public:
    int index;
    Utf8LiteralValue *Utf8_literal;

    virtual wchar_t *Name()   { return name_; }
    virtual int NameLength() { return length; }
    virtual NameSymbol *Identity() { return this; }
    char *Utf8Name() { return (char *) (Utf8_literal ? Utf8_literal -> value : NULL); }
    int Utf8NameLength() { return (Utf8_literal ? Utf8_literal -> length : 0); }

    NameSymbol() : name_(NULL)
    {}

    virtual ~NameSymbol()
    {
        delete [] name_;
    }

    inline void Initialize(wchar_t *str, int length_, unsigned hash_address_, int index_)
    {
        Symbol::_kind = NAME;

        hash_address = hash_address_;
        index = index_;

        length = length_;
        name_ = new wchar_t[length + 1];
        memmove(name_, str, length * sizeof(wchar_t));
        name_[length] = U_NULL;

        Utf8_literal = NULL;

        return;
    }

private:

    friend class NameLookupTable;

    wchar_t *name_;
    int length;
    unsigned hash_address;
};


class NameLookupTable
{
public:
    Tuple<NameSymbol *> symbol_pool;

    NameLookupTable(int estimate = 16384);
    ~NameLookupTable();

    NameSymbol *FindOrInsertName(wchar_t *, int);

private:
    enum
    {
        DEFAULT_HASH_SIZE = 4093,
        MAX_HASH_SIZE = 32771
    };

    NameSymbol **base;
    int hash_size;

    static int primes[];
    int prime_index;

    inline static unsigned Hash(wchar_t *head, int len) { return Hash::Function(head, len); }

    void Rehash();
};


class TypeLookupTable
{
public:
    TypeLookupTable(int estimate = 16384);
    ~TypeLookupTable();

    TypeSymbol *FindType(char *, int);
    void InsertType(TypeSymbol *);
    void SetEmpty();

private:
    Tuple<TypeSymbol *> symbol_pool;

    enum
    {
        DEFAULT_HASH_SIZE = 4093,
        MAX_HASH_SIZE = 32771
    };

    TypeSymbol **base;
    int hash_size;

    static int primes[];
    int prime_index;

    inline static unsigned Hash(char *head, int len) { return Hash::Function(head, len); }

    void Rehash();
};


class LiteralSymbol : public Symbol
{
public:
    LiteralValue *value;

    virtual wchar_t *Name()   { return name_; }
    virtual int NameLength() { return length; }
    virtual NameSymbol *Identity() { return (NameSymbol *) NULL; }

    LiteralSymbol() : name_(NULL)
    {}

    virtual ~LiteralSymbol()
    {
        delete [] name_;
    }

    void Initialize(wchar_t *str, unsigned hash_address_, int length_)
    {
        Symbol::_kind = LITERAL;

        hash_address = hash_address_;

        length = length_;
        name_ = new wchar_t[length + 1];
        memmove(name_, str, length * sizeof(wchar_t));
        name_[length] = U_NULL;

        value = NULL;
    }

private:

    friend class LiteralLookupTable;

    wchar_t *name_;
    int length;
    unsigned hash_address;
};


class LiteralLookupTable
{
public:
    Tuple<LiteralSymbol *> symbol_pool;

    LiteralLookupTable();
    ~LiteralLookupTable();

    LiteralSymbol *FindOrInsertLiteral(wchar_t *, int);

private:
    enum
    {
        DEFAULT_HASH_SIZE = 1021,
        MAX_HASH_SIZE = 8191
    };

    LiteralSymbol **base;
    int hash_size;

    static int primes[];
    int prime_index;

    inline static unsigned Hash(wchar_t *head, int len) { return Hash::Function(head, len); }

    void Rehash();
};


class IntLiteralTable
{
public:
    Tuple<IntLiteralValue *> symbol_pool;

    IntLiteralTable(LiteralValue *);
    ~IntLiteralTable();

    LiteralValue *FindOrInsertNull()
    {
        return FindOrInsert(0);
    }

    LiteralValue *FindOrInsertChar(LiteralSymbol *);
    LiteralValue *FindOrInsertInt(LiteralSymbol *);
    LiteralValue *FindOrInsertHexInt(LiteralSymbol *);
    LiteralValue *FindOrInsertOctalInt(LiteralSymbol *);
    LiteralValue *FindOrInsertNegativeInt(LiteralSymbol *);

    IntLiteralValue *FindOrInsert(int);
    IntLiteralValue *Find(int);

#ifdef TEST
    //
    // To prevent arithmentic conversion to allow illegal calls inadvertently.
    // Since the return type is wrong, compilation will fail !
    //
    void FindOrInsert(LongInt) {}
    void FindOrInsert(float)   {}
    void FindOrInsert(double)  {}
#endif

private:
    enum
    {
        DEFAULT_HASH_SIZE = 4093,
        MAX_HASH_SIZE = 32771
    };

    IntLiteralValue **base;
    int hash_size;

    static int primes[];
    int prime_index;

    static int int32_limit;

    LiteralValue *bad_value;

    void Rehash();
};


class LongLiteralTable
{
public:
    Tuple<LongLiteralValue *> symbol_pool;

    LongLiteralTable(LiteralValue *);
    ~LongLiteralTable();

    LiteralValue *FindOrInsertLong(LiteralSymbol *);
    LiteralValue *FindOrInsertHexLong(LiteralSymbol *);
    LiteralValue *FindOrInsertOctalLong(LiteralSymbol *);
    LiteralValue *FindOrInsertNegativeLong(LiteralSymbol *);

    LongLiteralValue *FindOrInsert(LongInt);

#ifdef TEST
    //
    // To prevent arithmentic conversion to allow illegal calls inadvertently.
    //
    void FindOrInsert(int)    {}
    void FindOrInsert(float)  {}
    void FindOrInsert(double) {}
#endif

private:

    enum
    {
        DEFAULT_HASH_SIZE = 1021,
        MAX_HASH_SIZE = 8191
    };

    LongLiteralValue **base;
    int hash_size;

    static int primes[];
    int prime_index;

    static LongInt int64_limit;

    LiteralValue *bad_value;

    void Rehash();
};


class FloatLiteralTable
{
public:
    Tuple<FloatLiteralValue *> symbol_pool;

    FloatLiteralTable(LiteralValue *);
    ~FloatLiteralTable();

    LiteralValue *FindOrInsertFloat(LiteralSymbol *);

    FloatLiteralValue *FindOrInsert(IEEEfloat);

#ifdef TEST
    //
    // To prevent arithmentic conversion to allow illegal calls inadvertently.
    //
    void FindOrInsert(int)     {}
    void FindOrInsert(LongInt) {}
    void FindOrInsert(double)  {}
#endif

private:

    enum
    {
        DEFAULT_HASH_SIZE = 1021,
        MAX_HASH_SIZE = 8191
    };

    FloatLiteralValue **base;
    int hash_size;

    static int primes[];
    int prime_index;

    LiteralValue *bad_value;

    inline static unsigned Hash(IEEEfloat value) { return Hash::Function(value); }

    void Rehash();
};


class DoubleLiteralTable
{
public:
    Tuple<DoubleLiteralValue *> symbol_pool;

    DoubleLiteralTable(LiteralValue *);
    ~DoubleLiteralTable();

    LiteralValue *FindOrInsertDouble(LiteralSymbol *);

    DoubleLiteralValue *FindOrInsert(IEEEdouble);

#ifdef TEST
    //
    // To prevent arithmentic conversion to allow illegal calls inadvertently.
    //
    void FindOrInsert(int)     {}
    void FindOrInsert(LongInt) {}
    void FindOrInsert(float)   {}
#endif

private:

    enum
    {
        DEFAULT_HASH_SIZE = 1021,
        MAX_HASH_SIZE = 8191
    };

    DoubleLiteralValue **base;
    int hash_size;

    static int primes[];
    int prime_index;

    LiteralValue *bad_value;

    inline static unsigned Hash(IEEEdouble value) { return Hash::Function(value); }

    void Rehash();
};


class Utf8LiteralTable
{
public:
    Tuple<Utf8LiteralValue *> symbol_pool;

    Utf8LiteralTable(LiteralValue *);
    ~Utf8LiteralTable();

    LiteralValue *FindOrInsertString(LiteralSymbol *);

    Utf8LiteralValue *FindOrInsert(char *, int);
    Utf8LiteralValue *FindOrInsert(wchar_t);

    void CheckStringConstant(AstExpression *expr);

private:

    Tuple<AstExpression *> *expr;
    bool IsConstant(AstExpression *);

    enum
    {
        DEFAULT_HASH_SIZE = 4093,
        MAX_HASH_SIZE = 32771
    };

    Utf8LiteralValue **base;
    int hash_size;

    static int primes[];
    int prime_index;

    LiteralValue *bad_value;

    inline static unsigned Hash(char *head, int len) { return Hash::Function(head, len); }

    void Rehash();
};
#endif
