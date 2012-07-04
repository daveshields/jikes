// $Id: symbol.h,v 1.67 2002/11/11 14:51:19 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef symbol_INCLUDED
#define symbol_INCLUDED

#include "platform.h"
#include "stream.h"
#include "lookup.h"
#include "access.h"
#include "tuple.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class Semantic;
class SemanticEnvironment;
class Ast;
class AstCompilationUnit;
class AstMethodDeclarator;
class AstBlock;
class AstList;
class AstExpression;
class AstVariableDeclarator;
class ExpandedTypeTable;
class ExpandedFieldTable;
class ExpandedMethodTable;
class SymbolTable;
class SymbolSet;
class SymbolMap;
class Zip;

template <class Key, class Value>
class Map;

class PackageSymbol;

class PathSymbol : public Symbol
{
public:
    NameSymbol *name_symbol;
    Zip *zipfile;

    PathSymbol(NameSymbol *);
    virtual ~PathSymbol();

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }

    inline bool IsZip() { return zipfile != NULL; }

    inline DirectorySymbol *RootDirectory() { return root_directory; }

private:
    friend class SymbolTable;
    DirectorySymbol *root_directory;
};


class DirectorySymbol : public Symbol
{
public:
    Symbol *owner;
    NameSymbol *name_symbol;

    Tuple<DirectorySymbol *> subdirectories;

    DirectorySymbol(NameSymbol *, Symbol *, bool source_dir_only);
    virtual ~DirectorySymbol();

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }
    bool IsSourceDirectory() { return source_dir_only; }

    DirectoryEntry *FindEntry(char *name, int len)
    {
        return (entries ? entries -> FindEntry(name, len)
                : (DirectoryEntry *) NULL);
    }

#ifdef WIN32_FILE_SYSTEM
    DirectoryEntry *FindCaseInsensitiveEntry(char *name, int length)
    {
        return (entries ? entries -> FindCaseInsensitiveEntry(name, length)
                : (DirectoryEntry *) NULL);
    }

    void InsertEntry(char *name, int length)
    {
        assert(entries);

        DirectoryEntry *entry = entries -> InsertEntry(this, name, length);
        entries -> InsertCaseInsensitiveEntry(entry);
    }
#endif // WIN32_FILE_SYSTEM

    PathSymbol *PathSym()
    {
        return (owner -> PathCast() ? (PathSymbol *) owner
                : ((DirectorySymbol *) owner) -> PathSym());
    }

    inline bool IsZip() { return PathSym() -> IsZip(); }

    void SetDirectoryName();
    inline char *DirectoryName()
    {
        if (! directory_name)
            SetDirectoryName();
        return directory_name;
    }
    inline size_t DirectoryNameLength()
    {
        if (! directory_name)
            SetDirectoryName();
        return directory_name_length;
    }

    inline DirectorySymbol *InsertDirectorySymbol(NameSymbol *, bool);
    inline DirectorySymbol *FindDirectorySymbol(NameSymbol *);

    inline FileSymbol *InsertFileSymbol(NameSymbol *);
    inline FileSymbol *FindFileSymbol(NameSymbol *);

    void ResetDirectory();

    void ReadDirectory();

private:

    time_t mtime;

    SymbolTable *table;
    inline SymbolTable *Table();

    DirectoryTable *entries;
    char *directory_name;
    size_t directory_name_length;

    bool source_dir_only;
};


class FileSymbol : public Symbol
{
private:
    enum FileKind
    {
        JAVA,
        CLASS,
        CLASS_ONLY
    };

    DirectorySymbol *output_directory;
    char *file_name;
    size_t file_name_length;
    Utf8LiteralValue *file_name_literal;

public:
    NameSymbol *name_symbol;
    DirectorySymbol *directory_symbol;
    PackageSymbol *package;
    FileKind kind;

    //
    // These fields are used for files in zip packages.
    //
    u4 uncompressed_size;
    u4 date_time;
    long offset;

    //
    // This field holds the time of last data modification for a non-zip file
    //
    time_t mtime;

    //
    // This field is used for buffer "files".
    //
    // FIXME: This field does not seem to be set anywhere,
    // but it is read in symbol.cpp and stream.cpp
    char *buffer;

    LexStream *lex_stream;
    AstCompilationUnit *compilation_unit;
    Semantic *semantic;

    Tuple<TypeSymbol *> types;

    FileSymbol(NameSymbol *name_symbol_) : output_directory(NULL),
                                           file_name(NULL),
                                           file_name_literal(NULL),
                                           name_symbol(name_symbol_),
                                           directory_symbol(NULL),
                                           package(NULL),
                                           mtime(0),
                                           buffer(NULL),
                                           lex_stream(NULL),
                                           compilation_unit(NULL),
                                           semantic(NULL),
                                           types(4)
    {
         Symbol::_kind = _FILE;
    }

    virtual ~FileSymbol()
    {
        delete [] file_name;
        delete [] buffer;
        delete lex_stream;
    }

    FileSymbol *Clone()
    {
        FileSymbol *clone = new FileSymbol(name_symbol);

        clone -> kind = kind;
        clone -> directory_symbol = directory_symbol;
        clone -> mtime = mtime;

        return clone;
    }

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }

    inline void SetJava()       { kind = JAVA; }
    inline void SetClass()      { kind = CLASS; }
    inline void SetClassOnly()  { kind = CLASS_ONLY; }

    inline bool IsJava()       { return kind == JAVA; }
    inline bool IsClass()      { return kind >= CLASS; }
    inline bool IsClassOnly()  { return kind == CLASS_ONLY; }

    PathSymbol *PathSym()
    {
        return directory_symbol -> PathSym();
    }
    inline bool IsZip() { return PathSym() -> IsZip(); }
    inline Zip *Zipfile() { return PathSym() -> zipfile; }

    static char *java_suffix;
    static int java_suffix_length;
    static char *class_suffix;
    static int class_suffix_length;
    static bool IsJavaSuffix(char *ptr);
    static bool IsClassSuffix(char *ptr);

    inline char *FileName()
    {
        if (! file_name)
            SetFileName();
        return file_name;
    }
    inline int FileNameLength()
    {
        if (! file_name)
            SetFileName();
        return file_name_length;
    }

    inline Utf8LiteralValue *FileNameLiteral()
    {
        assert(file_name_literal);
        return file_name_literal;
    }

    void SetFileNameLiteral(Control *);

    DirectorySymbol *OutputDirectory();

    void SetFileName();

    void CleanUp();

    void Reset()
    {
        CleanUp();

        delete [] file_name;
        file_name = NULL;
        types.Reset();
    }
};


class FileLocation
{
public:
    wchar_t *location;

    FileLocation (LexStream *lex_stream, LexStream::TokenIndex token_index)
    {
        char *file_name = lex_stream -> FileName();
        int length = lex_stream -> FileNameLength();
        location = new wchar_t[length + 13];
        for (int i = 0; i < length; i++) {
            location[i] = (wchar_t) file_name[i];
        }
        location[length++] = U_COLON;

        IntToWstring line_no(lex_stream -> Line(token_index));

        for (int j = 0; j < line_no.Length(); j++)
            location[length++] = line_no.String()[j];
        location[length] = U_NULL;
    }

    FileLocation (FileSymbol *file_symbol)
    {
        char *file_name = file_symbol -> FileName();
        int length = file_symbol -> FileNameLength();
        location = new wchar_t[length + 13];
        for (int i = 0; i < length; i++) {
            location[i] = (wchar_t) file_name[i];
        }
        location[length] = U_NULL;
    }

    ~FileLocation()
    {
        delete [] location;
    }
};


class PackageSymbol : public Symbol
{
public:
    Tuple<DirectorySymbol *> directory;
    PackageSymbol *owner;

    PackageSymbol(NameSymbol *name_symbol_, PackageSymbol *owner_)
        : directory(4),
          owner(owner_),
          name_symbol(name_symbol_),
          table(NULL),
          package_name(NULL)
    {
        Symbol::_kind = PACKAGE;
    }

    virtual ~PackageSymbol();

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }
    // This name is fully qualified, using slashes.

    void SetPackageName();
    // This name is fully qualified, using slashes.
    wchar_t *PackageName()
    {
        if (! package_name)
            SetPackageName();
        return package_name;
    }
    int PackageNameLength()
    {
        if (! package_name)
            SetPackageName();
        return package_name_length;
    }

    inline PackageSymbol *FindPackageSymbol(NameSymbol *);
    inline PackageSymbol *InsertPackageSymbol(NameSymbol *);

    inline TypeSymbol *FindTypeSymbol(NameSymbol *);
    inline TypeSymbol *InsertSystemTypeSymbol(NameSymbol *);
    inline TypeSymbol *InsertOuterTypeSymbol(NameSymbol *);
    inline void DeleteTypeSymbol(TypeSymbol *);

private:

    NameSymbol *name_symbol;
    SymbolTable *table;
    inline SymbolTable *Table();

    wchar_t *package_name;
    size_t package_name_length;
};


class MethodSymbol : public Symbol, public AccessFlags
{
public:
    Ast *declaration; // AstMethodDeclaration or AstConstructorDeclaration
    NameSymbol *name_symbol;
    TypeSymbol *containing_type;
    BlockSymbol *block_symbol;
    MethodSymbol *next_method;
    Utf8LiteralValue *signature;

    int max_block_depth;

    //
    // If this method is a method that permits access to a private member of an
    // enclosing type then accessed_member identifies the member in question.
    //
    Symbol *accessed_member;
    inline bool AccessesInstanceMember();

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }

    MethodSymbol(NameSymbol *name_symbol_)
        : declaration(NULL),
          name_symbol(name_symbol_),
          containing_type(NULL),
          block_symbol(NULL),
          next_method(NULL),
          signature(NULL),
          max_block_depth(1), // there must be at least one block in a method
          // this default is useful for default constructors.
          accessed_member(NULL),
          external_name_symbol(NULL),
          status(0),
          header(NULL),
          type_(NULL),
          formal_parameters(NULL),
          throws(NULL),
          throws_signatures(NULL),
          local_constructor(NULL)
    {
        Symbol::_kind = METHOD;
    }

    virtual ~MethodSymbol();

    bool IsTyped()
    {
        return type_ != NULL;
    }

    void SetType(TypeSymbol *_type)
    {
        type_ = _type;
    }

    void ProcessMethodSignature(Semantic *, LexStream::TokenIndex);
    void ProcessMethodThrows(Semantic *, LexStream::TokenIndex);

    TypeSymbol *Type()
    {
        // Make sure that the method signature associated with this method is
        // processed prior to invoking this function.
        //  ( "this -> ProcessMethodSignature(sem, tok);" )
        assert(type_);

        return type_;
    }

    int NumFormalParameters()
    {
        assert(type_);

        return (formal_parameters ? formal_parameters -> Length() : 0);
    }
    VariableSymbol *FormalParameter(int i)
    {
        return (*formal_parameters)[i];
    }
    void AddFormalParameter(VariableSymbol *variable)
    {
        if (! formal_parameters)
            formal_parameters = new Tuple<VariableSymbol *>(8);
        formal_parameters -> Next() = variable;
    }

    int NumThrows()
    {
        assert(! throws_signatures);

        return (throws ? throws -> Length() : 0);
    }
    TypeSymbol *Throws(int i)
    {
        return (*throws)[i];
    }
    void AddThrows(TypeSymbol *exception)
    {
        if (! throws)
            throws = new Tuple<TypeSymbol *>(8);
        throws -> Next() = exception;
    }

    int NumThrowsSignatures()
    {
        return (throws_signatures ? throws_signatures -> Length() : 0);
    }
    char *ThrowsSignature(int i)
    {
        return (*throws_signatures)[i];
    }
    void AddThrowsSignature(const char *signature_, int length)
    {
        char *signature = new char[length + 1];
        strncpy(signature, signature_, length);
        signature[length] = U_NULL;

        if (! throws_signatures)
            throws_signatures = new Tuple<char *>(8);
        throws_signatures -> Next() = signature;
    }

    void SetExternalIdentity(NameSymbol *external_name_symbol_)
    {
        external_name_symbol = external_name_symbol_;
    }
    NameSymbol *ExternalIdentity()
    {
        return (external_name_symbol ? external_name_symbol : name_symbol);
    }
    wchar_t *ExternalName()
    {
        return (external_name_symbol ? external_name_symbol -> Name()
                : name_symbol -> Name());
    }
    int ExternalNameLength()
    {
        return (external_name_symbol ? external_name_symbol -> NameLength()
                : name_symbol -> NameLength());
    }
    char *ExternalUtf8Name()
    {
        return (char *) (external_name_symbol
                         ? external_name_symbol -> Utf8_literal -> value
                         : (name_symbol -> Utf8_literal
                            ? name_symbol -> Utf8_literal -> value : NULL));
    }
    int ExternalUtf8NameLength()
    {
        return (external_name_symbol
                ? (external_name_symbol -> Utf8_literal
                   ? external_name_symbol -> Utf8_literal -> length : 0)
                : (name_symbol -> Utf8_literal
                   ? name_symbol -> Utf8_literal -> length : 0));
    }

    void SetContainingType(TypeSymbol *containing_type_) { containing_type = containing_type_; }
    void SetBlockSymbol(BlockSymbol *block_symbol_) { block_symbol = block_symbol_; }
    void SetSignature(Control &, TypeSymbol * = NULL);
    void SetSignature(Utf8LiteralValue *signature_) { signature = signature_; }
    char *SignatureString() { return signature -> value; }
    wchar_t *Header();

    void CleanUp();

    void MarkSynthetic() { status |= (unsigned char) 0x08; }
    bool IsSynthetic()   { return (status & (unsigned char) 0x08) != 0; }

    void MarkDeprecated() { status |= (unsigned char) 0x10; }
    bool IsDeprecated()   { return (status & (unsigned char) 0x10) != 0; }

private:
    NameSymbol *external_name_symbol;

    unsigned char status;
    wchar_t *header;

    TypeSymbol *type_;
    Tuple<VariableSymbol *> *formal_parameters;
    Tuple<TypeSymbol *> *throws;
    Tuple<char *> *throws_signatures;

    //
    // If the method in question is a constructor of a local type, we may need
    // to construct another constructor that accepts extra local parameters.
    //
    MethodSymbol *local_constructor;
};


class TypeSymbol : public Symbol, public AccessFlags
{
private:
    enum
    {
        CONSTRUCTOR_MEMBERS_PROCESSED = 0x0001,
        METHOD_MEMBERS_PROCESSED = 0x0002,
        FIELD_MEMBERS_PROCESSED = 0x0004,
        LOCAL_CLASS_PROCESSING_COMPLETED = 0x0008,
        SOURCE_PENDING = 0x0010,
        ANONYMOUS = 0x0020,
        HEADER_PROCESSED = 0x0040,
        PRIMITIVE = 0x0080,
        DEPRECATED = 0x0100,
        BAD = 0x0200,
        CIRCULAR = 0x0400
    };

public:
    SemanticEnvironment *semantic_environment;

    //
    // AstClassDeclaration, AstInterfaceDeclaration, or
    // AstClassInstanceCreationExpression
    //
    Ast *declaration;

    FileSymbol *file_symbol;
    FileLocation *file_location;
    NameSymbol *name_symbol;
    Symbol *owner;

    // A nested class identifies the outer most type that contains it. If a
    // class is not nested then it identifies itself as its outermost type.
    TypeSymbol *outermost_type;

    TypeSymbol *super;

    // Indicates the base type (type of elements in the last dimension) of an
    // array. For a normal type base_type is NULL. If base_type is a "bad"
    // type it points to itself (this).
    TypeSymbol *base_type;

    // This variable is used in TypeCycleChecker to determine if this type
    // forms an inter-type cycle in its "extends" or "implements" relationship.
    int index;

    // This variable is used in TypeCycleChecker to check if this type
    // forms an intra-type cycle in its "extends" or "implements" relationship;
    int unit_index;

    // This variable is used in TypeCycleChecker to determine which types
    // (files) need to be recompiled based on the "dependent" relationship.
    int incremental_index;

    int NumLocalConstructorCallEnvironments()
    {
        return (local_constructor_call_environments
                ? local_constructor_call_environments -> Length() : 0);
    }
    SemanticEnvironment *&LocalConstructorCallEnvironment(int i)
    {
        return (*local_constructor_call_environments)[i];
    }
    void AddLocalConstructorCallEnvironment(SemanticEnvironment *environment)
    {
        if (! local_constructor_call_environments)
            local_constructor_call_environments = new Tuple<SemanticEnvironment *>(8);
        local_constructor_call_environments -> Next() = environment;
    }

    int NumPrivateAccessMethods()
    {
        return (private_access_methods
                ? private_access_methods -> Length() : 0);
    }
    MethodSymbol *&PrivateAccessMethod(int i)
    {
        return (*private_access_methods)[i];
    }
    void AddPrivateAccessMethod(MethodSymbol *method_symbol)
    {
        if (! private_access_methods)
            private_access_methods = new Tuple<MethodSymbol *>(8);
        private_access_methods -> Next() = method_symbol;
    }

    int NumPrivateAccessConstructors()
    {
        return (private_access_constructors
                ? private_access_constructors -> Length() : 0);
    }
    MethodSymbol *&PrivateAccessConstructor(int i)
    {
        return (*private_access_constructors)[i];
    }
    void AddPrivateAccessConstructor(MethodSymbol *constructor_symbol)
    {
        if (! private_access_constructors)
            private_access_constructors = new Tuple<MethodSymbol *>(8);
        private_access_constructors -> Next() = constructor_symbol;
    }

    int NumConstructorParameters()
    {
        return (constructor_parameters
                ? constructor_parameters -> Length() : 0);
    }
    VariableSymbol *&ConstructorParameter(int i)
    {
        return (*constructor_parameters)[i];
    }
    void AddConstructorParameter(VariableSymbol *variable_symbol)
    {
        if (! constructor_parameters)
            constructor_parameters = new Tuple<VariableSymbol *>(8);
        constructor_parameters -> Next() = variable_symbol;
    }

    VariableSymbol *&EnclosingInstance()
    {
        return enclosing_instance;
    }

    int NumClassLiterals()
    {
        return (class_literals ? class_literals -> Length() : 0);
    }
    VariableSymbol *&ClassLiteral(int i) { return (*class_literals)[i]; }
    void AddClassLiteral(VariableSymbol *literal_symbol)
    {
        if (! class_literals)
            class_literals = new Tuple<VariableSymbol *>(8);
        class_literals -> Next() = literal_symbol;
    }

    VariableSymbol *AssertVariable() { return assert_variable; }

    int NumNestedTypes()
    {
        return (nested_types ? nested_types -> Length() : 0);
    }
    TypeSymbol *&NestedType(int i) { return (*nested_types)[i]; }
    void AddNestedType(TypeSymbol *type_symbol)
    {
        if (! nested_types)
            nested_types = new Tuple<TypeSymbol *>(8);
        nested_types -> Next() = type_symbol;
    }

    int NumInterfaces() { return (interfaces ? interfaces -> Length() : 0); }
    void ResetInterfaces()
    {
        delete interfaces;
        interfaces = NULL;
    }
    TypeSymbol *Interface(int i) { return (*interfaces)[i]; }
    void AddInterface(TypeSymbol *type_symbol)
    {
        if (! interfaces)
            interfaces = new Tuple<TypeSymbol *>(8);
        interfaces -> Next() = type_symbol;
    }

    int NumAnonymousTypes()
    {
        return anonymous_types ? anonymous_types -> Length() : 0;
    }
    TypeSymbol *&AnonymousType(int i) { return (*anonymous_types)[i]; }
    void AddAnonymousType(TypeSymbol *type_symbol)
    {
        if (! anonymous_types)
            anonymous_types = new Tuple<TypeSymbol *>(8);
        anonymous_types -> Next() = type_symbol;
        if (! outermost_type -> placeholder_type)
            outermost_type -> placeholder_type = type_symbol;
    }
    void DeleteAnonymousTypes();
    int NumLocalTypes();

    SymbolSet *local,
              *non_local;

    SymbolSet *supertypes_closure,
              *subtypes,
              *subtypes_closure,
              *innertypes_closure;

    SymbolSet *dependents,
              *parents,
              *static_parents,
              *dependents_closure,  // processed in cycle.cpp
              *parents_closure;     // processed in cycle.cpp

    // Index of element in symbol_pool (in the relevant symbol table) that
    // points to this type.
    int pool_index;

    Utf8LiteralValue *signature;
    Utf8LiteralValue *fully_qualified_name;

    ExpandedTypeTable *expanded_type_table;
    ExpandedFieldTable *expanded_field_table;
    ExpandedMethodTable *expanded_method_table;

    int num_dimensions;

    //
    // Initializer blocks and variable declarations which require
    // initialization are coalesced into these two methods. Notice that
    // bytecode.cpp emits an actual method named '<clinit>' for the static
    // case, and one named 'this' for the instance case (yes, that is a legal
    // VM name, but an illegal Java source code name). Constructors that do
    // not invoke another constructor via the this() statement will defer
    // variable initialization to a generated call to the method 'this'. This
    // relies on VM's allowing the assignment of final instance fields in an
    // instance method instead of a constructor.
    //
    MethodSymbol *instance_initializer_method;
    MethodSymbol *static_initializer_method;

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }


    void SetExternalIdentity(NameSymbol *external_name_symbol_)
    {
        external_name_symbol = external_name_symbol_;
    }
    NameSymbol *ExternalIdentity()
    {
        return (external_name_symbol ? external_name_symbol : name_symbol);
    }
    wchar_t *ExternalName()
    {
        return (external_name_symbol ? external_name_symbol -> Name()
                : name_symbol -> Name());
    }
    int ExternalNameLength()
    {
        return (external_name_symbol ? external_name_symbol -> NameLength()
                : name_symbol -> NameLength());
    }
    char *ExternalUtf8Name()
    {
        return (char *) (external_name_symbol
                         ? external_name_symbol -> Utf8_literal -> value
                         : (name_symbol -> Utf8_literal
                            ? name_symbol -> Utf8_literal -> value : NULL));
    }
    int ExternalUtf8NameLength()
    {
        return (external_name_symbol
                ? (external_name_symbol -> Utf8_literal
                   ? external_name_symbol -> Utf8_literal -> length : 0)
                : (name_symbol -> Utf8_literal
                   ? name_symbol -> Utf8_literal -> length : 0));
    }

    TypeSymbol(NameSymbol *);
    virtual ~TypeSymbol();

    void ProcessTypeHeaders();
    void ProcessMembers();
    void CompleteSymbolTable();
    void ProcessExecutableBodies();
    void RemoveCompilationReferences();

    VariableSymbol *InsertThis0();

    TypeSymbol *FindOrInsertClassLiteralClass();
    MethodSymbol *FindOrInsertClassLiteralMethod(Control &);
    MethodSymbol *ClassLiteralMethod()
    {
        return class_literal_method;
    }
    Utf8LiteralValue *FindOrInsertClassLiteralName(Control &);
    VariableSymbol *FindOrInsertClassLiteral(TypeSymbol *);
    VariableSymbol *FindOrInsertLocalShadow(VariableSymbol *);
    VariableSymbol *FindOrInsertAssertVariable();

    //
    // Get an accessor method in this class for the given symbol, with
    // qualifying type defaulting to this type if unspecified
    //
    MethodSymbol *GetReadAccessMethod(MethodSymbol *, TypeSymbol * = NULL);
    MethodSymbol *GetReadAccessConstructor(MethodSymbol *);
    MethodSymbol *GetReadAccessMethod(VariableSymbol *, TypeSymbol * = NULL);
    MethodSymbol *GetWriteAccessMethod(VariableSymbol *, TypeSymbol * = NULL);
    MethodSymbol *GetWriteAccessFromReadAccess(MethodSymbol *);
    TypeSymbol *GetPlaceholderType();

    bool IsArray() { return (num_dimensions > 0); }

    void SetOwner(Symbol *owner_) { owner = owner_; }

    bool IsOwner(TypeSymbol *type)
    {
        Symbol *sym = type -> owner;
        while (! sym -> PackageCast())
        {
            if (sym == this)
                return true;

            MethodSymbol *method = sym -> MethodCast();
            sym = (method ? method -> containing_type
                   : ((TypeSymbol *) sym) -> owner);
        }

        return false;
    }

    TypeSymbol *ContainingType()
    {
        if (owner)
        {
            TypeSymbol *type = owner -> TypeCast();
            if (type)
                return type;
            MethodSymbol *method = owner -> MethodCast();
            if (method)
                return method -> containing_type;
        }

        return (TypeSymbol *) NULL;
    }

    TypeSymbol *EnclosingType();
    bool HasEnclosingInstance(TypeSymbol *, bool = false);
    bool HasProtectedAccessTo(TypeSymbol *);

    //
    // Note that this test considers a class a subclass of itself, and also
    // interfaces are a subclass of Object. See also IsSubtype.
    //
    bool IsSubclass(TypeSymbol *super_class)
    {
        for (TypeSymbol *type = this; type; type = type -> super)
            if (type == super_class)
                return true;
        return false;
    }

    //
    // Note that this test considers an interface a subtype of itself, but
    // does not work for classes. See also IsSubtype.
    //
    bool IsSubinterface(TypeSymbol *super_interface)
    {
        if (this == super_interface)
            return true;
        for (int i = 0; i < NumInterfaces(); i++)
        {
            if (Interface(i) -> IsSubinterface(super_interface))
                return true;
        }
        return false;
    }

    //
    // This test works for classes, but not for interfaces. See also IsSubtype.
    //
    bool Implements(TypeSymbol *inter)
    {
        for (int i = 0; i < NumInterfaces(); i++)
        {
            if (Interface(i) -> IsSubinterface(inter))
                return true;
        }
        return super && super -> Implements(inter);
    }

    //
    // The most generic subtype relation. This correctly checks a class's
    // superclasses and superinterfaces, an interfaces's superinterfaces and
    // Object, and an array's compatible types (smaller dimensions of Object,
    // Cloneable, Serializable, and all equal dimension arrays where the
    // element type is a subtype). For simplicity, a type subtypes itself.
    //
    bool IsSubtype(TypeSymbol *type)
    {
        if (ACC_INTERFACE())
            return (type -> ACC_INTERFACE() && IsSubinterface(type)) ||
                super == type;
        if (num_dimensions)
        {
            TypeSymbol *base = type -> base_type ? type -> base_type : type;
            return (num_dimensions > type -> num_dimensions &&
                    ((base -> ACC_INTERFACE() && Implements(base)) ||
                     super == base)) ||
                (num_dimensions == type -> num_dimensions &&
                 base_type -> IsSubtype(base));
        }
        return type -> ACC_INTERFACE() ? Implements(type) : IsSubclass(type);
    }

    wchar_t *FileLoc()
    {
        return (wchar_t *) (file_location ? file_location -> location : NULL);
    }

    void SetLocation();

    LexStream::TokenIndex GetLocation();

    TypeSymbol *GetArrayType(Semantic *, int);

    TypeSymbol *ArraySubtype()
    {
        return base_type -> Array(num_dimensions - 1);
    }

    void SetSignature(Control &);
    void SetSignature(Utf8LiteralValue *signature_) { signature = signature_; }
    char *SignatureString() { return signature -> value; }

    void SetClassLiteralName(Utf8LiteralValue *class_literal_name_)
    {
        class_literal_name = class_literal_name_;
    }

    PackageSymbol* ContainingPackage()
    {
        return outermost_type -> owner -> PackageCast();
    }
    // Returns the fully-qualified '/' separated package name.
    const wchar_t* ContainingPackageName()
    {
        return outermost_type -> owner -> PackageCast() -> PackageName();
    }

    bool IsNestedIn(TypeSymbol *);

    bool IsNested() { return outermost_type != this; }

    //
    // JLS2 8.1.2 states that ALL local and anonymous classes are inner
    // classes, even when they occur in a static context.  Even in the static
    // context, such classes are not implicitly static, they simply lack an
    // enclosing instance.  In other words, the JLS definition of inner class
    // is lame. If everything works correctly, these classes will correctly
    // be marked nested, yet never static.
    //
    bool IsInner()
    {
        assert((! IsLocal() && ! Anonymous()) ||
               (IsNested() && ! ACC_STATIC()));
        return IsNested() && ! ACC_STATIC();
    }

    bool IsLocal()
    {
        for (Symbol *sym = owner;
             ! sym -> PackageCast(); sym = ((TypeSymbol *) sym) -> owner)
        {
            if (sym -> MethodCast())
                return true;
        }
        return false;
    }

    inline char *ClassName()
    {
        if (! class_name)
            SetClassName();
        return class_name;
    }

    void MarkConstructorMembersProcessed()
    {
        status |= CONSTRUCTOR_MEMBERS_PROCESSED;
    }
    bool ConstructorMembersProcessed()
    {
        return (status & CONSTRUCTOR_MEMBERS_PROCESSED) != 0;
    }

    void MarkMethodMembersProcessed()
    {
        status |= METHOD_MEMBERS_PROCESSED;
    }
    bool MethodMembersProcessed()
    {
        return (status & METHOD_MEMBERS_PROCESSED) != 0;
    }

    void MarkFieldMembersProcessed()
    {
        status |= FIELD_MEMBERS_PROCESSED;
    }
    bool FieldMembersProcessed()
    {
        return (status & FIELD_MEMBERS_PROCESSED) != 0;
    }

    void MarkLocalClassProcessingCompleted()
    {
        status |= LOCAL_CLASS_PROCESSING_COMPLETED;
    }
    bool LocalClassProcessingCompleted()
    {
        return (status & LOCAL_CLASS_PROCESSING_COMPLETED) != 0;
    }

    void MarkSourcePending() { status |= SOURCE_PENDING; }
    void MarkSourceNoLongerPending() { status &= ~ SOURCE_PENDING; }
    bool SourcePending() { return (status & SOURCE_PENDING) != 0; }

    void MarkAnonymous() { status |= ANONYMOUS; }
    bool Anonymous() { return (status & ANONYMOUS) != 0; }

    void MarkHeaderProcessed() { status |= HEADER_PROCESSED; }
    bool HeaderProcessed() { return (status & HEADER_PROCESSED) != 0; }

    void MarkPrimitive() { status |= PRIMITIVE; }
    bool Primitive() { return (status & PRIMITIVE) != 0; }

    void MarkDeprecated() { status |= DEPRECATED; }
    void ResetDeprecated() { status &= ~DEPRECATED; }
    bool IsDeprecated() { return (status & DEPRECATED) != 0; }

    void MarkBad()
    {
        SetACC_PUBLIC();
        status |= (BAD | HEADER_PROCESSED | CONSTRUCTOR_MEMBERS_PROCESSED |
                   METHOD_MEMBERS_PROCESSED | FIELD_MEMBERS_PROCESSED |
                   LOCAL_CLASS_PROCESSING_COMPLETED);
        MarkSourceNoLongerPending();
    }
    bool Bad() { return (status & BAD) != 0; }

    void MarkCircular()
    {
        status |= CIRCULAR;
        MarkBad();
    }
    void MarkNonCircular() { status &= ~ CIRCULAR; }
    bool Circular() { return (status & CIRCULAR) != 0; }

    void ProcessNestedTypeSignatures(Semantic *, LexStream::TokenIndex);

    bool NestedTypesProcessed() { return nested_type_signatures == NULL; }

    int NumNestedTypeSignatures()
    {
        return (nested_type_signatures
                ? nested_type_signatures -> Length() : 0);
    }
    char *NestedTypeSignature(int i)
    {
        return (*nested_type_signatures)[i];
    }
    void AddNestedTypeSignature(const char *signature_, int length)
    {
        char *signature = new char[length + 1];
        strncpy(signature, signature_, length);
        signature[length] = U_NULL;

        if (! nested_type_signatures)
            nested_type_signatures = new Tuple<char *>(8);
        nested_type_signatures -> Next() = signature;
    }

    inline void SetSymbolTable(int);
    inline SymbolTable *Table();

    int NumVariableSymbols();
    VariableSymbol *VariableSym(int);

    int NumMethodSymbols();
    MethodSymbol *MethodSym(int);

    int NumTypeSymbols();
    TypeSymbol *TypeSym(int);

    inline TypeSymbol *InsertAnonymousTypeSymbol(NameSymbol *);
    inline TypeSymbol *FindTypeSymbol(NameSymbol *);
    inline TypeSymbol *InsertNestedTypeSymbol(NameSymbol *);
    inline MethodSymbol *FindConstructorSymbol();
    inline MethodSymbol *InsertConstructorSymbol(NameSymbol *);
    inline void InsertConstructorSymbol(MethodSymbol *);
    inline MethodSymbol *FindMethodSymbol(NameSymbol *);
    inline VariableSymbol *FindVariableSymbol(NameSymbol *);
    inline VariableSymbol *InsertVariableSymbol(NameSymbol *);
    inline void InsertVariableSymbol(VariableSymbol *);

    inline MethodSymbol *InsertMethodSymbol(NameSymbol *);
    inline void InsertMethodSymbol(MethodSymbol *);
    inline MethodSymbol *Overload(MethodSymbol *);
    inline void Overload(MethodSymbol *, MethodSymbol *);
    inline MethodSymbol *LocalConstructorOverload(MethodSymbol *);
    MethodSymbol *FindOverloadMethod(MethodSymbol *, AstMethodDeclarator *);

    inline void CompressSpace();

private:
    //
    // The fields hash_address and next_type are used in the class
    // TypeLookupTable to contruct a mapping from each fully_qualified name
    // into the type that it defines.
    //
    friend class TypeLookupTable;
    unsigned hash_address;
    TypeSymbol *next_type;

    NameSymbol *external_name_symbol;

    SymbolTable *table;
    SymbolMap *local_shadow_map;

    unsigned short status;

    PackageSymbol *package;
    char *class_name;

    void SetClassName();

    MethodSymbol *class_literal_method;
    Utf8LiteralValue *class_literal_name;

    VariableSymbol *assert_variable;

    //
    // For a local type, when we first encounter an embedded call to one of
    // its constructors or a constructor of one of its inner types, either via
    // a ClassInstanceCreation or an ExplicitConstructorInvocation, we record
    // it and resolve it after we have computed all necessary information
    // about the type and its inner types.
    //
    Tuple<SemanticEnvironment *> *local_constructor_call_environments;

    //
    // When an inner class tries to access a private member of one of its
    // enclosing classes, one (or two) access method(s) to read (and/or write)
    // the private member is (are) generated.
    //
    // The maps read_methods and write_methods are used to keep track of the
    // read and write method to which a member has been mapped.
    //
    Tuple<MethodSymbol *> *private_access_methods,
                          *private_access_constructors;

    inline void MapSymbolToReadMethod(Symbol *, TypeSymbol *, MethodSymbol *);
    inline MethodSymbol *ReadMethod(Symbol *, TypeSymbol *);
    inline void MapSymbolToWriteMethod(VariableSymbol *, TypeSymbol *,
                                       MethodSymbol *);
    inline MethodSymbol *WriteMethod(VariableSymbol *, TypeSymbol *);

    Map<Symbol, Map<TypeSymbol, MethodSymbol> > *read_methods;
    Map<VariableSymbol, Map<TypeSymbol, MethodSymbol> > *write_methods;
    TypeSymbol *placeholder_type;

    //
    // For an accessible inner class the first element in this array
    // identifies the "this$0" pointer of the containing type. For a local
    // class, in addition to the this$0 pointer (if it is needed), all local
    // variables that are referred to in the local type are passed as argument
    // to the local type and copied in the constructor into a local field.
    // These local variables are stored in constructor_parameters.
    //
    // The array enclosing_instances is there for optimization purposes.
    // If this type is deeply nested within several other types and it makes
    // references to members in the enclosing types, then it might
    // be useful to keep a reference to each of these enclosing
    // instances in the form of this$0, this$1, this$2, ...
    //
    // The array class_identities is used to store static variables of type
    // Class that contain the proper value for a given type.
    //
    Tuple<VariableSymbol *> *constructor_parameters;
    VariableSymbol *enclosing_instance;
    Tuple<VariableSymbol *> *class_literals;

    Tuple<char *> *nested_type_signatures;

    //
    // The inner types that appear immediately within this type in the order
    // in which they should be processed (compiled).
    //
    Tuple<TypeSymbol *> *nested_types;
    // The interfaces that were declared in the header of the type.
    Tuple<TypeSymbol *> *interfaces;
    // The anonymous types that were declared in this type.
    Tuple<TypeSymbol *> *anonymous_types;

    //
    // The arrays of this type that were declared.
    //
    Tuple<TypeSymbol *> *array;
    inline int NumArrays()
    {
        return (array ? array -> Length() : 0);
    }
    inline TypeSymbol *Array(int i)
    {
        return (*array)[i];
    }
    inline void AddArrayType(TypeSymbol *type_symbol)
    {
        if (! array)
            array = new Tuple<TypeSymbol *>(4);
        array -> Next() = type_symbol;
    }
};


class VariableSymbol : public Symbol, public AccessFlags
{
public:
    AstVariableDeclarator *declarator;

    NameSymbol *name_symbol;
    Symbol *owner;
    LiteralValue *initial_value;

    VariableSymbol *accessed_local;

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }

    void SetExternalIdentity(NameSymbol *external_name_symbol_)
    {
        external_name_symbol = external_name_symbol_;
    }
    NameSymbol *ExternalIdentity()
    {
        return (external_name_symbol ? external_name_symbol : name_symbol);
    }
    wchar_t *ExternalName()
    {
        return (external_name_symbol ? external_name_symbol -> Name()
                : name_symbol -> Name());
    }
    int ExternalNameLength()
    {
        return (external_name_symbol ? external_name_symbol -> NameLength()
                : name_symbol -> NameLength());
    }
    char *ExternalUtf8Name()
    {
        return (char *) (external_name_symbol
                         ? external_name_symbol -> Utf8_literal -> value
                         : (name_symbol -> Utf8_literal
                            ? name_symbol -> Utf8_literal -> value : NULL));
    }
    int ExternalUtf8NameLength()
    {
        return (external_name_symbol
                ? (external_name_symbol -> Utf8_literal
                   ? external_name_symbol -> Utf8_literal -> length : 0)
                : (name_symbol -> Utf8_literal
                   ? name_symbol -> Utf8_literal -> length : 0));
    }

    VariableSymbol(NameSymbol *name_symbol_)
        : declarator(NULL),
          name_symbol(name_symbol_),
          owner(NULL),
          initial_value(NULL),
          accessed_local(NULL),
          external_name_symbol(NULL),
          status(0),
          local_variable_index(-1),
          type_(NULL),
          signature_string(NULL)
    {
        Symbol::_kind = VARIABLE;
    }

    virtual ~VariableSymbol() { delete [] signature_string; }

    void SetOwner(Symbol *owner_)
    {
        owner = owner_;
        assert(owner -> TypeCast() || owner -> MethodCast());
    }

    TypeSymbol *ContainingType()
    {
        MethodSymbol *method_owner = owner -> MethodCast();
        return method_owner ? method_owner -> containing_type
            : owner -> TypeCast();
    }

    void SetLocalVariableIndex(int index) { local_variable_index = index; }
    //
    // For local variables, returns the index allocated to the variable.
    //
    int LocalVariableIndex() { return local_variable_index; }
    //
    // Returns the local variable index, but for local variables, it adds an
    // offset to account for all final fields. This version should only be
    // needed for definite assignment analysis.
    //
    int LocalVariableIndex(Semantic *);

    bool IsTyped() { return type_ != NULL; }

    void SetType(TypeSymbol *_type) { type_ = _type; }

    void ProcessVariableSignature(Semantic *, LexStream::TokenIndex);

    TypeSymbol *Type()
    {
        // Make sure that the method signature associated with this method is
        // processed prior to invoking this function.
        // ( "this -> ProcessVariableSignature(sem, tok);" )
        assert(type_);

        return type_;
    }

    void SetSignatureString(const char *signature_, int length)
    {
        signature_string = new char[length + 1];
        strncpy(signature_string, signature_, length);
        signature_string[length] = U_NULL;
    }

    // Is variable a local variable?
    bool IsLocal() { return owner -> MethodCast() != NULL; }
    // Is variable local to a particular method ?
    bool IsLocal(MethodSymbol *method) { return owner == method; }

    bool IsFinal(TypeSymbol *type) { return owner == type && ACC_FINAL(); }

    //
    // These functions are used to identify when the declaration of a field
    // in the body of a class is complete.
    //
    void MarkComplete() { status |= COMPLETE; }
    bool IsDeclarationComplete() { return (status & COMPLETE) != 0; }

    void MarkSynthetic() { status |= SYNTHETIC; }
    bool IsSynthetic() { return (status & SYNTHETIC) != 0; }

    void MarkDeprecated() { status |= DEPRECATED; }
    bool IsDeprecated() { return (status & DEPRECATED) != 0; }

    void MarkInitialized() { status |= INITIALIZED; }
    bool IsInitialized() { return (status & INITIALIZED) != 0; }

private:
    enum {
        COMPLETE = 0x01, // Used to prevent use of field before declaration
        SYNTHETIC = 0x02, // Used to mark compiler-created variables
        DEPRECATED = 0x04, // Used to mark deprecated fields
        INITIALIZED = 0x08 // Used when initial value of final field is known
    };
    NameSymbol *external_name_symbol;

    unsigned char status;
    int local_variable_index;
    TypeSymbol *type_;
    char *signature_string;
};


class BlockSymbol : public Symbol
{
public:
    int max_variable_index,
        try_or_synchronized_variable_index;

    BlockSymbol(int hash_size);
    virtual ~BlockSymbol();

    int NumVariableSymbols();
    VariableSymbol *VariableSym(int);

    inline VariableSymbol *FindVariableSymbol(NameSymbol *);
    inline VariableSymbol *InsertVariableSymbol(NameSymbol *);
    inline void InsertVariableSymbol(VariableSymbol *);
    inline BlockSymbol *InsertBlockSymbol(int);

    inline void CompressSpace();

    inline SymbolTable *Table();

private:

    SymbolTable *table;
};


class LabelSymbol : public Symbol
{
public:
    AstBlock *block; // the block that is labeled by this symbol
    NameSymbol *name_symbol;

    int nesting_level;

    virtual wchar_t *Name() { return name_symbol -> Name(); }
    virtual size_t NameLength() { return name_symbol -> NameLength(); }
    virtual NameSymbol *Identity() { return name_symbol; }
    char *Utf8Name()
    {
        return (char *) (name_symbol -> Utf8_literal
                         ? name_symbol -> Utf8_literal -> value : NULL);
    }
    int Utf8NameLength()
    {
        return (name_symbol -> Utf8_literal
                ? name_symbol -> Utf8_literal -> length : 0);
    }

    LabelSymbol(NameSymbol *name_symbol_) : block(NULL),
                                            name_symbol(name_symbol_),
                                            nesting_level(0)
    {
        Symbol::_kind = LABEL;
    }

    virtual ~LabelSymbol() {}
};


class SymbolTable
{
public:
    enum
    {
        DEFAULT_HASH_SIZE = 13,
        MAX_HASH_SIZE = 1021
    };

    int NumAnonymousSymbols()
    {
        return (anonymous_symbol_pool ? anonymous_symbol_pool -> Length() : 0);
    }
    TypeSymbol *AnonymousSym(int i)
    {
        return (*anonymous_symbol_pool)[i];
    }
    void AddAnonymousSymbol(TypeSymbol *symbol)
    {
        if (! anonymous_symbol_pool)
            anonymous_symbol_pool = new ConvertibleArray<TypeSymbol *>(256);
        anonymous_symbol_pool -> Next() = symbol;
    }

    int NumTypeSymbols()
    {
        return (type_symbol_pool ? type_symbol_pool -> Length() : 0);
    }
    TypeSymbol *&TypeSym(int i)
    {
        return (*type_symbol_pool)[i];
    }
    void AddTypeSymbol(TypeSymbol *symbol)
    {
        if (! type_symbol_pool)
            type_symbol_pool = new ConvertibleArray<TypeSymbol *>(256);
        type_symbol_pool -> Next() = symbol;
    }

    int NumMethodSymbols()
    {
        return (method_symbol_pool ? method_symbol_pool -> Length() : 0);
    }
    MethodSymbol *MethodSym(int i)
    {
        return (*method_symbol_pool)[i];
    }
    void AddMethodSymbol(MethodSymbol *symbol)
    {
        if (! method_symbol_pool)
            method_symbol_pool = new ConvertibleArray<MethodSymbol *>(256);
        method_symbol_pool -> Next() = symbol;
    }

    int NumVariableSymbols()
    {
        return (variable_symbol_pool ? variable_symbol_pool -> Length() : 0);
    }
    VariableSymbol *VariableSym(int i)
    {
        return (*variable_symbol_pool)[i];
    }
    void AddVariableSymbol(VariableSymbol *symbol)
    {
        if (! variable_symbol_pool)
            variable_symbol_pool = new ConvertibleArray<VariableSymbol *>(256);
        variable_symbol_pool -> Next() = symbol;
    }

    int NumOtherSymbols()
    {
        return (other_symbol_pool ? other_symbol_pool -> Length() : 0);
    }
    Symbol *OtherSym(int i)
    {
        return (*other_symbol_pool)[i];
    }
    void AddOtherSymbol(Symbol *symbol)
    {
        if (! other_symbol_pool)
            other_symbol_pool = new ConvertibleArray<Symbol *>(256);
        other_symbol_pool -> Next() = symbol;
    }

    SymbolTable(int hash_size_ = DEFAULT_HASH_SIZE);
    ~SymbolTable();

    inline void CompressSpace()
    {
        if (anonymous_symbol_pool)
            (void) anonymous_symbol_pool -> Array();
        if (method_symbol_pool)
            (void) method_symbol_pool -> Array();
        if (variable_symbol_pool)
            (void) variable_symbol_pool -> Array();
        if (other_symbol_pool)
            (void) other_symbol_pool -> Array();
    }

private:

    Tuple<TypeSymbol *> *type_symbol_pool; // This array should not be convertible. See SymbolTable::DeleteTypeSymbol

    ConvertibleArray<TypeSymbol *>     *anonymous_symbol_pool;
    ConvertibleArray<MethodSymbol *>   *method_symbol_pool;
    ConvertibleArray<VariableSymbol *> *variable_symbol_pool;
    ConvertibleArray<Symbol *>         *other_symbol_pool;

    Symbol **base;
    int hash_size;

    static int primes[];
    int prime_index;

    int Size()
    {
        return NumAnonymousSymbols() +
               NumTypeSymbols() +
               NumMethodSymbols() +
               NumVariableSymbols() +
               NumOtherSymbols();
    }
    void Rehash();

    MethodSymbol *constructor;

public:

    inline PathSymbol *InsertPathSymbol(NameSymbol *, DirectorySymbol *);
    inline PathSymbol *FindPathSymbol(NameSymbol *);
    inline DirectorySymbol *InsertDirectorySymbol(NameSymbol *, Symbol *,
                                                  bool source_path);
    inline DirectorySymbol *FindDirectorySymbol(NameSymbol *);
    inline FileSymbol *InsertFileSymbol(NameSymbol *);
    inline FileSymbol *FindFileSymbol(NameSymbol *);
    inline PackageSymbol *InsertPackageSymbol(NameSymbol *, PackageSymbol *);
    inline PackageSymbol *FindPackageSymbol(NameSymbol *);
    inline TypeSymbol *InsertAnonymousTypeSymbol(NameSymbol *);
    inline TypeSymbol *InsertSystemTypeSymbol(NameSymbol *);
    inline TypeSymbol *InsertOuterTypeSymbol(NameSymbol *);
    inline TypeSymbol *InsertNestedTypeSymbol(NameSymbol *);
    inline void DeleteTypeSymbol(TypeSymbol *);
    inline void DeleteAnonymousTypes();
    inline TypeSymbol *FindTypeSymbol(NameSymbol *);
    inline MethodSymbol *InsertMethodSymbol(NameSymbol *);
    inline MethodSymbol *InsertConstructorSymbol(NameSymbol *);
    inline void InsertMethodSymbol(MethodSymbol *);
    inline void InsertConstructorSymbol(MethodSymbol *);
    inline MethodSymbol *FindMethodSymbol(NameSymbol *);
    inline MethodSymbol *FindConstructorSymbol();
    inline VariableSymbol *InsertVariableSymbol(NameSymbol *);
    inline void InsertVariableSymbol(VariableSymbol *);
    inline VariableSymbol *FindVariableSymbol(NameSymbol *);
    inline LabelSymbol *InsertLabelSymbol(NameSymbol *);
    inline LabelSymbol *FindLabelSymbol(NameSymbol *);
    inline BlockSymbol *InsertBlockSymbol(int);

    inline MethodSymbol *Overload(MethodSymbol *);
    inline void Overload(MethodSymbol *, MethodSymbol *);
    inline MethodSymbol *LocalConstructorOverload(MethodSymbol *);
    MethodSymbol *FindOverloadMethod(MethodSymbol *, AstMethodDeclarator *);
};

inline bool MethodSymbol::AccessesInstanceMember() {
    return (accessed_member &&
            ((accessed_member -> MethodCast() &&
              ! accessed_member -> MethodCast() -> ACC_STATIC()) ||
             (accessed_member -> VariableCast() &&
              ! accessed_member -> VariableCast() -> ACC_STATIC())));
}

inline int TypeSymbol::NumVariableSymbols()
{
    return (table ? table -> NumVariableSymbols() : 0);
}
inline VariableSymbol *TypeSymbol::VariableSym(int i)
{
    return table -> VariableSym(i);
}

inline int BlockSymbol::NumVariableSymbols()
{
    return (table ? table -> NumVariableSymbols() : 0);
}
inline VariableSymbol *BlockSymbol::VariableSym(int i)
{
    return table -> VariableSym(i);
}

inline int TypeSymbol::NumMethodSymbols()
{
    return (table ? table -> NumMethodSymbols() : 0);
}
inline MethodSymbol *TypeSymbol::MethodSym(int i)
{
    return table -> MethodSym(i);
}

inline int TypeSymbol::NumTypeSymbols()
{
    return (table ? table -> NumTypeSymbols() : 0);
}
inline TypeSymbol *TypeSymbol::TypeSym(int i) { return table -> TypeSym(i); }

inline void TypeSymbol::CompressSpace()
{
    if (table)
        table -> CompressSpace();
}
inline void BlockSymbol::CompressSpace()
{
    if (table)
        table -> CompressSpace();
}

inline PathSymbol *SymbolTable::InsertPathSymbol(NameSymbol *name_symbol,
                                                 DirectorySymbol *directory_symbol)
{
    assert(base);

    PathSymbol *symbol = new PathSymbol(name_symbol);
    directory_symbol -> owner = symbol;
    symbol -> root_directory = directory_symbol;
    AddOtherSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline PathSymbol *SymbolTable::FindPathSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
            return (PathSymbol *) symbol;
    }

    return (PathSymbol *) NULL;
}


inline DirectorySymbol *SymbolTable::InsertDirectorySymbol(NameSymbol *name_symbol,
                                                           Symbol *owner,
                                                           bool source_path)
{
    assert(base);

    DirectorySymbol *symbol = new DirectorySymbol(name_symbol, owner,
                                                  source_path);
    AddOtherSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline DirectorySymbol *DirectorySymbol::InsertDirectorySymbol(NameSymbol *name_symbol,
                                                               bool source_dir)
{
    DirectorySymbol *subdirectory_symbol = Table() -> InsertDirectorySymbol(name_symbol, this, source_dir);
    subdirectories.Next() = subdirectory_symbol;

    return subdirectory_symbol;
}


inline DirectorySymbol *SymbolTable::FindDirectorySymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            DirectorySymbol *directory_symbol = symbol -> DirectoryCast();
            if (directory_symbol)
                return directory_symbol;
        }
    }

    return (DirectorySymbol *) NULL;
}


inline DirectorySymbol *DirectorySymbol::FindDirectorySymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindDirectorySymbol(name_symbol)
            : (DirectorySymbol *) NULL);
}


inline FileSymbol *SymbolTable::InsertFileSymbol(NameSymbol *name_symbol)
{
    assert(base);

    FileSymbol *symbol = new FileSymbol(name_symbol);
    AddOtherSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline FileSymbol *DirectorySymbol::InsertFileSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertFileSymbol(name_symbol);
}


inline FileSymbol *SymbolTable::FindFileSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            FileSymbol *file_symbol = symbol -> FileCast();
            if (file_symbol)
                return file_symbol;
        }
    }

    return (FileSymbol *) NULL;
}


inline FileSymbol *DirectorySymbol::FindFileSymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindFileSymbol(name_symbol)
            : (FileSymbol *) NULL);
}


inline PackageSymbol *SymbolTable::InsertPackageSymbol(NameSymbol *name_symbol,
                                                       PackageSymbol *owner)
{
    assert(base);

    PackageSymbol *symbol = new PackageSymbol(name_symbol, owner);
    AddOtherSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline PackageSymbol *PackageSymbol::InsertPackageSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertPackageSymbol(name_symbol, this);
}


inline PackageSymbol *SymbolTable::FindPackageSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            PackageSymbol *package_symbol = symbol -> PackageCast();
            if (package_symbol)
                return package_symbol;
        }
    }

    return (PackageSymbol *) NULL;
}


inline PackageSymbol *PackageSymbol::FindPackageSymbol(NameSymbol *name_symbol)
{
  return (table ? table -> FindPackageSymbol(name_symbol)
          : (PackageSymbol *) NULL);
}

inline TypeSymbol *SymbolTable::InsertAnonymousTypeSymbol(NameSymbol *name_symbol)
{
    TypeSymbol *symbol = new TypeSymbol(name_symbol);
    AddAnonymousSymbol(symbol);

    return symbol;
}


inline TypeSymbol *TypeSymbol::InsertAnonymousTypeSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertAnonymousTypeSymbol(name_symbol);
}


inline TypeSymbol *SymbolTable::InsertSystemTypeSymbol(NameSymbol *name_symbol)
{
    assert(base);

    TypeSymbol *symbol = new TypeSymbol(name_symbol);
    symbol -> pool_index = NumTypeSymbols();
    AddTypeSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline TypeSymbol *PackageSymbol::InsertSystemTypeSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertSystemTypeSymbol(name_symbol);
}


inline TypeSymbol *SymbolTable::InsertOuterTypeSymbol(NameSymbol *name_symbol)
{
    assert(base);

    TypeSymbol *symbol = new TypeSymbol(name_symbol);
    symbol -> pool_index = NumTypeSymbols();
    AddTypeSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline TypeSymbol *PackageSymbol::InsertOuterTypeSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertOuterTypeSymbol(name_symbol);
}


inline TypeSymbol *SymbolTable::InsertNestedTypeSymbol(NameSymbol *name_symbol)
{
    assert(base);

    TypeSymbol *symbol = new TypeSymbol(name_symbol);
    symbol -> pool_index = NumTypeSymbols();
    AddTypeSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline TypeSymbol *TypeSymbol::InsertNestedTypeSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertNestedTypeSymbol(name_symbol);
}


inline void SymbolTable::DeleteTypeSymbol(TypeSymbol *type)
{
    assert(base);

    int k = type -> name_symbol -> index % hash_size;
    if (type == base[k])
        base[k] = type -> next;
    else
    {
        Symbol *previous = base[k];
        for (Symbol *symbol = previous -> next; symbol != type; previous = symbol, symbol = symbol -> next)
            ;
        previous -> next = type -> next;
    }

    int last_index = NumTypeSymbols() - 1;
    if (type -> pool_index != last_index)
    {
        // Move last element to position previously occupied by element being
        // deleted
        TypeSym(last_index) -> pool_index = type -> pool_index;
        TypeSym(type -> pool_index) = TypeSym(last_index);
    }

    type_symbol_pool -> Reset(last_index); // remove last slot in symbol_pool

    delete type;
}


inline void PackageSymbol::DeleteTypeSymbol(TypeSymbol *type)
{
    if (table)
        table -> DeleteTypeSymbol(type);
}


inline void SymbolTable::DeleteAnonymousTypes()
{
    for (int i = 0; i < NumAnonymousSymbols(); i++)
        delete AnonymousSym(i);
    delete anonymous_symbol_pool;
    anonymous_symbol_pool = NULL;
}


inline void TypeSymbol::DeleteAnonymousTypes()
{
    delete anonymous_types;
    anonymous_types = NULL;
    if (table)
        table -> DeleteAnonymousTypes();
}

inline TypeSymbol *SymbolTable::FindTypeSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            TypeSymbol *type = symbol -> TypeCast();
            if (type)
                return type;
        }
    }

    return (TypeSymbol *) NULL;
}


inline TypeSymbol *PackageSymbol::FindTypeSymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindTypeSymbol(name_symbol)
            : (TypeSymbol *) NULL);
}


inline TypeSymbol *TypeSymbol::FindTypeSymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindTypeSymbol(name_symbol)
            : (TypeSymbol *) NULL);
}


inline MethodSymbol *SymbolTable::InsertMethodSymbol(NameSymbol *name_symbol)
{
    assert(base);

    MethodSymbol *symbol = new MethodSymbol(name_symbol);
    AddMethodSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline MethodSymbol *TypeSymbol::InsertMethodSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertMethodSymbol(name_symbol);
}


inline MethodSymbol *SymbolTable::InsertConstructorSymbol(NameSymbol *name_symbol)
{
    assert(! constructor);

    constructor = InsertMethodSymbol(name_symbol);
    return constructor;
}


inline MethodSymbol *TypeSymbol::InsertConstructorSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertConstructorSymbol(name_symbol);
}


inline void SymbolTable::InsertMethodSymbol(MethodSymbol *method_symbol)
{
    assert(base);

    AddMethodSymbol(method_symbol);

    int k = method_symbol -> name_symbol -> index % hash_size;
    method_symbol -> next = base[k];
    base[k] = method_symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();
}


inline void TypeSymbol::InsertMethodSymbol(MethodSymbol *method_symbol)
{
    Table() -> InsertMethodSymbol(method_symbol);
}


inline void SymbolTable::InsertConstructorSymbol(MethodSymbol *method_symbol)
{
    assert(! constructor);

    constructor = method_symbol;
    InsertMethodSymbol(method_symbol);
}


inline void TypeSymbol::InsertConstructorSymbol(MethodSymbol *method_symbol)
{
    Table() -> InsertConstructorSymbol(method_symbol);
}


inline MethodSymbol *SymbolTable::FindMethodSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            MethodSymbol *method = symbol -> MethodCast();
            if (method)
                return method;
        }
    }

    return (MethodSymbol *) NULL;
}


inline MethodSymbol *TypeSymbol::FindMethodSymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindMethodSymbol(name_symbol)
            : (MethodSymbol *) NULL);
}


inline MethodSymbol *SymbolTable::FindConstructorSymbol()
{
    return constructor;
}

inline MethodSymbol *TypeSymbol::FindConstructorSymbol()
{
    return (table ? table -> FindConstructorSymbol()
            : (MethodSymbol *) NULL);
}

inline VariableSymbol *SymbolTable::InsertVariableSymbol(NameSymbol *name_symbol)
{
    assert(base);

    VariableSymbol *symbol = new VariableSymbol(name_symbol);
    AddVariableSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();

    return symbol;
}


inline VariableSymbol *TypeSymbol::InsertVariableSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertVariableSymbol(name_symbol);
}


inline VariableSymbol *BlockSymbol::InsertVariableSymbol(NameSymbol *name_symbol)
{
    return Table() -> InsertVariableSymbol(name_symbol);
}


inline void SymbolTable::InsertVariableSymbol(VariableSymbol *variable_symbol)
{
    assert(base);

    AddVariableSymbol(variable_symbol);

    int k = variable_symbol -> name_symbol -> index % hash_size;
    variable_symbol -> next = base[k];
    base[k] = variable_symbol;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if ((Size() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
        Rehash();
}


inline void TypeSymbol::InsertVariableSymbol(VariableSymbol *variable_symbol)
{
    Table() -> InsertVariableSymbol(variable_symbol);
}


inline void BlockSymbol::InsertVariableSymbol(VariableSymbol *variable_symbol)
{
    Table() -> InsertVariableSymbol(variable_symbol);
}


inline VariableSymbol *SymbolTable::FindVariableSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            VariableSymbol *variable_symbol = symbol -> VariableCast();
            if (variable_symbol)
                return variable_symbol;
        }
    }

    return (VariableSymbol *) NULL;
}


inline VariableSymbol *TypeSymbol::FindVariableSymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindVariableSymbol(name_symbol)
            : (VariableSymbol *) NULL);
}


inline VariableSymbol *BlockSymbol::FindVariableSymbol(NameSymbol *name_symbol)
{
    return (table ? table -> FindVariableSymbol(name_symbol)
            : (VariableSymbol *) NULL);
}


inline LabelSymbol *SymbolTable::InsertLabelSymbol(NameSymbol *name_symbol)
{
    assert(base);

    LabelSymbol *symbol = new LabelSymbol(name_symbol);
    AddOtherSymbol(symbol);

    int k = name_symbol -> index % hash_size;
    symbol -> next = base[k];
    base[k] = symbol;

    //
    // as only one label can be inserted in any given symboltable,
    // we don't need to try to rehash here !
    //

    return symbol;
}


inline LabelSymbol *SymbolTable::FindLabelSymbol(NameSymbol *name_symbol)
{
    assert(base);

    for (Symbol *symbol = base[name_symbol -> index % hash_size];
         symbol; symbol = symbol -> next)
    {
        if (name_symbol == symbol -> Identity())
        {
            LabelSymbol *label = symbol -> LabelCast();
            if (label)
                return label;
        }
    }

    return (LabelSymbol *) NULL;
}

inline BlockSymbol *SymbolTable::InsertBlockSymbol(int hash_size = 0)
{
    BlockSymbol *symbol = new BlockSymbol(hash_size);
    AddOtherSymbol(symbol);

    return symbol;
}


inline BlockSymbol *BlockSymbol::InsertBlockSymbol(int hash_size = 0)
{
    return Table() -> InsertBlockSymbol(hash_size);
}


inline MethodSymbol *SymbolTable::Overload(MethodSymbol *base_method)
{
    MethodSymbol *overload_method = new MethodSymbol(base_method -> Identity());
    AddMethodSymbol(overload_method);

    overload_method -> next = overload_method; // mark overloaded method
    overload_method -> next_method = base_method -> next_method;
    base_method -> next_method = overload_method;

    return overload_method;
}


inline MethodSymbol *TypeSymbol::Overload(MethodSymbol *base_method)
{
    assert(table);

    return table -> Overload(base_method);
}


inline void SymbolTable::Overload(MethodSymbol *base_method,
                                  MethodSymbol *overload_method)
{
    AddMethodSymbol(overload_method);

    overload_method -> next = overload_method; // mark overloaded method
    overload_method -> next_method = base_method -> next_method;
    base_method -> next_method = overload_method;
}

inline void TypeSymbol::Overload(MethodSymbol *base_method,
                                 MethodSymbol *overload_method)
{
    assert(table);

    table -> Overload(base_method, overload_method);
}


inline MethodSymbol *SymbolTable::LocalConstructorOverload(MethodSymbol *base_method)
{
    MethodSymbol *overload_method = new MethodSymbol(base_method -> Identity());
    AddMethodSymbol(overload_method);

    overload_method -> next = overload_method; // mark overloaded method

    return overload_method;
}


inline MethodSymbol *TypeSymbol::LocalConstructorOverload(MethodSymbol *base_method)
{
    assert(table);

    return table -> LocalConstructorOverload(base_method);
}


inline MethodSymbol *TypeSymbol::FindOverloadMethod(MethodSymbol *base_method,
                                                    AstMethodDeclarator *method_declarator)
{
    return (table ? table -> FindOverloadMethod(base_method,
                                                method_declarator)
            : (MethodSymbol *) NULL);
}


inline SymbolTable *DirectorySymbol::Table()
{
    return (table ? table : table = new SymbolTable(101));
}

inline SymbolTable *PackageSymbol::Table()
{
    return (table ? table : table = new SymbolTable(101));
}

inline void TypeSymbol::SetSymbolTable(int estimate)
{
    if (! table) // If table was not yet allocated, allocate one based on the estimate
        table = new SymbolTable(estimate);
}

inline SymbolTable *TypeSymbol::Table()
{
    return (table ? table : table = new SymbolTable());
}

inline SymbolTable *BlockSymbol::Table()
{
    return (table ? table : table = new SymbolTable());
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // symbol_INCLUDED

