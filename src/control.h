// $Id: control.h,v 1.49 2004/01/26 06:07:15 cabbey Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef control_INCLUDED
#define control_INCLUDED

#include "platform.h"
#include "symbol.h"
#include "tuple.h"
#include "set.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class StoragePool;
class Option;
class Scanner;
class Parser;
class Semantic;
class LexStream;
class AstPackageDeclaration;
class AstName;
class TypeDependenceChecker;

class Control : public StringConstant
{
public:
    int return_code;
    Option& option;
    SymbolTable classpath_table;
    SymbolTable external_table;

    PackageSymbol* system_package;
    PackageSymbol* unnamed_package;
    unsigned dot_classpath_index;
    Tuple<PathSymbol*> classpath;
    Tuple<wchar_t*> bad_dirnames;
    Tuple<wchar_t*> bad_zip_filenames;
    Tuple<wchar_t*> bad_input_filenames;
    Tuple<wchar_t*> unreadable_input_filenames;

    SystemTable* system_table;
    Tuple<DirectorySymbol*> system_directories;

    Semantic* system_semantic;
    Tuple<Semantic*> semantic;
    Tuple<TypeSymbol*> needs_body_work;
    Tuple<TypeSymbol*> type_trash_bin;

    NameSymbolMap unnamed_package_types;

    SymbolSet input_java_file_set;
    SymbolSet input_class_file_set;
    SymbolSet expired_file_set;
    SymbolSet recompilation_file_set;

    Parser* parser;
    Scanner* scanner;

    //
    //
    //
    LiteralLookupTable string_table;
    LiteralLookupTable int_table;
    LiteralLookupTable long_table;
    LiteralLookupTable char_table;
    LiteralLookupTable float_table;
    LiteralLookupTable double_table;
    NameLookupTable name_table;
    TypeLookupTable type_table;

    //
    //
    //
    NameSymbol* dot_name_symbol;
    NameSymbol* dot_dot_name_symbol;
    NameSymbol* length_name_symbol;
    NameSymbol* init_name_symbol;
    NameSymbol* clinit_name_symbol;
    NameSymbol* block_init_name_symbol;
    NameSymbol* this0_name_symbol;
    NameSymbol* clone_name_symbol;
    NameSymbol* object_name_symbol;
    NameSymbol* type_name_symbol;
    NameSymbol* class_name_symbol;
    NameSymbol* equals_name_symbol;
    NameSymbol* hashCode_name_symbol;
    NameSymbol* serialVersionUID_name_symbol;
    NameSymbol* toString_name_symbol;
    NameSymbol* append_name_symbol;
    NameSymbol* forName_name_symbol;
    NameSymbol* getMessage_name_symbol;
    NameSymbol* desiredAssertionStatus_name_symbol;
    NameSymbol* getClass_name_symbol;
    NameSymbol* getComponentType_name_symbol;
    NameSymbol* initCause_name_symbol;

    //
    //
    //
    TypeSymbol* byte_type;
    TypeSymbol* short_type;
    TypeSymbol* int_type;
    TypeSymbol* long_type;
    TypeSymbol* char_type;
    TypeSymbol* float_type;
    TypeSymbol* double_type;
    TypeSymbol* boolean_type;
    TypeSymbol* void_type;
    TypeSymbol* null_type;
    TypeSymbol* no_type;

    //
    TypeSymbol* GetType(PackageSymbol*, const wchar_t*);

    inline TypeSymbol* Serializable()
    {
        if (! Serializable_type)
        {
            PackageSymbol* io_package =
                ProcessPackage(US_java_SL_io);
            FindPathsToDirectory(io_package);
            Serializable_type = GetType(io_package, US_Serializable);
        }
        return Serializable_type;
    }

    inline TypeSymbol* Object()
    {
        return Object_type ? Object_type
            : Object_type = GetType(system_package, US_Object);
    }

    MethodSymbol* Object_getClassMethod();

    inline TypeSymbol* Cloneable()
    {
        return Cloneable_type ? Cloneable_type
            : Cloneable_type = GetType(system_package, US_Cloneable);
    }

    inline TypeSymbol* String()
    {
        return String_type ? String_type
            : String_type = GetType(system_package, US_String);
    }

    inline TypeSymbol* Void()
    {
        return Void_type ? Void_type
            : Void_type = GetType(system_package, US_Void);
    }

    inline TypeSymbol* Boolean()
    {
        return Boolean_type ? Boolean_type
            : Boolean_type = GetType(system_package, US_Boolean);
    }

    inline TypeSymbol* Byte()
    {
        return Byte_type ? Byte_type
            : Byte_type = GetType(system_package, US_Byte);
    }

    inline TypeSymbol* Short()
    {
        return Short_type ? Short_type
            : Short_type = GetType(system_package, US_Short);
    }

    inline TypeSymbol* Character()
    {
        return Character_type ? Character_type
            : Character_type = GetType(system_package, US_Character);
    }

    inline TypeSymbol* Integer()
    {
        return Integer_type ? Integer_type
            : Integer_type = GetType(system_package, US_Integer);
    }

    inline TypeSymbol* Long()
    {
        return Long_type ? Long_type
            : Long_type = GetType(system_package, US_Long);
    }

    inline TypeSymbol* Float()
    {
        return Float_type ? Float_type
            : Float_type = GetType(system_package, US_Float);
    }

    inline TypeSymbol* Double()
    {
        return Double_type ? Double_type
            : Double_type = GetType(system_package, US_Double);
    }

    inline TypeSymbol* Comparable()
    {
        return Comparable_type ? Comparable_type
            : Comparable_type = GetType(system_package, US_Comparable);
    }

    void InitAssertionErrorInfo();
    inline TypeSymbol* AssertionError()
    {
        if (! AssertionError_type)
        {
            AssertionError_type = GetType(system_package, US_AssertionError);
            InitAssertionErrorInfo();
        }
        return AssertionError_type;
    }

    MethodSymbol* AssertionError_InitMethod()
    {
        if (! AssertionError_Init_method)
            (void) AssertionError();
        return AssertionError_Init_method;
    }

    MethodSymbol* AssertionError_InitWithCharMethod()
    {
        if (! AssertionError_InitWithChar_method)
            (void) AssertionError();
        return AssertionError_InitWithChar_method;
    }

    MethodSymbol* AssertionError_InitWithBooleanMethod()
    {
        if (! AssertionError_InitWithBoolean_method)
            (void) AssertionError();
        return AssertionError_InitWithBoolean_method;
    }

    MethodSymbol* AssertionError_InitWithIntMethod()
    {
        if (! AssertionError_InitWithInt_method)
            (void) AssertionError();
        return AssertionError_InitWithInt_method;
    }

    MethodSymbol* AssertionError_InitWithLongMethod()
    {
        if (! AssertionError_InitWithLong_method)
            (void) AssertionError();
        return AssertionError_InitWithLong_method;
    }

    MethodSymbol* AssertionError_InitWithFloatMethod()
    {
        if (! AssertionError_InitWithFloat_method)
            (void) AssertionError();
        return AssertionError_InitWithFloat_method;
    }

    MethodSymbol* AssertionError_InitWithDoubleMethod()
    {
        if (! AssertionError_InitWithDouble_method)
            (void) AssertionError();
        return AssertionError_InitWithDouble_method;
    }

    MethodSymbol* AssertionError_InitWithObjectMethod()
    {
        if (! AssertionError_InitWithObject_method)
            (void) AssertionError();
        return AssertionError_InitWithObject_method;
    }

    void InitClassInfo();
    inline TypeSymbol* Class()
    {
        if (! Class_type)
        {
            Class_type = GetType(system_package, US_Class);
            InitClassInfo();
        }
        return Class_type;
    }

    MethodSymbol* Class_forNameMethod()
    {
        if (! Class_forName_method)
            (void) Class();
        return Class_forName_method;
    }

    MethodSymbol* Class_getComponentTypeMethod()
    {
        if (! Class_getComponentType_method)
            (void) Class();
        return Class_getComponentType_method;
    }

    MethodSymbol* Class_desiredAssertionStatusMethod()
    {
        if (! Class_desiredAssertionStatus_method)
            (void) Class();
        return Class_desiredAssertionStatus_method;
    }


    void InitThrowableInfo();
    inline TypeSymbol* Throwable()
    {
        if (! Throwable_type)
        {
            Throwable_type = GetType(system_package, US_Throwable);
            InitThrowableInfo();
        }
        return Throwable_type;
    }

    MethodSymbol* Throwable_getMessageMethod()
    {
        if (! Throwable_getMessage_method)
            (void) Throwable();
        return Throwable_getMessage_method;
    }

    MethodSymbol* Throwable_initCauseMethod()
    {
        if (! Throwable_initCause_method)
            (void) Throwable();
        return Throwable_initCause_method;
    }

    inline TypeSymbol* Exception()
    {
        return Exception_type ? Exception_type
            : Exception_type = GetType(system_package, US_Exception);
    }

    inline TypeSymbol* RuntimeException()
    {
        return RuntimeException_type ? RuntimeException_type
            : RuntimeException_type = GetType(system_package,
                                              US_RuntimeException);
    }

    inline TypeSymbol* ClassNotFoundException()
    {
        return ClassNotFoundException_type ? ClassNotFoundException_type
            : (ClassNotFoundException_type =
               GetType(system_package, US_ClassNotFoundException));
    }

    inline TypeSymbol* Error()
    {
        return Error_type ? Error_type
            : Error_type = GetType(system_package, US_Error);
    }

    void InitNoClassDefFoundErrorInfo();
    inline TypeSymbol* NoClassDefFoundError()
    {
        if (! NoClassDefFoundError_type)
        {
            NoClassDefFoundError_type =
                GetType(system_package, US_NoClassDefFoundError);
            InitNoClassDefFoundErrorInfo();
        }
        return NoClassDefFoundError_type;
    }

    MethodSymbol* NoClassDefFoundError_InitStringMethod()
    {
        if (! NoClassDefFoundError_InitString_method)
            (void) NoClassDefFoundError();
        return NoClassDefFoundError_InitString_method;
    }

    MethodSymbol* NoClassDefFoundError_InitMethod()
    {
        if (! NoClassDefFoundError_Init_method)
            (void) NoClassDefFoundError();
        return NoClassDefFoundError_Init_method;
    }

    void InitStringBufferInfo();
    inline TypeSymbol* StringBuffer()
    {
        if (! StringBuffer_type)
        {
            StringBuffer_type =
                GetType(system_package, US_StringBuffer);
            InitStringBufferInfo();
        }
        return StringBuffer_type;
    }


    MethodSymbol* StringBuffer_InitMethod()
    {
        if (! StringBuffer_Init_method)
            (void) StringBuffer();
        return StringBuffer_Init_method;
    }


    MethodSymbol* StringBuffer_InitWithStringMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_InitWithString_method;
    }


    MethodSymbol* StringBuffer_toStringMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_toString_method;
    }


    MethodSymbol* StringBuffer_append_charMethod()
    {
        if (! StringBuffer_append_char_method)
            (void) StringBuffer();
        return StringBuffer_append_char_method;
    }


    MethodSymbol* StringBuffer_append_booleanMethod()
    {
        if (! StringBuffer_append_boolean_method)
            (void) StringBuffer();
        return StringBuffer_append_boolean_method;
    }


    MethodSymbol* StringBuffer_append_intMethod()
    {
        if (! StringBuffer_append_int_method)
            (void) StringBuffer();
        return StringBuffer_append_int_method;
    }


    MethodSymbol* StringBuffer_append_longMethod()
    {
        if (! StringBuffer_append_long_method)
            (void) StringBuffer();
        return StringBuffer_append_long_method;
    }


    MethodSymbol* StringBuffer_append_floatMethod()
    {
        if (! StringBuffer_append_float_method)
            (void) StringBuffer();
        return StringBuffer_append_float_method;
    }


    MethodSymbol* StringBuffer_append_doubleMethod()
    {
        if (! StringBuffer_append_double_method)
            (void) StringBuffer();
        return StringBuffer_append_double_method;
    }


    MethodSymbol* StringBuffer_append_stringMethod()
    {
        if (! StringBuffer_append_string_method)
            (void) StringBuffer();
        return StringBuffer_append_string_method;
    }


    MethodSymbol* StringBuffer_append_objectMethod()
    {
        if (! StringBuffer_append_object_method)
            (void) StringBuffer();
        return StringBuffer_append_object_method;
    }

    IntLiteralTable int_pool;
    LongLiteralTable long_pool;
    FloatLiteralTable float_pool;
    DoubleLiteralTable double_pool;
    Utf8LiteralTable Utf8_pool;

    Utf8LiteralValue* ConstantValue_literal;
    Utf8LiteralValue* Exceptions_literal;
    Utf8LiteralValue* InnerClasses_literal;
    Utf8LiteralValue* Synthetic_literal;
    Utf8LiteralValue* Deprecated_literal;
    Utf8LiteralValue* LineNumberTable_literal;
    Utf8LiteralValue* LocalVariableTable_literal;
    Utf8LiteralValue* Code_literal;
    Utf8LiteralValue* SourceFile_literal;

    Utf8LiteralValue* null_literal;
    Utf8LiteralValue* this_literal;

    Control(char**, Option&);
    ~Control();

    Utf8LiteralValue* ConvertUnicodeToUtf8(const wchar_t* source)
    {
        // Should be big enough for the worst case.
        char* target = new char[wcslen(source) * 3 + 1];
        int length = ConvertUnicodeToUtf8(source, target);
        Utf8LiteralValue* literal = Utf8_pool.FindOrInsert(target, length);
        delete [] target;
        return literal;
    }

    static int ConvertUtf8ToUnicode(wchar_t*, const char*, int);

    NameSymbol* ConvertUtf8ToUnicode(const char* source, int length)
    {
        wchar_t* name = new wchar_t[length + 1];
        int name_length = ConvertUtf8ToUnicode(name, source, length);
        NameSymbol* name_symbol = FindOrInsertName(name, name_length);
        delete [] name;
        return name_symbol;
    }

    void FindPathsToDirectory(PackageSymbol*);

    DirectoryEntry* FindInputFile(FileSymbol*);
    void FindMoreRecentInputFiles(SymbolSet&);
    void RemoveTrashedTypes(SymbolSet&);
    void RereadDirectory(DirectorySymbol*);
    void RereadDirectories();
    void ComputeRecompilationSet(TypeDependenceChecker&);
    bool IncrementalRecompilation();

    //
    // The one and only bad value constant.
    //
    LiteralValue* BadValue() { return &bad_value; }

    //
    // Note that only names are converted here and not literals, since
    // no error can occur in a name.
    // A literal is converted during the semantic pass so that an
    // accurate diagnostic can be issued in case it is invalid.
    //
    NameSymbol* FindOrInsertName(const wchar_t* name, int len)
    {
        NameSymbol* name_symbol = name_table.FindOrInsertName(name, len);
        if (! name_symbol -> Utf8_literal)
            name_symbol -> Utf8_literal =
                ConvertUnicodeToUtf8(name_symbol -> Name());
        return name_symbol;
    }

    //
    // Make up a parameter name of the form $(num) and return its name symbol.
    //
    NameSymbol* MakeParameter(int num)
    {
        IntToWstring value(num);
        wchar_t str[13];
        str[0] = U_DOLLAR; // '$'
        wcscpy(&str[1], value.String());
        return FindOrInsertName(str, value.Length() + 1);
    }

    //
    //
    //
    static DirectorySymbol* GetOutputDirectory(FileSymbol*);
    static FileSymbol* GetJavaFile(PackageSymbol*, const NameSymbol*);
    static FileSymbol* GetFile(Control&, PackageSymbol*, const NameSymbol*);
    static FileSymbol* GetFileFirst(Control&, PackageSymbol*,
                                    const NameSymbol*);
    static FileSymbol* GetFileBoth(Control&, PackageSymbol*,
                                   const NameSymbol*);

    PackageSymbol* FindOrInsertPackage(LexStream*, AstName*);
    void ProcessPackageDeclaration(FileSymbol*, AstPackageDeclaration*);
    void CleanUp(FileSymbol*);

    inline bool IsSimpleIntegerValueType(const TypeSymbol* type)
    {
        return type == byte_type || type == short_type ||
            type == int_type || type == char_type;
    }

    inline bool IsIntegral(const TypeSymbol* type)
    {
        return IsSimpleIntegerValueType(type) || type == long_type;
    }

    inline bool IsFloatingPoint(const TypeSymbol* type)
    {
        return type == float_type || type == double_type;
    }

    inline bool IsNumeric(const TypeSymbol* type)
    {
        return IsIntegral(type) || IsFloatingPoint(type);
    }

    inline bool IsDoubleWordType(const TypeSymbol* type)
    {
        return type == long_type || type == double_type;
    }

    inline bool IsPrimitive(const TypeSymbol* type)
    {
        return IsNumeric(type) || type == boolean_type;
    }

    inline void ProcessBadType(TypeSymbol* type_symbol)
    {
        type_trash_bin.Next() = type_symbol;
    }

    void ProcessHeaders(FileSymbol*);

#ifdef JIKES_DEBUG
    int input_files_processed,
        class_files_read,
        class_files_written,
        line_count;
#endif // JIKES_DEBUG

    PackageSymbol* ProcessPackage(const wchar_t*);

    DirectorySymbol* FindSubdirectory(PathSymbol*, wchar_t*, int);
    DirectorySymbol* ProcessSubdirectories(wchar_t*, int, bool);

private:

    LiteralValue bad_value;

    TypeSymbol* Serializable_type;
    TypeSymbol* Object_type;
    TypeSymbol* Cloneable_type;
    TypeSymbol* String_type;
    TypeSymbol* Void_type;
    TypeSymbol* Boolean_type;
    TypeSymbol* Byte_type;
    TypeSymbol* Short_type;
    TypeSymbol* Character_type;
    TypeSymbol* Integer_type;
    TypeSymbol* Long_type;
    TypeSymbol* Float_type;
    TypeSymbol* Double_type;
    TypeSymbol* Comparable_type;
    TypeSymbol* AssertionError_type;
    TypeSymbol* Class_type;
    TypeSymbol* Throwable_type;
    TypeSymbol* Exception_type;
    TypeSymbol* RuntimeException_type;
    TypeSymbol* ClassNotFoundException_type;
    TypeSymbol* Error_type;
    TypeSymbol* NoClassDefFoundError_type;
    TypeSymbol* StringBuffer_type;

    MethodSymbol* Object_getClass_method;

    MethodSymbol* Class_forName_method;
    MethodSymbol* Class_getComponentType_method;
    MethodSymbol* Class_desiredAssertionStatus_method;

    MethodSymbol* AssertionError_Init_method;
    MethodSymbol* AssertionError_InitWithChar_method;
    MethodSymbol* AssertionError_InitWithBoolean_method;
    MethodSymbol* AssertionError_InitWithInt_method;
    MethodSymbol* AssertionError_InitWithLong_method;
    MethodSymbol* AssertionError_InitWithFloat_method;
    MethodSymbol* AssertionError_InitWithDouble_method;
    MethodSymbol* AssertionError_InitWithObject_method;

    MethodSymbol* Throwable_getMessage_method;
    MethodSymbol* Throwable_initCause_method;

    MethodSymbol* NoClassDefFoundError_InitString_method;
    MethodSymbol* NoClassDefFoundError_Init_method;

    MethodSymbol* StringBuffer_Init_method;
    MethodSymbol* StringBuffer_InitWithString_method;
    MethodSymbol* StringBuffer_toString_method;
    MethodSymbol* StringBuffer_append_char_method;
    MethodSymbol* StringBuffer_append_boolean_method;
    MethodSymbol* StringBuffer_append_int_method;
    MethodSymbol* StringBuffer_append_long_method;
    MethodSymbol* StringBuffer_append_float_method;
    MethodSymbol* StringBuffer_append_double_method;
    MethodSymbol* StringBuffer_append_string_method;
    MethodSymbol* StringBuffer_append_object_method;

    static int ConvertUnicodeToUtf8(const wchar_t*, char*);

    void ProcessGlobals();
    void ProcessUnnamedPackage();
    void ProcessPath();
    void ProcessBootClassPath();
    void ProcessExtDirs();
    void ProcessClassPath();
    void ProcessSourcePath();
    TypeSymbol* GetPrimitiveType(const wchar_t*, const char*);
    void ProcessSystemInformation();

    void ProcessFile(FileSymbol*);
    void ProcessMembers();
    void CollectTypes(TypeSymbol*, Tuple<TypeSymbol*>&);
    void ProcessBodies(TypeSymbol*);

    void ProcessNewInputFiles(SymbolSet&, char**);

    FileSymbol* FindOrInsertJavaInputFile(DirectorySymbol*, NameSymbol*);
    FileSymbol* FindOrInsertJavaInputFile(wchar_t*, int);
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // control_INCLUDED

