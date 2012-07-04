// $Id: control.h,v 1.36 2001/09/14 05:31:32 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef control_INCLUDED
#define control_INCLUDED

#include "platform.h"
#include "option.h"
#include "symbol.h"
#include "tuple.h"
#include "set.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class StoragePool;
class Scanner;
class Parser;
class Semantic;
class LexStream;
class AstPackageDeclaration;

class Control : public StringConstant
{
public:
    int return_code;
    Option &option;
    SymbolTable classpath_table,
                external_table;

    PackageSymbol *system_package,
                  *java_util_package,
                  *unnamed_package;
    int dot_classpath_index;
    Tuple<PathSymbol *> classpath;
    Tuple<wchar_t *> bad_dirnames,
                     bad_zip_filenames,
                     bad_input_filenames,
                     unreadable_input_filenames;

    SystemTable *system_table;
    Tuple<DirectorySymbol *> system_directories;

    Semantic *system_semantic;
    Tuple<Semantic *> semantic;
    Tuple<TypeSymbol *> needs_body_work,
                        type_trash_bin;

    NameSymbolMap unnamed_package_types;

    SymbolSet input_java_file_set,
              input_class_file_set,
              expired_file_set,
              recompilation_file_set;

    Parser *parser;
    Scanner *scanner;

    //
    //
    //
    LiteralLookupTable string_table,
                       int_table,
                       long_table,
                       char_table,
                       float_table,
                       double_table;
    NameLookupTable name_table;
    TypeLookupTable type_table;

    //
    //
    //
    NameSymbol *dot_name_symbol,
               *dot_dot_name_symbol,
               *length_name_symbol,
               *init_name_symbol,
               *clinit_name_symbol,
               *block_init_name_symbol,
               *this0_name_symbol,
               *clone_name_symbol,
               *object_name_symbol,
               *type_name_symbol,
               *class_name_symbol,
               *toString_name_symbol,
               *append_name_symbol,
               *forName_name_symbol,
               *getMessage_name_symbol;

    //
    //
    //
    TypeSymbol *byte_type,
               *short_type,
               *int_type,
               *long_type,
               *char_type,
               *float_type,
               *double_type,
               *boolean_type,
               *void_type,
               *null_type,
               *no_type;

    //
    TypeSymbol *GetType(PackageSymbol *, wchar_t *);

    inline TypeSymbol *Serializable()
    {
        if (! Serializable_type)
        {
            PackageSymbol *io_package = ProcessPackage(StringConstant::US_java_SL_io);
            FindPathsToDirectory(io_package);
            Serializable_type = GetType(io_package, StringConstant::US_Serializable);
        }

        return Serializable_type;
    }

    inline TypeSymbol *Object()
    {
        return (Object_type ? Object_type : Object_type = GetType(system_package, StringConstant::US_Object));
    }

    inline TypeSymbol *Cloneable()
    {
        return (Cloneable_type ? Cloneable_type : Cloneable_type = GetType(system_package, StringConstant::US_Cloneable));
    }

    inline TypeSymbol *String()
    {
        return (String_type ? String_type : String_type = GetType(system_package, StringConstant::US_String));
    }

    inline TypeSymbol *Void()
    {
        return (Void_type ? Void_type : Void_type = GetType(system_package, StringConstant::US_Void));
    }

    inline TypeSymbol *Boolean()
    {
        return (Boolean_type ? Boolean_type : Boolean_type = GetType(system_package, StringConstant::US_Boolean));
    }

    inline TypeSymbol *Byte()
    {
        return (Byte_type ? Byte_type : Byte_type = GetType(system_package, StringConstant::US_Byte));
    }

    inline TypeSymbol *Short()
    {
        return (Short_type ? Short_type : Short_type = GetType(system_package, StringConstant::US_Short));
    }

    inline TypeSymbol *Character()
    {
        return (Character_type ? Character_type : Character_type = GetType(system_package, StringConstant::US_Character));
    }

    inline TypeSymbol *Integer()
    {
        return (Integer_type ? Integer_type : Integer_type = GetType(system_package, StringConstant::US_Integer));
    }

    inline TypeSymbol *Long()
    {
        return (Long_type ? Long_type : Long_type = GetType(system_package, StringConstant::US_Long));
    }

    inline TypeSymbol *Float()
    {
        return (Float_type ? Float_type : Float_type = GetType(system_package, StringConstant::US_Float));
    }

    inline TypeSymbol *Double()
    {
        return (Double_type ? Double_type : Double_type = GetType(system_package, StringConstant::US_Double));
    }

    void InitClassInfo();
    inline TypeSymbol *Class()
    {
        if (! Class_type)
        {
            Class_type = GetType(system_package, StringConstant::US_Class);
            InitClassInfo();
        }

        return Class_type;
    }

    MethodSymbol *Class_forNameMethod()
    {
        if (! Class_forName_method)
            (void) Class();
        return Class_forName_method;
    }


    void InitThrowableInfo();
    inline TypeSymbol *Throwable()
    {
        if (! Throwable_type)
        {
            Throwable_type = GetType(system_package, StringConstant::US_Throwable);
            InitThrowableInfo();
        }

        return Throwable_type;
    }

    MethodSymbol *Throwable_getMessageMethod()
    {
        if (! Throwable_getMessage_method)
            (void) Throwable();
        return Throwable_getMessage_method;
    }

    inline TypeSymbol *Exception()
    {
        return (Exception_type
                       ? Exception_type
                       : Exception_type = GetType(system_package, StringConstant::US_Exception));
    }

    inline TypeSymbol *RuntimeException()
    {
        return (RuntimeException_type
                       ? RuntimeException_type
                       : RuntimeException_type = GetType(system_package, StringConstant::US_RuntimeException));
    }

    inline TypeSymbol *ClassNotFoundException()
    {
        return (ClassNotFoundException_type
                       ? ClassNotFoundException_type
                       : ClassNotFoundException_type = GetType(system_package, StringConstant::US_ClassNotFoundException));
    }

    inline TypeSymbol *Error()
    {
        return (Error_type ? Error_type : Error_type = GetType(system_package, StringConstant::US_Error));
    }

    void InitNoClassDefFoundErrorInfo();
    inline TypeSymbol *NoClassDefFoundError()
    {
        if (! NoClassDefFoundError_type)
        {
            NoClassDefFoundError_type = GetType(system_package, StringConstant::US_NoClassDefFoundError);
            InitNoClassDefFoundErrorInfo();
        }

        return NoClassDefFoundError_type;
    }

    MethodSymbol *NoClassDefFoundError_InitMethod()
    {
        if (! NoClassDefFoundError_InitWithString_method)
            (void) NoClassDefFoundError();
        return NoClassDefFoundError_InitWithString_method;
    }


    void InitStringBufferInfo();
    inline TypeSymbol *StringBuffer()
    {
        if (! StringBuffer_type)
        {
            StringBuffer_type = GetType(system_package, StringConstant::US_StringBuffer);
            InitStringBufferInfo();
        }

        return StringBuffer_type;
    }


    MethodSymbol *StringBuffer_InitMethod()
    {
        if (! StringBuffer_Init_method)
            (void) StringBuffer();
        return StringBuffer_Init_method;
    }


    MethodSymbol *StringBuffer_InitWithStringMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_InitWithString_method;
    }


    MethodSymbol *StringBuffer_toStringMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_toString_method;
    }


    MethodSymbol *StringBuffer_append_char_arrayMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_char_array_method;
    }


    MethodSymbol *StringBuffer_append_charMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_char_method;
    }


    MethodSymbol *StringBuffer_append_booleanMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_boolean_method;
    }


    MethodSymbol *StringBuffer_append_intMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_int_method;
    }


    MethodSymbol *StringBuffer_append_longMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_long_method;
    }


    MethodSymbol *StringBuffer_append_floatMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_float_method;
    }


    MethodSymbol *StringBuffer_append_doubleMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_double_method;
    }


    MethodSymbol *StringBuffer_append_stringMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_string_method;
    }


    MethodSymbol *StringBuffer_append_objectMethod()
    {
        if (! StringBuffer_InitWithString_method)
            (void) StringBuffer();
        return StringBuffer_append_object_method;
    }

    IntLiteralTable    int_pool;
    LongLiteralTable   long_pool;
    FloatLiteralTable  float_pool;
    DoubleLiteralTable double_pool;
    Utf8LiteralTable   Utf8_pool;

    Utf8LiteralValue *ConstantValue_literal,
                     *Exceptions_literal,
                     *InnerClasses_literal,
                     *Synthetic_literal,
                     *Deprecated_literal,
                     *LineNumberTable_literal,
                     *LocalVariableTable_literal,
                     *Code_literal,
                     *Sourcefile_literal,

                     *null_literal,
                     *this_literal;

    Control(char **, Option &);
    ~Control();

    Utf8LiteralValue *ConvertUnicodeToUtf8(wchar_t *source)
    {
        char *target = new char[wcslen(source) * 3 + 1]; // should be big enough for the worst case
        int length = ConvertUnicodeToUtf8(source, target);
        Utf8LiteralValue *literal = Utf8_pool.FindOrInsert(target, length);
        delete [] target;

        return literal;
    }

    static int ConvertUtf8ToUnicode(wchar_t *, const char *, int);

    NameSymbol *ConvertUtf8ToUnicode(const char *source, int length)
    {
        wchar_t *name = new wchar_t[length + 1];
        int name_length = ConvertUtf8ToUnicode(name, source, length);
        NameSymbol *name_symbol = FindOrInsertName(name, name_length);
        delete [] name;

        return name_symbol;
    }

    void FindPathsToDirectory(PackageSymbol *);

    DirectoryEntry *FindInputFile(FileSymbol *);
    void FindMoreRecentInputFiles(SymbolSet &);
    void RemoveTrashedTypes(SymbolSet &);
    void RereadDirectory(DirectorySymbol *);
    void RereadDirectories();
    void ComputeRecompilationSet(TypeDependenceChecker &);
    bool IncrementalRecompilation();

    //
    // The one and only bad value constant.
    //
    LiteralValue *BadValue() { return &bad_value; }

    //
    // Note that only names are converted here and not literals, since
    // no error can occur in a name.
    // A literal is converted during the semantic pass so that an
    // accurate diagnostic can be issued in case it is invalid.
    //
    NameSymbol *FindOrInsertName(wchar_t *name, int len)
    {
        NameSymbol *name_symbol = name_table.FindOrInsertName(name, len);
        if (! name_symbol -> Utf8_literal)
            name_symbol -> Utf8_literal = ConvertUnicodeToUtf8(name_symbol -> Name());

        return name_symbol;
    }

    //
    // Make up a parameter name of the form #(num) and return its name symbol.
    //
    NameSymbol *MakeParameter(int num)
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
    static DirectorySymbol *GetOutputDirectory(FileSymbol *);
    static FileSymbol *GetJavaFile(PackageSymbol *, NameSymbol *);
    static FileSymbol *GetFile(Control &, PackageSymbol *, NameSymbol *);
    static FileSymbol *GetFileFirst(Control &, PackageSymbol *, NameSymbol *);
    static FileSymbol *GetFileBoth(Control &, PackageSymbol *, NameSymbol *);

    PackageSymbol *FindOrInsertPackage(LexStream *, AstExpression *);
    void ProcessPackageDeclaration(FileSymbol *, AstPackageDeclaration *);
    void CleanUp(FileSymbol *);

    inline bool IsSimpleIntegerValueType(TypeSymbol *type)
    {
        return (type == byte_type || type == short_type || type == int_type || type == char_type);
    }

    inline bool IsIntegral(TypeSymbol *type)
    {
        return (IsSimpleIntegerValueType(type) || type == long_type);
    }

    inline bool IsFloatingPoint(TypeSymbol *type)
    {
        return (type == float_type || type == double_type);
    }

    inline bool IsNumeric(TypeSymbol *type)
    {
        return IsIntegral(type) || IsFloatingPoint(type);
    }

    inline bool IsDoubleWordType(TypeSymbol *type)
    {
        return (type == long_type || type == double_type);
    }

    inline bool IsPrimitive(TypeSymbol *type)
    {
        return (IsNumeric(type) || type == boolean_type);
    }

    inline void ProcessBadType(TypeSymbol *type_symbol)
    {
        type_trash_bin.Next() = type_symbol;
    }

    void ProcessHeaders(FileSymbol *);

#ifdef JIKES_DEBUG
    int input_files_processed,
        class_files_read,
        class_files_written,
        line_count;
#endif

    PackageSymbol *ProcessPackage(wchar_t *);

    DirectorySymbol *FindSubdirectory(PathSymbol *, wchar_t *, int);
    DirectorySymbol *ProcessSubdirectories(wchar_t *, int, bool);

private:

    LiteralValue bad_value;

    TypeSymbol *Serializable_type,

               *Object_type,
               *Cloneable_type,
               *String_type,
               *Void_type,
               *Boolean_type,
               *Byte_type,
               *Short_type,
               *Character_type,
               *Integer_type,
               *Long_type,
               *Float_type,
               *Double_type,
               *Class_type,
               *Throwable_type,
               *Exception_type,
               *RuntimeException_type,
               *ClassNotFoundException_type,
               *Error_type,
               *NoClassDefFoundError_type,
               *StringBuffer_type;

    MethodSymbol *Class_forName_method,

                 *Throwable_getMessage_method,

                 *NoClassDefFoundError_InitWithString_method,

                 *StringBuffer_Init_method,
                 *StringBuffer_InitWithString_method,
                 *StringBuffer_toString_method,
                 *StringBuffer_append_char_array_method,
                 *StringBuffer_append_char_method,
                 *StringBuffer_append_boolean_method,
                 *StringBuffer_append_int_method,
                 *StringBuffer_append_long_method,
                 *StringBuffer_append_float_method,
                 *StringBuffer_append_double_method,
                 *StringBuffer_append_string_method,
                 *StringBuffer_append_object_method;

    static int ConvertUnicodeToUtf8(wchar_t *, char *);

    void ProcessGlobals();
    void ProcessUnnamedPackage();
    void ProcessPath();
    void ProcessBootClassPath();
    void ProcessExtDirs();
    void ProcessClassPath();
    void ProcessSourcePath();
    TypeSymbol *GetPrimitiveType(wchar_t *, char *);
    void ProcessSystemInformation();

    void ProcessFile(FileSymbol *);
    void ProcessMembers();
    void ProcessBodies(TypeSymbol *);

    void ProcessNewInputFiles(SymbolSet &, char **, int = 0);

    FileSymbol *FindOrInsertJavaInputFile(DirectorySymbol *, NameSymbol *);
    FileSymbol *FindOrInsertJavaInputFile(wchar_t *, int);
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // control_INCLUDED

