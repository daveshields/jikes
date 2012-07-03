// $Id: error.h,v 1.32 2000/01/06 08:24:30 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef error_INCLUDED
#define error_INCLUDED

#include "config.h"
#include "stream.h"
#include "tuple.h"

class Control;
class LexStream;
class Ast_CompilationUnit;
class SymbolSet;
class Semantic;

class SemanticError
{
public:
    enum SemanticErrorKind
    {
        BAD_ERROR,
        DEFAULT_ERROR,

        INVALID_OPTION,
        INVALID_K_OPTION,
        INVALID_K_TARGET,
        INVALID_TAB_VALUE,
        INVALID_DIRECTORY,
        UNSUPPORTED_ENCODING,
        UNSUPPORTED_OPTION,
        DISABLED_OPTION,

        NO_CURRENT_DIRECTORY,
        CANNOT_OPEN_ZIP_FILE,
        CANNOT_OPEN_PATH_DIRECTORY,
        PACKAGE_NOT_FOUND,
        CANNOT_OPEN_DIRECTORY,
        BAD_INPUT_FILE,
        UNREADABLE_INPUT_FILE,
        NON_STANDARD_LIBRARY_TYPE,
        LIBRARY_METHOD_NOT_FOUND,
        CANNOT_REOPEN_FILE,
        CANNOT_WRITE_FILE,
        CONSTANT_POOL_OVERFLOW,
        INTERFACES_OVERFLOW,
        METHODS_OVERFLOW,
        STRING_OVERFLOW,
        PARAMETER_OVERFLOW,
        ARRAY_OVERFLOW,
        FIELDS_OVERFLOW,
        LOCAL_VARIABLES_OVERFLOW,
        STACK_OVERFLOW,
        CODE_OVERFLOW,
        CANNOT_COMPUTE_COLUMNS,
        EMPTY_DECLARATION,
        REDUNDANT_ABSTRACT,
        REDUNDANT_FINAL,
        REDUNDANT_PUBLIC,
        REDUNDANT_STATIC,
        OBSOLESCENT_ABSTRACT,
        OBSOLESCENT_BRACKETS,
        NO_TYPES,
        MULTIPLE_PUBLIC_TYPES,
        TYPE_IN_MULTIPLE_FILES,
        PACKAGE_TYPE_CONFLICT,
        DIRECTORY_FILE_CONFLICT,
        FILE_FILE_CONFLICT,
        MISMATCHED_TYPE_AND_FILE_NAMES,
        REFERENCE_TO_TYPE_IN_MISMATCHED_FILE,
        DUPLICATE_INNER_TYPE_NAME,
        DUPLICATE_TYPE_DECLARATION,
        UNNECESSARY_TYPE_IMPORT,
        DUPLICATE_ACCESS_MODIFIER,
        DUPLICATE_MODIFIER,
        FINAL_ABSTRACT_CLASS,
        VOLATILE_FINAL,
        FINAL_VOLATILE,
        INVALID_TOP_LEVEL_CLASS_MODIFIER,
        INVALID_INNER_CLASS_MODIFIER,
        INVALID_STATIC_INNER_CLASS_MODIFIER,
        INVALID_LOCAL_CLASS_MODIFIER,
        INVALID_INTERFACE_MODIFIER,
        INVALID_FIELD_MODIFIER,
        INVALID_LOCAL_MODIFIER,
        INVALID_METHOD_MODIFIER,
        INVALID_SIGNATURE_MODIFIER,
        INVALID_CONSTRUCTOR_MODIFIER,
        INVALID_CONSTANT_MODIFIER,
        UNINITIALIZED_FIELD,
        PARENT_TYPE_IN_UNNAMED_PACKAGE,
        RECOMPILATION,
        TYPE_NOT_FOUND,
        DUPLICATE_ON_DEMAND_IMPORT,
        NOT_A_TYPE,
        NOT_A_CLASS,
        NOT_AN_INTERFACE,
        SUPER_IS_FINAL,
        OBJECT_WITH_SUPER_TYPE,
        OBJECT_HAS_NO_SUPER_TYPE,
        DUPLICATE_FIELD,
        DUPLICATE_METHOD,
        DUPLICATE_CONSTRUCTOR,
        MISMATCHED_INHERITED_METHOD,
        MISMATCHED_INHERITED_METHOD_EXTERNALLY,
        DUPLICATE_FORMAL_PARAMETER,
        MISMATCHED_CONSTRUCTOR_NAME,
        METHOD_WITH_CONSTRUCTOR_NAME,
        DUPLICATE_LOCAL_VARIABLE_DECLARATION,
        DUPLICATE_LOCAL_TYPE_DECLARATION,
        MULTIPLE_DEFAULT_LABEL,
        UNDECLARED_LABEL,
        DUPLICATE_LABEL,
        CATCH_PRIMITIVE_TYPE,
        CATCH_ARRAY_TYPE,
        AMBIGUOUS_FIELD,
        AMBIGUOUS_TYPE,
        FIELD_IS_TYPE,
        FIELD_NOT_FOUND,
        FIELD_NAME_MISSPELLED,
        FIELD_WITH_PRIVATE_ACCESS_NOT_ACCESSIBLE,
        FIELD_WITH_DEFAULT_ACCESS_NOT_ACCESSIBLE,
        NAME_NOT_FOUND,
        METHOD_NOT_FIELD,
        NAME_NOT_YET_AVAILABLE,
        NAME_NOT_VARIABLE,
        NAME_NOT_CLASS_VARIABLE,
        NOT_A_NUMERIC_VARIABLE,
        METHOD_NOT_FOUND,
        METHOD_NAME_NOT_FOUND_IN_TYPE,
        METHOD_NAME_MISSPELLED,
        METHOD_WITH_PRIVATE_ACCESS_NOT_ACCESSIBLE,
        METHOD_WITH_DEFAULT_ACCESS_NOT_ACCESSIBLE,
        HIDDEN_METHOD_IN_ENCLOSING_CLASS,
        FIELD_NOT_METHOD,
        TYPE_NOT_METHOD,
        TYPE_NOT_FIELD,
        METHOD_NOT_CLASS_METHOD,
        AMBIGUOUS_CONSTRUCTOR_INVOCATION,
        AMBIGUOUS_METHOD_INVOCATION,
        CONSTRUCTOR_NOT_FOUND,
        METHOD_FOUND_FOR_CONSTRUCTOR,
        ABSTRACT_TYPE_CREATION,
        INVALID_INSTANCEOF_CONVERSION,
        INVALID_CAST_CONVERSION,
        INVALID_CAST_TYPE,
        INCOMPATIBLE_TYPE_FOR_INITIALIZATION,
        INCOMPATIBLE_TYPE_FOR_ASSIGNMENT,
        INCOMPATIBLE_TYPE_FOR_BINARY_EXPRESSION,
        INCOMPATIBLE_TYPE_FOR_CONDITIONAL_EXPRESSION,
        VOID_ARRAY,
        VOID_TYPE_IN_EQUALITY_EXPRESSION,
        TYPE_NOT_THROWABLE,
        TYPE_NOT_PRIMITIVE,
        TYPE_NOT_INTEGRAL,
        TYPE_NOT_NUMERIC,
        TYPE_NOT_INTEGER,
        TYPE_NOT_BOOLEAN,
        TYPE_NOT_ARRAY,
        TYPE_NOT_REFERENCE,
        TYPE_NOT_VALID_FOR_SWITCH,
        TYPE_IS_VOID,
        VALUE_NOT_REPRESENTABLE_IN_SWITCH_TYPE,
        TYPE_NOT_CONVERTIBLE_TO_SWITCH_TYPE,
        DUPLICATE_CASE_VALUE,
        MISPLACED_THIS_EXPRESSION,
        MISPLACED_SUPER_EXPRESSION,
        TARGET_VARIABLE_IS_FINAL,
        FINAL_VARIABLE_TARGET_IN_LOOP,
        UNINITIALIZED_FINAL_VARIABLE,
        UNINITIALIZED_STATIC_FINAL_VARIABLE,
        UNINITIALIZED_FINAL_VARIABLE_IN_CONSTRUCTOR,
        INIT_SCALAR_WITH_ARRAY,
        INIT_ARRAY_WITH_SCALAR,
        INVALID_BYTE_VALUE,
        INVALID_SHORT_VALUE,
        INVALID_CHARACTER_VALUE,
        INVALID_INT_VALUE,
        INVALID_LONG_VALUE,
        INVALID_FLOAT_VALUE,
        INVALID_DOUBLE_VALUE,
        INVALID_STRING_VALUE,
        RETURN_STATEMENT_IN_INITIALIZER,
        MISPLACED_RETURN_WITH_EXPRESSION,
        MISPLACED_RETURN_WITH_NO_EXPRESSION,
        MISMATCHED_RETURN_AND_METHOD_TYPE,
        EXPRESSION_NOT_THROWABLE,
        BAD_THROWABLE_EXPRESSION_IN_TRY,
        BAD_THROWABLE_EXPRESSION_IN_METHOD,
        BAD_THROWABLE_EXPRESSION,
        MISPLACED_BREAK_STATEMENT,
        MISPLACED_CONTINUE_STATEMENT,
        MISPLACED_EXPLICIT_CONSTRUCTOR_INVOCATION,
        INVALID_CONTINUE_TARGET,
        NON_ABSTRACT_TYPE_CONTAINS_ABSTRACT_METHOD,
        NON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD,
        NON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD_FROM_ABSTRACT_CLASS,
        NON_ABSTRACT_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD,
        NO_ABSTRACT_METHOD_IMPLEMENTATION,
        DUPLICATE_INTERFACE,
        UNKNOWN_QUALIFIED_NAME_BASE,
        UNKNOWN_AMBIGUOUS_NAME,
        CIRCULAR_INTERFACE,
        CIRCULAR_CLASS,
        TYPE_NOT_ACCESSIBLE,
        PRIVATE_FIELD_NOT_ACCESSIBLE,
        PROTECTED_FIELD_NOT_ACCESSIBLE,
        DEFAULT_FIELD_NOT_ACCESSIBLE,
        PRIVATE_METHOD_NOT_ACCESSIBLE,
        PROTECTED_METHOD_NOT_ACCESSIBLE,
        DEFAULT_METHOD_NOT_ACCESSIBLE,
        PRIVATE_CONSTRUCTOR_NOT_ACCESSIBLE,
        PROTECTED_CONSTRUCTOR_NOT_ACCESSIBLE,
        DEFAULT_CONSTRUCTOR_NOT_ACCESSIBLE,
        CONSTRUCTOR_DOES_NOT_THROW_THIS_EXCEPTION,
        CONSTRUCTOR_DOES_NOT_THROW_SUPER_EXCEPTION,
        PARAMETER_REDECLARED,
        BAD_ABSTRACT_METHOD_MODIFIER,
        ABSTRACT_METHOD_MODIFIER_CONFLICT,
        ABSTRACT_METHOD_INVOCATION,
        FINAL_METHOD_OVERRIDE,
        FINAL_METHOD_OVERRIDE_EXTERNALLY,
        PRIVATE_METHOD_OVERRIDE,
        PRIVATE_METHOD_OVERRIDE_EXTERNALLY,
        INSTANCE_METHOD_OVERRIDE,
        INSTANCE_METHOD_OVERRIDE_EXTERNALLY,
        CLASS_METHOD_OVERRIDE,
        CLASS_METHOD_OVERRIDE_EXTERNALLY,
        MISMATCHED_OVERRIDDEN_EXCEPTION,
        MISMATCHED_OVERRIDDEN_EXCEPTION_EXTERNALLY,
        ABSTRACT_METHOD_WITH_BODY,
        NON_ABSTRACT_METHOD_WITHOUT_BODY,
        BAD_ACCESS_METHOD_OVERRIDE,
        BAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY,
        STATIC_OVERRIDE_ABSTRACT,
        STATIC_OVERRIDE_ABSTRACT_EXTERNALLY,
        CIRCULAR_THIS_CALL,
        INSTANCE_VARIABLE_IN_EXPLICIT_CONSTRUCTOR_INVOCATION,
        INSTANCE_METHOD_IN_EXPLICIT_CONSTRUCTOR_INVOCATION,
        SYNTHETIC_VARIABLE_ACCESS,
        SYNTHETIC_METHOD_INVOCATION,
        SYNTHETIC_CONSTRUCTOR_INVOCATION,
        THIS_IN_EXPLICIT_CONSTRUCTOR_INVOCATION,
        SUPER_IN_EXPLICIT_CONSTRUCTOR_INVOCATION,
        INNER_CONSTRUCTOR_IN_EXPLICIT_CONSTRUCTOR_INVOCATION,
        EXPRESSION_NOT_CONSTANT,
        UNCATCHABLE_METHOD_THROWN_CHECKED_EXCEPTION,
        UNCATCHABLE_CONSTRUCTOR_THROWN_CHECKED_EXCEPTION,
        UNREACHABLE_CATCH_CLAUSE,
        UNREACHABLE_DEFAULT_CATCH_CLAUSE,
        UNREACHABLE_STATEMENT,
        UNREACHABLE_STATEMENTS,
        UNREACHABLE_CONSTRUCTOR_BODY,
        BLOCKED_CATCH_CLAUSE,
        VARIABLE_NOT_DEFINITELY_ASSIGNED,
        TYPED_METHOD_WITH_NO_RETURN,

        DEFAULT_METHOD_NOT_OVERRIDDEN,

        ONE_UNNAMED_PACKAGE,
        TYPE_NOT_IN_UNNAMED_PACKAGE,
        TYPE_IN_WRONG_PACKAGE,
        TYPE_NAME_MISMATCH,

        DEPRECATED_TYPE,
        DEPRECATED_FIELD,
        DEPRECATED_METHOD,
        DEPRECATED_CONSTRUCTOR,

        COMPRESSED_ZIP_FILE,
        INVALID_CLASS_FILE,
        CANNOT_OPEN_CLASS_FILE,

        STATIC_NOT_INNER_CLASS,
        TYPE_NOT_INNER_CLASS,
        SUPER_TYPE_NOT_INNER_CLASS,
        STATIC_FIELD_IN_INNER_CLASS,
        STATIC_METHOD_IN_INNER_CLASS,
        STATIC_TYPE_IN_INNER_CLASS,
        STATIC_INITIALIZER_IN_INNER_CLASS,
        INNER_CLASS_REFERENCE_TO_NON_FINAL_LOCAL_VARIABLE,
        STATIC_PROTECTED_FIELD_ACCESS,
        STATIC_PROTECTED_METHOD_ACCESS,
        INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL,
        INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER,
        ILLEGAL_THIS_FIELD_ACCESS,
        CONSTRUCTOR_FOUND_IN_ANONYMOUS_CLASS,
        ENCLOSING_INSTANCE_ACCESS_FROM_CONSTRUCTOR_INVOCATION,
        ENCLOSING_INSTANCE_ACCESS_ACROSS_STATIC_REGION,
        ENCLOSING_INSTANCE_NOT_ACCESSIBLE,
        INVALID_ENCLOSING_INSTANCE,

        ZERO_DIVIDE_ERROR,
        ZERO_DIVIDE_CAUTION,
        VOID_TO_STRING,

        _num_kinds
    };

    static void StaticInitializer();

    void Report(SemanticErrorKind,
                LexStream::TokenIndex,
                LexStream::TokenIndex,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL,
                wchar_t * = NULL);

    SemanticError(Control &, FileSymbol *);
    ~SemanticError()
    {
        for (int i = 0; i < buffer.Length(); i++)
            delete [] buffer[i];
    }

    int num_errors,
        num_warnings;

    void EnteringClone() { clone_count++; }
    void ExitingClone()  { clone_count--; }

    int PrintMessages();

private:
    friend class Semantic;

    Control &control;
    LexStream *lex_stream;

    int clone_count;

    struct ErrorInfo
    {
        LexStream::TokenIndex left_token,
                              right_token;
        wchar_t *insert1,
                *insert2,
                *insert3,
                *insert4,
                *insert5,
                *insert6,
                *insert7,
                *insert8,
                *insert9;
        unsigned num;
        short    msg_code;
        short    right_string_length;
    };

    Tuple<wchar_t *> buffer;
    Tuple<ErrorInfo> error;

    void PrintLargeSource(int);
    void PrintSmallSource(int);

    void PrintEmacsMessage(int);

    static unsigned char warning[];
    static void (*print_message[_num_kinds])(ErrorInfo &, LexStream *, Control &);

    static bool NotDot(wchar_t *str) { return (! (wcslen(str) == 0 || wcscmp(str, StringConstant::US__DO) == 0)); }

    static void PrintBAD_ERROR(ErrorInfo &, LexStream *, Control &);
    static void PrintDEFAULT_ERROR(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_OPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_K_OPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_K_TARGET(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_TAB_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_DIRECTORY(ErrorInfo &, LexStream *, Control &);
    static void PrintUNSUPPORTED_ENCODING(ErrorInfo &, LexStream *, Control &);
    static void PrintUNSUPPORTED_OPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintDISABLED_OPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintNO_CURRENT_DIRECTORY(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_OPEN_ZIP_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_OPEN_PATH_DIRECTORY(ErrorInfo &, LexStream *, Control &);
    static void PrintPACKAGE_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_OPEN_DIRECTORY(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_INPUT_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNREADABLE_INPUT_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintNON_STANDARD_LIBRARY_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintLIBRARY_METHOD_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_REOPEN_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_WRITE_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintCONSTANT_POOL_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintINTERFACES_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHODS_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintSTRING_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintPARAMETER_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintARRAY_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELDS_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintLOCAL_VARIABLES_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintSTACK_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintCODE_OVERFLOW(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_COMPUTE_COLUMNS(ErrorInfo &, LexStream *, Control &);

    static void PrintEMPTY_DECLARATION(ErrorInfo &, LexStream *, Control &);
    static void PrintREDUNDANT_ABSTRACT(ErrorInfo &, LexStream *, Control &);
    static void PrintREDUNDANT_FINAL(ErrorInfo &, LexStream *, Control &);
    static void PrintREDUNDANT_PUBLIC(ErrorInfo &, LexStream *, Control &);
    static void PrintREDUNDANT_STATIC(ErrorInfo &, LexStream *, Control &);
    static void PrintOBSOLESCENT_ABSTRACT(ErrorInfo &, LexStream *, Control &);
    static void PrintOBSOLESCENT_BRACKETS(ErrorInfo &, LexStream *, Control &);
    static void PrintNO_TYPES(ErrorInfo &, LexStream *, Control &);
    static void PrintMULTIPLE_PUBLIC_TYPES(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_IN_MULTIPLE_FILES(ErrorInfo &, LexStream *, Control &);
    static void PrintPACKAGE_TYPE_CONFLICT(ErrorInfo &, LexStream *, Control &);
    static void PrintDIRECTORY_FILE_CONFLICT(ErrorInfo &, LexStream *, Control &);
    static void PrintFILE_FILE_CONFLICT(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_TYPE_AND_FILE_NAMES(ErrorInfo &, LexStream *, Control &);
    static void PrintREFERENCE_TO_TYPE_IN_MISMATCHED_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_INNER_TYPE_NAME(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_TYPE_DECLARATION(ErrorInfo &, LexStream *, Control &);
    static void PrintUNNECESSARY_TYPE_IMPORT(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_ACCESS_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintFINAL_ABSTRACT_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintVOLATILE_FINAL(ErrorInfo &, LexStream *, Control &);
    static void PrintFINAL_VOLATILE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_TOP_LEVEL_CLASS_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_INNER_CLASS_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_STATIC_INNER_CLASS_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_LOCAL_CLASS_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_INTERFACE_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_FIELD_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_LOCAL_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_METHOD_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_SIGNATURE_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CONSTRUCTOR_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CONSTANT_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintUNINITIALIZED_FIELD(ErrorInfo &, LexStream *, Control &);
    static void PrintPARENT_TYPE_IN_UNNAMED_PACKAGE(ErrorInfo &, LexStream *, Control &);
    static void PrintRECOMPILATION(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_ON_DEMAND_IMPORT(ErrorInfo &, LexStream *, Control &);
    static void PrintNOT_A_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintNOT_A_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintNOT_AN_INTERFACE(ErrorInfo &, LexStream *, Control &);
    static void PrintSUPER_IS_FINAL(ErrorInfo &, LexStream *, Control &);
    static void PrintOBJECT_WITH_SUPER_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintOBJECT_HAS_NO_SUPER_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_FIELD(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_CONSTRUCTOR(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_INHERITED_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_INHERITED_METHOD_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_FORMAL_PARAMETER(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_CONSTRUCTOR_NAME(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_WITH_CONSTRUCTOR_NAME(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_LOCAL_VARIABLE_DECLARATION(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_LOCAL_TYPE_DECLARATION(ErrorInfo &, LexStream *, Control &);
    static void PrintMULTIPLE_DEFAULT_LABEL(ErrorInfo &, LexStream *, Control &);
    static void PrintUNDECLARED_LABEL(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_LABEL(ErrorInfo &, LexStream *, Control &);
    static void PrintCATCH_PRIMITIVE_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintCATCH_ARRAY_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintAMBIGUOUS_FIELD(ErrorInfo &, LexStream *, Control &);
    static void PrintAMBIGUOUS_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELD_IS_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELD_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELD_NAME_MISSPELLED(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELD_WITH_PRIVATE_ACCESS_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELD_WITH_DEFAULT_ACCESS_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintNAME_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_NOT_FIELD(ErrorInfo &, LexStream *, Control &);
    static void PrintNAME_NOT_YET_AVAILABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintNAME_NOT_VARIABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintNAME_NOT_CLASS_VARIABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintNOT_A_NUMERIC_VARIABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_NAME_NOT_FOUND_IN_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_NAME_MISSPELLED(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_WITH_PRIVATE_ACCESS_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_WITH_DEFAULT_ACCESS_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintHIDDEN_METHOD_IN_ENCLOSING_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintFIELD_NOT_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_FIELD(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_NOT_CLASS_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintAMBIGUOUS_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintAMBIGUOUS_METHOD_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintCONSTRUCTOR_NOT_FOUND(ErrorInfo &, LexStream *, Control &);
    static void PrintMETHOD_FOUND_FOR_CONSTRUCTOR(ErrorInfo &, LexStream *, Control &);
    static void PrintABSTRACT_TYPE_CREATION(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_INSTANCEOF_CONVERSION(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CAST_CONVERSION(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CAST_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintINCOMPATIBLE_TYPE_FOR_INITIALIZATION(ErrorInfo &, LexStream *, Control &);
    static void PrintINCOMPATIBLE_TYPE_FOR_ASSIGNMENT(ErrorInfo &, LexStream *, Control &);
    static void PrintINCOMPATIBLE_TYPE_FOR_BINARY_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintINCOMPATIBLE_TYPE_FOR_CONDITIONAL_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintVOID_ARRAY(ErrorInfo &, LexStream *, Control &);
    static void PrintVOID_TYPE_IN_EQUALITY_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_THROWABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_PRIMITIVE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_INTEGRAL(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_NUMERIC(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_INTEGER(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_BOOLEAN(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_ARRAY(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_REFERENCE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_VALID_FOR_SWITCH(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_IS_VOID(ErrorInfo &, LexStream *, Control &);
    static void PrintVALUE_NOT_REPRESENTABLE_IN_SWITCH_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_CONVERTIBLE_TO_SWITCH_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_CASE_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_THIS_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_SUPER_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintTARGET_VARIABLE_IS_FINAL(ErrorInfo &, LexStream *, Control &);
    static void PrintFINAL_VARIABLE_TARGET_IN_LOOP(ErrorInfo &, LexStream *, Control &);
    static void PrintUNINITIALIZED_FINAL_VARIABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNINITIALIZED_STATIC_FINAL_VARIABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNINITIALIZED_FINAL_VARIABLE_IN_CONSTRUCTOR(ErrorInfo &, LexStream *, Control &);
    static void PrintINIT_SCALAR_WITH_ARRAY(ErrorInfo &, LexStream *, Control &);
    static void PrintINIT_ARRAY_WITH_SCALAR(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_BYTE_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_SHORT_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CHARACTER_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_INT_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_LONG_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_FLOAT_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_DOUBLE_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_STRING_VALUE(ErrorInfo &, LexStream *, Control &);
    static void PrintRETURN_STATEMENT_IN_INITIALIZER(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_RETURN_WITH_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_RETURN_WITH_NO_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_RETURN_AND_METHOD_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintEXPRESSION_NOT_THROWABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_THROWABLE_EXPRESSION_IN_TRY(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_THROWABLE_EXPRESSION_IN_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_THROWABLE_EXPRESSION(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_BREAK_STATEMENT(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_CONTINUE_STATEMENT(ErrorInfo &, LexStream *, Control &);
    static void PrintMISPLACED_EXPLICIT_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CONTINUE_TARGET(ErrorInfo &, LexStream *, Control &);
    static void PrintNON_ABSTRACT_TYPE_CONTAINS_ABSTRACT_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintNON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintNON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD_FROM_ABSTRACT_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintNON_ABSTRACT_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintNO_ABSTRACT_METHOD_IMPLEMENTATION(ErrorInfo &, LexStream *, Control &);
    static void PrintDUPLICATE_INTERFACE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNKNOWN_QUALIFIED_NAME_BASE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNKNOWN_AMBIGUOUS_NAME(ErrorInfo &, LexStream *, Control &);
    static void PrintCIRCULAR_INTERFACE(ErrorInfo &, LexStream *, Control &);
    static void PrintCIRCULAR_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintPRIVATE_FIELD_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintPROTECTED_FIELD_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintDEFAULT_FIELD_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintPRIVATE_METHOD_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintPROTECTED_METHOD_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintDEFAULT_METHOD_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintPRIVATE_CONSTRUCTOR_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintPROTECTED_CONSTRUCTOR_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintDEFAULT_CONSTRUCTOR_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintCONSTRUCTOR_DOES_NOT_THROW_THIS_EXCEPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintCONSTRUCTOR_DOES_NOT_THROW_SUPER_EXCEPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintPARAMETER_REDECLARED(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_ABSTRACT_METHOD_MODIFIER(ErrorInfo &, LexStream *, Control &);
    static void PrintABSTRACT_METHOD_MODIFIER_CONFLICT(ErrorInfo &, LexStream *, Control &);
    static void PrintABSTRACT_METHOD_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintFINAL_METHOD_OVERRIDE(ErrorInfo &, LexStream *, Control &);
    static void PrintFINAL_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintPRIVATE_METHOD_OVERRIDE(ErrorInfo &, LexStream *, Control &);
    static void PrintPRIVATE_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintINSTANCE_METHOD_OVERRIDE(ErrorInfo &, LexStream *, Control &);
    static void PrintINSTANCE_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintCLASS_METHOD_OVERRIDE(ErrorInfo &, LexStream *, Control &);
    static void PrintCLASS_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_OVERRIDDEN_EXCEPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintMISMATCHED_OVERRIDDEN_EXCEPTION_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintABSTRACT_METHOD_WITH_BODY(ErrorInfo &, LexStream *, Control &);
    static void PrintNON_ABSTRACT_METHOD_WITHOUT_BODY(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_ACCESS_METHOD_OVERRIDE(ErrorInfo &, LexStream *, Control &);
    static void PrintBAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_OVERRIDE_ABSTRACT(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_OVERRIDE_ABSTRACT_EXTERNALLY(ErrorInfo &, LexStream *, Control &);
    static void PrintCIRCULAR_THIS_CALL(ErrorInfo &, LexStream *, Control &);
    static void PrintINSTANCE_VARIABLE_IN_EXPLICIT_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintINSTANCE_METHOD_IN_EXPLICIT_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintSYNTHETIC_VARIABLE_ACCESS(ErrorInfo &, LexStream *, Control &);
    static void PrintSYNTHETIC_METHOD_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintSYNTHETIC_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintTHIS_IN_EXPLICIT_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintSUPER_IN_EXPLICIT_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintINNER_CONSTRUCTOR_IN_EXPLICIT_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintEXPRESSION_NOT_CONSTANT(ErrorInfo &, LexStream *, Control &);
    static void PrintUNCATCHABLE_METHOD_THROWN_CHECKED_EXCEPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintUNCATCHABLE_CONSTRUCTOR_THROWN_CHECKED_EXCEPTION(ErrorInfo &, LexStream *, Control &);
    static void PrintUNREACHABLE_CATCH_CLAUSE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNREACHABLE_DEFAULT_CATCH_CLAUSE(ErrorInfo &, LexStream *, Control &);
    static void PrintUNREACHABLE_STATEMENT(ErrorInfo &, LexStream *, Control &);
    static void PrintUNREACHABLE_STATEMENTS(ErrorInfo &, LexStream *, Control &);
    static void PrintUNREACHABLE_CONSTRUCTOR_BODY(ErrorInfo &, LexStream *, Control &);
    static void PrintBLOCKED_CATCH_CLAUSE(ErrorInfo &, LexStream *, Control &);
    static void PrintVARIABLE_NOT_DEFINITELY_ASSIGNED(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPED_METHOD_WITH_NO_RETURN(ErrorInfo &, LexStream *, Control &);

    static void PrintDEFAULT_METHOD_NOT_OVERRIDDEN(ErrorInfo &, LexStream *, Control &);

    static void PrintONE_UNNAMED_PACKAGE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_IN_UNNAMED_PACKAGE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_IN_WRONG_PACKAGE(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NAME_MISMATCH(ErrorInfo &, LexStream *, Control &);

    static void PrintDEPRECATED_TYPE(ErrorInfo &, LexStream *, Control &);
    static void PrintDEPRECATED_FIELD(ErrorInfo &, LexStream *, Control &);
    static void PrintDEPRECATED_METHOD(ErrorInfo &, LexStream *, Control &);
    static void PrintDEPRECATED_CONSTRUCTOR(ErrorInfo &, LexStream *, Control &);

    static void PrintCOMPRESSED_ZIP_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_CLASS_FILE(ErrorInfo &, LexStream *, Control &);
    static void PrintCANNOT_OPEN_CLASS_FILE(ErrorInfo &, LexStream *, Control &);

    static void PrintSTATIC_NOT_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintTYPE_NOT_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintSUPER_TYPE_NOT_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_FIELD_IN_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_METHOD_IN_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_TYPE_IN_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_INITIALIZER_IN_INNER_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintINNER_CLASS_REFERENCE_TO_NON_FINAL_LOCAL_VARIABLE(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_PROTECTED_FIELD_ACCESS(ErrorInfo &, LexStream *, Control &);
    static void PrintSTATIC_PROTECTED_METHOD_ACCESS(ErrorInfo &, LexStream *, Control &);
    static void PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL(ErrorInfo &, LexStream *, Control &);
    static void PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER(ErrorInfo &, LexStream *, Control &);
    static void PrintILLEGAL_THIS_FIELD_ACCESS(ErrorInfo &, LexStream *, Control &);
    static void PrintCONSTRUCTOR_FOUND_IN_ANONYMOUS_CLASS(ErrorInfo &, LexStream *, Control &);
    static void PrintENCLOSING_INSTANCE_ACCESS_FROM_CONSTRUCTOR_INVOCATION(ErrorInfo &, LexStream *, Control &);
    static void PrintENCLOSING_INSTANCE_ACCESS_ACROSS_STATIC_REGION(ErrorInfo &, LexStream *, Control &);
    static void PrintENCLOSING_INSTANCE_NOT_ACCESSIBLE(ErrorInfo &, LexStream *, Control &);
    static void PrintINVALID_ENCLOSING_INSTANCE(ErrorInfo &, LexStream *, Control &);
    static void PrintZERO_DIVIDE_ERROR(ErrorInfo &, LexStream *, Control &);
    static void PrintZERO_DIVIDE_CAUTION(ErrorInfo &, LexStream *, Control &);
    static void PrintVOID_TO_STRING(ErrorInfo &, LexStream *, Control &);

    void SortMessages();
};

#endif
