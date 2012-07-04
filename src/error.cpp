// $Id: error.cpp,v 1.104 2002/09/09 22:52:56 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "error.h"
#include "control.h"
#include "semantic.h"
#include "ast.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

unsigned char SemanticError::warning[SemanticError::_num_kinds] = { 0 };
wchar_t * (*SemanticError::print_message[SemanticError::_num_kinds]) (ErrorInfo &, LexStream *, Control &) = { NULL };

void ErrorInfo::Initialize(LexStream *l, wchar_t *m, JikesErrorSeverity s)
{
    lex_stream = l;

    left_line_no = lex_stream -> Line(left_token);
    left_column_no = lex_stream -> Column(left_token);
    right_line_no = lex_stream -> Line(right_token);
    right_column_no = lex_stream -> RightColumn(right_token);

    msg = m;
    severity = s;
}

ErrorInfo::ErrorInfo() : msg(NULL),
                         severity(JikesError::JIKES_ERROR)
{
}

ErrorInfo::~ErrorInfo()
{
    delete [] msg;
}


JikesError::JikesErrorSeverity ErrorInfo::getSeverity() { return severity; }
int ErrorInfo::getLeftLineNo() { return left_line_no; }
int ErrorInfo::getLeftColumnNo() { return left_column_no; }
int ErrorInfo::getRightLineNo() { return right_line_no; }
int ErrorInfo::getRightColumnNo() { return right_column_no; }

const char *ErrorInfo::getFileName()
{
    assert(lex_stream);
    return lex_stream -> FileName();
}

const wchar_t *ErrorInfo::getErrorMessage()
{
    assert(msg);
    return msg;
}

bool ErrorInfo::emacs_style_report = false;

const wchar_t *ErrorInfo::getErrorReport()
{
    return emacs_style_report ? emacsErrorString() : regularErrorString();
}

wchar_t *ErrorInfo::regularErrorString()
{
    ErrorString s;

    lex_stream -> OutputSource(this, s);

    s << endl << "*** " << getSeverityString() << ": "
      << getErrorMessage();

    return s.Array();
}


wchar_t *ErrorInfo::emacsErrorString()
{
    ErrorString s;

    s << getFileName()
      << ':' << left_line_no  << ':' << left_column_no
      << ':' << right_line_no << ':' << right_column_no
      << ": " << getSeverityString() << ": "
      << getErrorMessage();

    return s.Array();
}


SemanticError::SemanticError(Control &control_,
                             FileSymbol *file_symbol)
    : num_errors(0),
      num_warnings(0),
      control(control_),
      lex_stream(file_symbol -> lex_stream),
      clone_count(0),
      buffer(1024),
      error(512)
{
    ErrorInfo::emacs_style_report = ! control.option.errors;
}

//
// This procedure is invoked by an JIKES PARSER or a semantic
// routine to process an error message.  The JIKES parser always
// passes the value 0 to msg_level to indicate an error.
// This routine simply stores all necessary information about
// the message into an array: error.
//
void SemanticError::Report(SemanticErrorKind msg_code,
                           LexStream::TokenIndex left_token,
                           LexStream::TokenIndex right_token,
                           wchar_t *insert1,
                           wchar_t *insert2,
                           wchar_t *insert3,
                           wchar_t *insert4,
                           wchar_t *insert5,
                           wchar_t *insert6,
                           wchar_t *insert7,
                           wchar_t *insert8,
                           wchar_t *insert9)
{
    //
    // Do not report errors detected while processing a clone !!!
    // If we have a warning and the nowarn option is set, ignore it.
    //
    if (clone_count > 0 ||
        (control.option.nowarn && (warning[msg_code] == 1 ||
                                   (warning[msg_code] == 2 &&
                                    (! control.option.zero_defect)))))
        return;

    int i = error.NextIndex();

    if (warning[msg_code] > 0)
         num_warnings++;
    else num_errors++;

    error[i].msg_code = msg_code;

    int total_length = 0,
        length1 = 0,
        length2 = 0,
        length3 = 0,
        length4 = 0,
        length5 = 0,
        length6 = 0,
        length7 = 0,
        length8 = 0,
        length9 = 0;

    if (insert1)
    {
        length1 = wcslen(insert1);
        total_length += (length1 + 1);
    }
    else error[i].insert1 = NULL;

    if (insert2)
    {
        length2 = wcslen(insert2);
        total_length += (length2 + 1);
    }
    else error[i].insert2 = NULL;

    if (insert3)
    {
        length3 = wcslen(insert3);
        total_length += (length3 + 1);
    }
    else error[i].insert3 = NULL;

    if (insert4)
    {
        length4 = wcslen(insert4);
        total_length += (length4 + 1);
    }
    else error[i].insert4 = NULL;

    if (insert5)
    {
        length5 = wcslen(insert5);
        total_length += (length5 + 1);
    }
    else error[i].insert5 = NULL;

    if (insert6)
    {
        length6 = wcslen(insert6);
        total_length += (length6 + 1);
    }
    else error[i].insert6 = NULL;

    if (insert7)
    {
        length7 = wcslen(insert7);
        total_length += (length7 + 1);
    }
    else error[i].insert7 = NULL;

    if (insert8)
    {
        length8 = wcslen(insert8);
        total_length += (length8 + 1);
    }
    else error[i].insert8 = NULL;

    if (insert9)
    {
        length9 = wcslen(insert9);
        total_length += (length9 + 1);
    }
    else error[i].insert9 = NULL;

    if (total_length > 0)
    {
        wchar_t *ptr = new wchar_t[total_length];
        buffer.Next() = ptr;

        if (insert1)
        {
            memmove(ptr, insert1, length1 * sizeof(wchar_t));
            error[i].insert1 = ptr;
            ptr += length1;
            *ptr++ = U_NULL;
        }

        if (insert2)
        {
            memmove(ptr, insert2, length2 * sizeof(wchar_t));
            error[i].insert2 = ptr;
            ptr += length2;
            *ptr++ = U_NULL;
        }

        if (insert3)
        {
            memmove(ptr, insert3, length3 * sizeof(wchar_t));
            error[i].insert3 = ptr;
            ptr += length3;
            *ptr++ = U_NULL;
        }

        if (insert4)
        {
            memmove(ptr, insert4, length4 * sizeof(wchar_t));
            error[i].insert4 = ptr;
            ptr += length4;
            *ptr++ = U_NULL;
        }

        if (insert5)
        {
            memmove(ptr, insert5, length5 * sizeof(wchar_t));
            error[i].insert5 = ptr;
            ptr += length5;
            *ptr++ = U_NULL;
        }

        if (insert6)
        {
            memmove(ptr, insert6, length6 * sizeof(wchar_t));
            error[i].insert6 = ptr;
            ptr += length6;
            *ptr++ = U_NULL;
        }

        if (insert7)
        {
            memmove(ptr, insert7, length7 * sizeof(wchar_t));
            error[i].insert7 = ptr;
            ptr += length7;
            *ptr++ = U_NULL;
        }

        if (insert8)
        {
            memmove(ptr, insert8, length8 * sizeof(wchar_t));
            error[i].insert8 = ptr;
            ptr += length8;
            *ptr++ = U_NULL;
        }

        if (insert9)
        {
            memmove(ptr, insert9, length9 * sizeof(wchar_t));
            error[i].insert9 = ptr;
            ptr += length9;
            *ptr++ = U_NULL;
        }
    }

    error[i].num = i;
    error[i].left_token = (left_token > right_token ? right_token : left_token);
    error[i].right_token = right_token;

    //
    // Dump the error immediately ?
    //
    if (control.option.dump_errors)
    {
        reportError(i);

        if (buffer.Length() > 0)
        {
            delete [] buffer[0];
            buffer.Reset();
        }
        // we need at least 1 error in order for the return code to be
        // set properly. See print_messages
        error.Reset(1);
    }
}

void SemanticError::StaticInitializer()
{
    memset(warning, 0, _num_kinds * sizeof(unsigned char));

    warning[CANNOT_OPEN_ZIP_FILE] = 1;
    warning[CANNOT_OPEN_PATH_DIRECTORY] = 1;

    warning[EMPTY_DECLARATION] = 1;
    warning[REDUNDANT_MODIFIER] = 1;
    warning[RECOMMENDED_MODIFIER_ORDER] = 1;
    warning[OBSOLESCENT_BRACKETS] = 1;
    warning[DUPLICATE_THROWS_CLAUSE_CLASS] = 1;
    warning[REDUNDANT_THROWS_CLAUSE_CLASS] = 1;
    warning[UNCHECKED_THROWS_CLAUSE_CLASS] = 1;
    warning[NO_TYPES] = 1;

    warning[DEPRECATED_TYPE] = 1;
    warning[DEPRECATED_FIELD] = 1;
    warning[DEPRECATED_METHOD] = 1;
    warning[DEPRECATED_CONSTRUCTOR] = 1;

    warning[UNNECESSARY_TYPE_IMPORT] = 1;
    warning[MULTIPLE_PUBLIC_TYPES] = 1;
    warning[TYPE_IN_MULTIPLE_FILES] = 1;
    warning[MISMATCHED_TYPE_AND_FILE_NAMES] = 1;
    warning[REFERENCE_TO_TYPE_IN_MISMATCHED_FILE] = 1;
    warning[ONE_UNNAMED_PACKAGE] = 1;
    warning[RECOMPILATION] = 1;
    warning[METHOD_WITH_CONSTRUCTOR_NAME] = 1;
    warning[DEFAULT_METHOD_NOT_OVERRIDDEN] = 1;
    warning[INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL] = 1;
    warning[INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER] = 1;

    //
    // Something stronger than a warning, but code will be generated anyway
    //
    warning[BAD_INPUT_FILE] = 2;
    warning[UNREADABLE_INPUT_FILE] = 2;
    warning[NEGATIVE_ARRAY_SIZE] = 2;
    warning[UNNECESSARY_PARENTHESIS] = 2;
    warning[ZERO_DIVIDE_CAUTION] = 2;
    warning[UNIMPLEMENTABLE_INTERFACE] = 2;
    warning[UNIMPLEMENTABLE_CLASS] = 2;
    warning[INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_TYPE] = 2;

#ifdef JIKES_DEBUG
    for (int i = 0; i < _num_kinds; i++)
        print_message[i] = NULL;
#endif

    print_message[BAD_ERROR] = PrintBAD_ERROR;
    print_message[DEFAULT_ERROR] = PrintDEFAULT_ERROR;
    print_message[NO_CURRENT_DIRECTORY] = PrintNO_CURRENT_DIRECTORY;
    print_message[CANNOT_OPEN_ZIP_FILE] = PrintCANNOT_OPEN_ZIP_FILE;
    print_message[CANNOT_OPEN_PATH_DIRECTORY] = PrintCANNOT_OPEN_PATH_DIRECTORY;
    print_message[PACKAGE_NOT_FOUND] = PrintPACKAGE_NOT_FOUND;
    print_message[CANNOT_OPEN_DIRECTORY] = PrintCANNOT_OPEN_DIRECTORY;
    print_message[BAD_INPUT_FILE] = PrintBAD_INPUT_FILE;
    print_message[UNREADABLE_INPUT_FILE] = PrintUNREADABLE_INPUT_FILE;
    print_message[NON_STANDARD_LIBRARY_TYPE] = PrintNON_STANDARD_LIBRARY_TYPE;
    print_message[LIBRARY_METHOD_NOT_FOUND] = PrintLIBRARY_METHOD_NOT_FOUND;
    print_message[CANNOT_REOPEN_FILE] = PrintCANNOT_REOPEN_FILE;
    print_message[CANNOT_WRITE_FILE] = PrintCANNOT_WRITE_FILE;
    print_message[CONSTANT_POOL_OVERFLOW] = PrintCONSTANT_POOL_OVERFLOW;
    print_message[INTERFACES_OVERFLOW] = PrintINTERFACES_OVERFLOW;
    print_message[METHODS_OVERFLOW] = PrintMETHODS_OVERFLOW;
    print_message[STRING_OVERFLOW] = PrintSTRING_OVERFLOW;
    print_message[PARAMETER_OVERFLOW] = PrintPARAMETER_OVERFLOW;
    print_message[ARRAY_OVERFLOW] = PrintARRAY_OVERFLOW;
    print_message[FIELDS_OVERFLOW] = PrintFIELDS_OVERFLOW;
    print_message[LOCAL_VARIABLES_OVERFLOW] = PrintLOCAL_VARIABLES_OVERFLOW;
    print_message[STACK_OVERFLOW] = PrintSTACK_OVERFLOW;
    print_message[CODE_OVERFLOW] = PrintCODE_OVERFLOW;
    print_message[NEGATIVE_ARRAY_SIZE] = PrintNEGATIVE_ARRAY_SIZE;
    print_message[UNNECESSARY_PARENTHESIS] = PrintUNNECESSARY_PARENTHESIS;
    print_message[EMPTY_DECLARATION] = PrintEMPTY_DECLARATION;
    print_message[REDUNDANT_MODIFIER] = PrintREDUNDANT_MODIFIER;
    print_message[RECOMMENDED_MODIFIER_ORDER] = PrintRECOMMENDED_MODIFIER_ORDER;
    print_message[OBSOLESCENT_BRACKETS] = PrintOBSOLESCENT_BRACKETS;
    print_message[NO_TYPES] = PrintNO_TYPES;
    print_message[MULTIPLE_PUBLIC_TYPES] = PrintMULTIPLE_PUBLIC_TYPES;
    print_message[TYPE_IN_MULTIPLE_FILES] = PrintTYPE_IN_MULTIPLE_FILES;
    print_message[PACKAGE_TYPE_CONFLICT] = PrintPACKAGE_TYPE_CONFLICT;
    print_message[DIRECTORY_FILE_CONFLICT] = PrintDIRECTORY_FILE_CONFLICT;
    print_message[FILE_FILE_CONFLICT] = PrintFILE_FILE_CONFLICT;
    print_message[MISMATCHED_TYPE_AND_FILE_NAMES] =
        PrintMISMATCHED_TYPE_AND_FILE_NAMES;
    print_message[REFERENCE_TO_TYPE_IN_MISMATCHED_FILE] =
        PrintREFERENCE_TO_TYPE_IN_MISMATCHED_FILE;
    print_message[DUPLICATE_INNER_TYPE_NAME] = PrintDUPLICATE_INNER_TYPE_NAME;
    print_message[DUPLICATE_TYPE_DECLARATION] = PrintDUPLICATE_TYPE_DECLARATION;
    print_message[DUPLICATE_IMPORT_NAME] = PrintDUPLICATE_IMPORT_NAME;
    print_message[UNNECESSARY_TYPE_IMPORT] = PrintUNNECESSARY_TYPE_IMPORT;
    print_message[DUPLICATE_ACCESS_MODIFIER] = PrintDUPLICATE_ACCESS_MODIFIER;
    print_message[DUPLICATE_MODIFIER] = PrintDUPLICATE_MODIFIER;
    print_message[FINAL_ABSTRACT_CLASS] = PrintFINAL_ABSTRACT_CLASS;
    print_message[VOLATILE_FINAL_FIELD] = PrintVOLATILE_FINAL_FIELD;
    print_message[INVALID_TOP_LEVEL_CLASS_MODIFIER] =
        PrintINVALID_TOP_LEVEL_CLASS_MODIFIER;
    print_message[INVALID_INNER_CLASS_MODIFIER] =
        PrintINVALID_INNER_CLASS_MODIFIER;
    print_message[INVALID_STATIC_INNER_CLASS_MODIFIER] =
        PrintINVALID_STATIC_INNER_CLASS_MODIFIER;
    print_message[INVALID_LOCAL_CLASS_MODIFIER] =
        PrintINVALID_LOCAL_CLASS_MODIFIER;
    print_message[INVALID_INTERFACE_MODIFIER] = PrintINVALID_INTERFACE_MODIFIER;
    print_message[INVALID_FIELD_MODIFIER] = PrintINVALID_FIELD_MODIFIER;
    print_message[INVALID_LOCAL_MODIFIER] = PrintINVALID_LOCAL_MODIFIER;
    print_message[INVALID_METHOD_MODIFIER] = PrintINVALID_METHOD_MODIFIER;
    print_message[INVALID_SIGNATURE_MODIFIER] = PrintINVALID_SIGNATURE_MODIFIER;
    print_message[INVALID_CONSTRUCTOR_MODIFIER] =
        PrintINVALID_CONSTRUCTOR_MODIFIER;
    print_message[INVALID_CONSTANT_MODIFIER] = PrintINVALID_CONSTANT_MODIFIER;
    print_message[UNINITIALIZED_FIELD] = PrintUNINITIALIZED_FIELD;
    print_message[RECOMPILATION] = PrintRECOMPILATION;
    print_message[PACKAGE_NOT_TYPE] = PrintPACKAGE_NOT_TYPE;
    print_message[TYPE_NOT_FOUND] = PrintTYPE_NOT_FOUND;
    print_message[IMPORT_FROM_UNNAMED_PACKAGE] =
        PrintIMPORT_FROM_UNNAMED_PACKAGE;
    print_message[DUPLICATE_ON_DEMAND_IMPORT] = PrintDUPLICATE_ON_DEMAND_IMPORT;
    print_message[UNKNOWN_ON_DEMAND_IMPORT] = PrintUNKNOWN_ON_DEMAND_IMPORT;
    print_message[IMPORT_NOT_CANONICAL] = PrintIMPORT_NOT_CANONICAL;
    print_message[NOT_A_TYPE] = PrintNOT_A_TYPE;
    print_message[NOT_A_CLASS] = PrintNOT_A_CLASS;
    print_message[NOT_AN_INTERFACE] = PrintNOT_AN_INTERFACE;
    print_message[SUPER_IS_FINAL] = PrintSUPER_IS_FINAL;
    print_message[OBJECT_WITH_SUPER_TYPE] = PrintOBJECT_WITH_SUPER_TYPE;
    print_message[OBJECT_HAS_NO_SUPER_TYPE] = PrintOBJECT_HAS_NO_SUPER_TYPE;
    print_message[DUPLICATE_FIELD] = PrintDUPLICATE_FIELD;
    print_message[DUPLICATE_METHOD] = PrintDUPLICATE_METHOD;
    print_message[DUPLICATE_CONSTRUCTOR] = PrintDUPLICATE_CONSTRUCTOR;
    print_message[MISMATCHED_INHERITED_METHOD] =
        PrintMISMATCHED_INHERITED_METHOD;
    print_message[MISMATCHED_IMPLICIT_METHOD] = PrintMISMATCHED_IMPLICIT_METHOD;
    print_message[UNIMPLEMENTABLE_INTERFACE] = PrintUNIMPLEMENTABLE_INTERFACE;
    print_message[UNIMPLEMENTABLE_CLASS] = PrintUNIMPLEMENTABLE_CLASS;
    print_message[MISMATCHED_INHERITED_METHOD_EXTERNALLY] =
        PrintMISMATCHED_INHERITED_METHOD_EXTERNALLY;
    print_message[DUPLICATE_FORMAL_PARAMETER] = PrintDUPLICATE_FORMAL_PARAMETER;
    print_message[MISSPELLED_CONSTRUCTOR_NAME] =
        PrintMISSPELLED_CONSTRUCTOR_NAME;
    print_message[MISMATCHED_CONSTRUCTOR_NAME] =
        PrintMISMATCHED_CONSTRUCTOR_NAME;
    print_message[METHOD_WITH_CONSTRUCTOR_NAME] =
        PrintMETHOD_WITH_CONSTRUCTOR_NAME;
    print_message[DUPLICATE_LOCAL_VARIABLE_DECLARATION] =
        PrintDUPLICATE_LOCAL_VARIABLE_DECLARATION;
    print_message[DUPLICATE_LOCAL_TYPE_DECLARATION] =
        PrintDUPLICATE_LOCAL_TYPE_DECLARATION;
    print_message[MULTIPLE_DEFAULT_LABEL] = PrintMULTIPLE_DEFAULT_LABEL;
    print_message[UNDECLARED_LABEL] = PrintUNDECLARED_LABEL;
    print_message[DUPLICATE_LABEL] = PrintDUPLICATE_LABEL;
    print_message[CATCH_PRIMITIVE_TYPE] = PrintCATCH_PRIMITIVE_TYPE;
    print_message[CATCH_ARRAY_TYPE] = PrintCATCH_ARRAY_TYPE;
    print_message[AMBIGUOUS_FIELD] = PrintAMBIGUOUS_FIELD;
    print_message[AMBIGUOUS_TYPE] = PrintAMBIGUOUS_TYPE;
    print_message[FIELD_IS_TYPE] = PrintFIELD_IS_TYPE;
    print_message[FIELD_NOT_FOUND] = PrintFIELD_NOT_FOUND;
    print_message[FIELD_NAME_MISSPELLED] = PrintFIELD_NAME_MISSPELLED;
    print_message[METHOD_NOT_FIELD] = PrintMETHOD_NOT_FIELD;
    print_message[NAME_NOT_YET_AVAILABLE] = PrintNAME_NOT_YET_AVAILABLE;
    print_message[NAME_NOT_CLASS_VARIABLE] = PrintNAME_NOT_CLASS_VARIABLE;
    print_message[NOT_A_VARIABLE] = PrintNOT_A_VARIABLE;
    print_message[NOT_A_NUMERIC_VARIABLE] = PrintNOT_A_NUMERIC_VARIABLE;
    print_message[METHOD_OVERLOAD_NOT_FOUND] = PrintMETHOD_OVERLOAD_NOT_FOUND;
    print_message[METHOD_NOT_FOUND] = PrintMETHOD_NOT_FOUND;
    print_message[METHOD_NAME_MISSPELLED] = PrintMETHOD_NAME_MISSPELLED;
    print_message[HIDDEN_METHOD_IN_ENCLOSING_CLASS] =
        PrintHIDDEN_METHOD_IN_ENCLOSING_CLASS;
    print_message[FIELD_NOT_METHOD] = PrintFIELD_NOT_METHOD;
    print_message[TYPE_NOT_METHOD] = PrintTYPE_NOT_METHOD;
    print_message[TYPE_NOT_FIELD] = PrintTYPE_NOT_FIELD;
    print_message[METHOD_NOT_CLASS_METHOD] = PrintMETHOD_NOT_CLASS_METHOD;
    print_message[AMBIGUOUS_CONSTRUCTOR_INVOCATION] =
        PrintAMBIGUOUS_CONSTRUCTOR_INVOCATION;
    print_message[AMBIGUOUS_METHOD_INVOCATION] =
        PrintAMBIGUOUS_METHOD_INVOCATION;
    print_message[CONSTRUCTOR_NOT_FOUND] = PrintCONSTRUCTOR_NOT_FOUND;
    print_message[METHOD_FOUND_FOR_CONSTRUCTOR] =
        PrintMETHOD_FOUND_FOR_CONSTRUCTOR;
    print_message[CONSTRUCTOR_OVERLOAD_NOT_FOUND] =
        PrintCONSTRUCTOR_OVERLOAD_NOT_FOUND;
    print_message[ABSTRACT_TYPE_CREATION] = PrintABSTRACT_TYPE_CREATION;
    print_message[INVALID_INSTANCEOF_CONVERSION] =
        PrintINVALID_INSTANCEOF_CONVERSION;
    print_message[INVALID_CAST_CONVERSION] = PrintINVALID_CAST_CONVERSION;
    print_message[INVALID_CAST_TYPE] = PrintINVALID_CAST_TYPE;
    print_message[INCOMPATIBLE_TYPE_FOR_INITIALIZATION] =
        PrintINCOMPATIBLE_TYPE_FOR_INITIALIZATION;
    print_message[INCOMPATIBLE_TYPE_FOR_ASSIGNMENT] =
        PrintINCOMPATIBLE_TYPE_FOR_ASSIGNMENT;
    print_message[INCOMPATIBLE_TYPE_FOR_BINARY_EXPRESSION] =
        PrintINCOMPATIBLE_TYPE_FOR_BINARY_EXPRESSION;
    print_message[INCOMPATIBLE_TYPE_FOR_CONDITIONAL_EXPRESSION] =
        PrintINCOMPATIBLE_TYPE_FOR_CONDITIONAL_EXPRESSION;
    print_message[VOID_ARRAY] = PrintVOID_ARRAY;
    print_message[DUPLICATE_THROWS_CLAUSE_CLASS] =
        PrintDUPLICATE_THROWS_CLAUSE_CLASS;
    print_message[REDUNDANT_THROWS_CLAUSE_CLASS] =
        PrintREDUNDANT_THROWS_CLAUSE_CLASS;
    print_message[UNCHECKED_THROWS_CLAUSE_CLASS] =
        PrintUNCHECKED_THROWS_CLAUSE_CLASS;
    print_message[TYPE_NOT_THROWABLE] = PrintTYPE_NOT_THROWABLE;
    print_message[TYPE_NOT_INTEGRAL] = PrintTYPE_NOT_INTEGRAL;
    print_message[TYPE_NOT_NUMERIC] = PrintTYPE_NOT_NUMERIC;
    print_message[TYPE_NOT_INTEGER] = PrintTYPE_NOT_INTEGER;
    print_message[TYPE_NOT_BOOLEAN] = PrintTYPE_NOT_BOOLEAN;
    print_message[TYPE_NOT_ARRAY] = PrintTYPE_NOT_ARRAY;
    print_message[TYPE_NOT_REFERENCE] = PrintTYPE_NOT_REFERENCE;
    print_message[TYPE_IS_VOID] = PrintTYPE_IS_VOID;
    print_message[VALUE_NOT_REPRESENTABLE_IN_SWITCH_TYPE] =
        PrintVALUE_NOT_REPRESENTABLE_IN_SWITCH_TYPE;
    print_message[DUPLICATE_CASE_VALUE] = PrintDUPLICATE_CASE_VALUE;
    print_message[MISPLACED_THIS_EXPRESSION] = PrintMISPLACED_THIS_EXPRESSION;
    print_message[MISPLACED_SUPER_EXPRESSION] = PrintMISPLACED_SUPER_EXPRESSION;
    print_message[VARIABLE_NOT_DEFINITELY_UNASSIGNED] =
        PrintVARIABLE_NOT_DEFINITELY_UNASSIGNED;
    print_message[VARIABLE_NOT_DEFINITELY_UNASSIGNED_IN_LOOP] =
        PrintVARIABLE_NOT_DEFINITELY_UNASSIGNED_IN_LOOP;
    print_message[FINAL_VARIABLE_NOT_BLANK] = PrintFINAL_VARIABLE_NOT_BLANK;
    print_message[FINAL_FIELD_ASSIGNMENT_NOT_SIMPLE] =
        PrintFINAL_FIELD_ASSIGNMENT_NOT_SIMPLE;
    print_message[UNINITIALIZED_FINAL_VARIABLE] =
        PrintUNINITIALIZED_FINAL_VARIABLE;
    print_message[UNINITIALIZED_STATIC_FINAL_VARIABLE] =
        PrintUNINITIALIZED_STATIC_FINAL_VARIABLE;
    print_message[UNINITIALIZED_FINAL_VARIABLE_IN_CONSTRUCTOR] =
        PrintUNINITIALIZED_FINAL_VARIABLE_IN_CONSTRUCTOR;
    print_message[UNINITIALIZED_FINAL_VARIABLE_IN_INTERFACE] =
        PrintUNINITIALIZED_FINAL_VARIABLE_IN_INTERFACE;
    print_message[INIT_SCALAR_WITH_ARRAY] = PrintINIT_SCALAR_WITH_ARRAY;
    print_message[INIT_ARRAY_WITH_SCALAR] = PrintINIT_ARRAY_WITH_SCALAR;
    print_message[INVALID_BYTE_VALUE] = PrintINVALID_BYTE_VALUE;
    print_message[INVALID_SHORT_VALUE] = PrintINVALID_SHORT_VALUE;
    print_message[INVALID_CHARACTER_VALUE] = PrintINVALID_CHARACTER_VALUE;
    print_message[INVALID_INT_VALUE] = PrintINVALID_INT_VALUE;
    print_message[INVALID_LONG_VALUE] = PrintINVALID_LONG_VALUE;
    print_message[INVALID_FLOAT_VALUE] = PrintINVALID_FLOAT_VALUE;
    print_message[INVALID_DOUBLE_VALUE] = PrintINVALID_DOUBLE_VALUE;
    print_message[RETURN_STATEMENT_IN_INITIALIZER] =
        PrintRETURN_STATEMENT_IN_INITIALIZER;
    print_message[ABRUPT_INITIALIZER] = PrintABRUPT_INITIALIZER;
    print_message[MISPLACED_RETURN_WITH_EXPRESSION] =
        PrintMISPLACED_RETURN_WITH_EXPRESSION;
    print_message[MISPLACED_RETURN_WITH_NO_EXPRESSION] =
        PrintMISPLACED_RETURN_WITH_NO_EXPRESSION;
    print_message[MISMATCHED_RETURN_AND_METHOD_TYPE] =
        PrintMISMATCHED_RETURN_AND_METHOD_TYPE;
    print_message[EXPRESSION_NOT_THROWABLE] = PrintEXPRESSION_NOT_THROWABLE;
    print_message[MISPLACED_BREAK_STATEMENT] = PrintMISPLACED_BREAK_STATEMENT;
    print_message[MISPLACED_CONTINUE_STATEMENT] =
        PrintMISPLACED_CONTINUE_STATEMENT;
    print_message[MISPLACED_EXPLICIT_CONSTRUCTOR] =
        PrintMISPLACED_EXPLICIT_CONSTRUCTOR;
    print_message[INVALID_CONTINUE_TARGET] = PrintINVALID_CONTINUE_TARGET;
    print_message[NON_ABSTRACT_TYPE_CONTAINS_ABSTRACT_METHOD] =
        PrintNON_ABSTRACT_TYPE_CONTAINS_ABSTRACT_METHOD;
    print_message[NON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD] =
        PrintNON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD;
    print_message[NON_ABSTRACT_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD] =
        PrintNON_ABSTRACT_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD;
    print_message[ANONYMOUS_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD] =
        PrintANONYMOUS_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD;
    print_message[DUPLICATE_INTERFACE] = PrintDUPLICATE_INTERFACE;
    print_message[UNKNOWN_AMBIGUOUS_NAME] = PrintUNKNOWN_AMBIGUOUS_NAME;
    print_message[CIRCULAR_INTERFACE] = PrintCIRCULAR_INTERFACE;
    print_message[CIRCULAR_CLASS] = PrintCIRCULAR_CLASS;
    print_message[TYPE_NOT_ACCESSIBLE] = PrintTYPE_NOT_ACCESSIBLE;
    print_message[FIELD_NOT_ACCESSIBLE] = PrintFIELD_NOT_ACCESSIBLE;
    print_message[PROTECTED_INSTANCE_FIELD_NOT_ACCESSIBLE] =
        PrintPROTECTED_INSTANCE_FIELD_NOT_ACCESSIBLE;
    print_message[METHOD_NOT_ACCESSIBLE] = PrintMETHOD_NOT_ACCESSIBLE;
    print_message[PROTECTED_INSTANCE_METHOD_NOT_ACCESSIBLE] =
        PrintPROTECTED_INSTANCE_METHOD_NOT_ACCESSIBLE;
    print_message[PROTECTED_INTERFACE_METHOD_NOT_ACCESSIBLE] =
        PrintPROTECTED_INTERFACE_METHOD_NOT_ACCESSIBLE;
    print_message[CONSTRUCTOR_NOT_ACCESSIBLE] = PrintCONSTRUCTOR_NOT_ACCESSIBLE;
    print_message[PARAMETER_REDECLARED] = PrintPARAMETER_REDECLARED;
    print_message[BAD_ABSTRACT_METHOD_MODIFIER] =
        PrintBAD_ABSTRACT_METHOD_MODIFIER;
    print_message[ABSTRACT_METHOD_MODIFIER_CONFLICT] =
        PrintABSTRACT_METHOD_MODIFIER_CONFLICT;
    print_message[STRICTFP_NATIVE_METHOD] = PrintSTRICTFP_NATIVE_METHOD;
    print_message[ABSTRACT_METHOD_INVOCATION] = PrintABSTRACT_METHOD_INVOCATION;
    print_message[FINAL_METHOD_OVERRIDE] = PrintFINAL_METHOD_OVERRIDE;
    print_message[FINAL_IMPLICIT_METHOD_OVERRIDE] =
        PrintFINAL_IMPLICIT_METHOD_OVERRIDE;
    print_message[INSTANCE_METHOD_OVERRIDE] = PrintINSTANCE_METHOD_OVERRIDE;
    print_message[INSTANCE_METHOD_OVERRIDE_EXTERNALLY] =
        PrintINSTANCE_METHOD_OVERRIDE_EXTERNALLY;
    print_message[CLASS_METHOD_OVERRIDE] = PrintCLASS_METHOD_OVERRIDE;
    print_message[MISMATCHED_OVERRIDDEN_EXCEPTION] =
        PrintMISMATCHED_OVERRIDDEN_EXCEPTION;
    print_message[MISMATCHED_IMPLICIT_OVERRIDDEN_EXCEPTION] =
        PrintMISMATCHED_IMPLICIT_OVERRIDDEN_EXCEPTION;
    print_message[MISMATCHED_OVERRIDDEN_EXCEPTION_EXTERNALLY] =
        PrintMISMATCHED_OVERRIDDEN_EXCEPTION_EXTERNALLY;
    print_message[ABSTRACT_METHOD_WITH_BODY] = PrintABSTRACT_METHOD_WITH_BODY;
    print_message[NON_ABSTRACT_METHOD_WITHOUT_BODY] =
        PrintNON_ABSTRACT_METHOD_WITHOUT_BODY;
    print_message[BAD_ACCESS_METHOD_OVERRIDE] = PrintBAD_ACCESS_METHOD_OVERRIDE;
    print_message[BAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY] =
        PrintBAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY;
    print_message[CIRCULAR_THIS_CALL] = PrintCIRCULAR_THIS_CALL;
    print_message[INSTANCE_VARIABLE_IN_EXPLICIT_CONSTRUCTOR] =
        PrintINSTANCE_VARIABLE_IN_EXPLICIT_CONSTRUCTOR;
    print_message[INSTANCE_METHOD_IN_EXPLICIT_CONSTRUCTOR] =
        PrintINSTANCE_METHOD_IN_EXPLICIT_CONSTRUCTOR;
    print_message[SYNTHETIC_VARIABLE_ACCESS] = PrintSYNTHETIC_VARIABLE_ACCESS;
    print_message[SYNTHETIC_METHOD_INVOCATION] =
        PrintSYNTHETIC_METHOD_INVOCATION;
    print_message[SYNTHETIC_CONSTRUCTOR_INVOCATION] =
        PrintSYNTHETIC_CONSTRUCTOR_INVOCATION;
    print_message[SELF_IN_EXPLICIT_CONSTRUCTOR] =
        PrintSELF_IN_EXPLICIT_CONSTRUCTOR;
    print_message[INNER_CONSTRUCTOR_IN_EXPLICIT_CONSTRUCTOR] =
        PrintINNER_CONSTRUCTOR_IN_EXPLICIT_CONSTRUCTOR;
    print_message[EXPRESSION_NOT_CONSTANT] = PrintEXPRESSION_NOT_CONSTANT;
    print_message[UNCAUGHT_METHOD_EXCEPTION] = PrintUNCAUGHT_METHOD_EXCEPTION;
    print_message[UNCAUGHT_CONSTRUCTOR_EXCEPTION] =
        PrintUNCAUGHT_CONSTRUCTOR_EXCEPTION;
    print_message[UNCAUGHT_ANONYMOUS_CONSTRUCTOR_EXCEPTION] =
        PrintUNCAUGHT_ANONYMOUS_CONSTRUCTOR_EXCEPTION;
    print_message[UNCAUGHT_THROWN_EXCEPTION] = PrintUNCAUGHT_THROWN_EXCEPTION;
    print_message[UNCAUGHT_EXPLICIT_THIS_EXCEPTION] =
        PrintUNCAUGHT_EXPLICIT_THIS_EXCEPTION;
    print_message[UNCAUGHT_EXPLICIT_SUPER_EXCEPTION] =
        PrintUNCAUGHT_EXPLICIT_SUPER_EXCEPTION;
    print_message[UNREACHABLE_CATCH_CLAUSE] = PrintUNREACHABLE_CATCH_CLAUSE;
    print_message[UNREACHABLE_STATEMENT] = PrintUNREACHABLE_STATEMENT;
    print_message[UNREACHABLE_STATEMENTS] = PrintUNREACHABLE_STATEMENTS;
    print_message[BLOCKED_CATCH_CLAUSE] = PrintBLOCKED_CATCH_CLAUSE;
    print_message[VARIABLE_NOT_DEFINITELY_ASSIGNED] =
        PrintVARIABLE_NOT_DEFINITELY_ASSIGNED;
    print_message[TYPED_METHOD_WITH_NO_RETURN] =
        PrintTYPED_METHOD_WITH_NO_RETURN;

    print_message[DEFAULT_METHOD_NOT_OVERRIDDEN] =
        PrintDEFAULT_METHOD_NOT_OVERRIDDEN;

    print_message[ONE_UNNAMED_PACKAGE] = PrintONE_UNNAMED_PACKAGE;
    print_message[TYPE_NOT_IN_UNNAMED_PACKAGE] =
        PrintTYPE_NOT_IN_UNNAMED_PACKAGE;
    print_message[TYPE_IN_WRONG_PACKAGE] = PrintTYPE_IN_WRONG_PACKAGE;
    print_message[TYPE_NAME_MISMATCH] = PrintTYPE_NAME_MISMATCH;

    print_message[DEPRECATED_TYPE] = PrintDEPRECATED_TYPE;
    print_message[DEPRECATED_FIELD] = PrintDEPRECATED_FIELD;
    print_message[DEPRECATED_METHOD] = PrintDEPRECATED_METHOD;
    print_message[DEPRECATED_CONSTRUCTOR] = PrintDEPRECATED_CONSTRUCTOR;

    print_message[COMPRESSED_ZIP_FILE] = PrintCOMPRESSED_ZIP_FILE;
    print_message[INVALID_CLASS_FILE] = PrintINVALID_CLASS_FILE;
    print_message[CANNOT_OPEN_CLASS_FILE] = PrintCANNOT_OPEN_CLASS_FILE;

    print_message[INTERFACE_NOT_INNER_CLASS] = PrintINTERFACE_NOT_INNER_CLASS;
    print_message[STATIC_NOT_INNER_CLASS] = PrintSTATIC_NOT_INNER_CLASS;
    print_message[TYPE_NOT_INNER_CLASS] = PrintTYPE_NOT_INNER_CLASS;
    print_message[SUPER_TYPE_NOT_INNER_CLASS] = PrintSUPER_TYPE_NOT_INNER_CLASS;
    print_message[STATIC_FIELD_IN_INNER_CLASS_NOT_FINAL] =
        PrintSTATIC_FIELD_IN_INNER_CLASS_NOT_FINAL;
    print_message[STATIC_FIELD_IN_INNER_CLASS_NOT_CONSTANT] =
        PrintSTATIC_FIELD_IN_INNER_CLASS_NOT_CONSTANT;
    print_message[STATIC_METHOD_IN_INNER_CLASS] =
        PrintSTATIC_METHOD_IN_INNER_CLASS;
    print_message[STATIC_TYPE_IN_INNER_CLASS] = PrintSTATIC_TYPE_IN_INNER_CLASS;
    print_message[STATIC_INITIALIZER_IN_INNER_CLASS] =
        PrintSTATIC_INITIALIZER_IN_INNER_CLASS;
    print_message[INNER_CLASS_REFERENCE_TO_NON_FINAL_LOCAL_VARIABLE] =
        PrintINNER_CLASS_REFERENCE_TO_NON_FINAL_LOCAL_VARIABLE;
    print_message[INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL] =
        PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL;
    print_message[INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER] =
        PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER;
    print_message[INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_TYPE] =
        PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_TYPE;
    print_message[ILLEGAL_THIS_FIELD_ACCESS] = PrintILLEGAL_THIS_FIELD_ACCESS;
    print_message[CONSTRUCTOR_FOUND_IN_ANONYMOUS_CLASS] =
        PrintCONSTRUCTOR_FOUND_IN_ANONYMOUS_CLASS;
    print_message[ENCLOSING_INSTANCE_ACCESS_FROM_CONSTRUCTOR_INVOCATION] =
        PrintENCLOSING_INSTANCE_ACCESS_FROM_CONSTRUCTOR_INVOCATION;
    print_message[ENCLOSING_INSTANCE_ACCESS_ACROSS_STATIC_REGION] =
        PrintENCLOSING_INSTANCE_ACCESS_ACROSS_STATIC_REGION;
    print_message[ENCLOSING_INSTANCE_NOT_ACCESSIBLE] =
        PrintENCLOSING_INSTANCE_NOT_ACCESSIBLE;
    print_message[INVALID_ENCLOSING_INSTANCE] = PrintINVALID_ENCLOSING_INSTANCE;
    print_message[ZERO_DIVIDE_CAUTION] = PrintZERO_DIVIDE_CAUTION;
    print_message[VOID_TO_STRING] = PrintVOID_TO_STRING;

#ifdef JIKES_DEBUG
    //
    // Make sure that there is a message associated with each code
    //
    for (int k = 0; k < _num_kinds; k++)
        assert(print_message[k] != NULL);
#endif
}


//
// This procedure uses a  quick sort algorithm to sort the ERRORS
// by the left_line_no and left_column_no fields.
//
void SemanticError::SortMessages()
{
     int lower,
         upper,
         lostack[32],
         histack[32];

     int top,
         i,
         j;
     ErrorInfo pivot,
               temp;

     top = 0;
     lostack[top] = 0;
     histack[top] = error.Length() - 1;

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
             pivot = error[i];
             error[i] = error[lower];

             //
             // Split the array section indicated by LOWER and UPPER
             // using ARRAY(LOWER) as the pivot.
             //
             i = lower;
             for (j = lower + 1; j <= upper; j++)
                 if ((error[j].left_token < pivot.left_token) ||
                 //
                 // When two error messages start in the same location
                 // and one is nested inside the other, the outer one
                 // is placed first so that it can be printed last.
                 // Recall that its right-span location is reached
                 // after the inner one has been completely processed.
                 //
                     (error[j].left_token == pivot.left_token &&
                      error[j].right_token > pivot.right_token) ||
                 //
                 // When two error messages are at the same location
                 // span, check the NUM field to keep the sort stable.
                 // When the location spans only a single symbol,
                 // the one with the lowest "num" is placed first.
                 //
                     (error[j].left_token  == pivot.left_token  &&
                      error[j].right_token == pivot.right_token &&
                      pivot.left_token == pivot.right_token     &&
                      error[j].num < pivot.num)                       ||
                 //
                 // When two error messages are at the same location
                 // which spans more than one symbol in the source,
                 // the first message is treated as being nested into
                 // the second message and (just like the nested case
                 // above) it is placed last in the sorted sequence.
                 //
                     (error[j].left_token  == pivot.left_token  &&
                      error[j].right_token == pivot.right_token &&
                      pivot.left_token < pivot.right_token      &&
                      error[j].num > pivot.num))
                 {
                     temp = error[++i];
                     error[i] = error[j];
                     error[j] = temp;
                 }
             error[lower] = error[i];
             error[i] = pivot;

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
}


//
// This is the local private procedure that prints the semantic error messages.
//
int SemanticError::PrintMessages()
{
    int return_code = (num_errors > 0 ? 1 : 0);

    //
    // If the errors have not yet been dumped,...
    //
    if (control.option.errors) // regular error messages
    {
        if (num_errors == 0)
        {
            if (control.option.nowarn)
                // we only had warnings and they should not be reported
                return return_code;

            Coutput << endl << "Issued " << num_warnings
                    << (lex_stream -> file_symbol -> semantic ==
                        control.system_semantic ? " system" : " semantic")
                    << " warning" << (num_warnings <= 1 ? "" : "s");
        }
        else // we had some errors, and possibly warnings as well
        {
            Coutput << endl << "Found " << num_errors
                    << (lex_stream -> file_symbol -> semantic ==
                        control.system_semantic ? " system" : " semantic")
                    << " error" << (num_errors <= 1 ? "" : "s");
            if (num_warnings > 0 && !control.option.nowarn)
            {
                Coutput << " and issued " << num_warnings
                        << " warning" << (num_warnings <= 1 ? "" : "s");
            }
        }

        if (lex_stream -> file_symbol -> semantic !=
            control.system_semantic)
        {
            Coutput << " compiling \"" << lex_stream -> FileName() << '\"';
        }
        Coutput << ':';
    }

    //
    // Reopen the file to report the errors, unless we didn't parse it in the
    // first place.
    //
    if (lex_stream -> file_symbol -> semantic != control.system_semantic)
    {
        lex_stream -> RereadInput();

        if (! lex_stream -> InputBuffer())
        {
            char *file_name = lex_stream -> FileName();
            int length = lex_stream -> FileNameLength();
            wchar_t *name = new wchar_t[length + 1];
            for (int i = 0; i < length; i++)
                name[i] = file_name[i];
            name[length] = U_NULL;
            control.system_semantic ->
                ReportSemError(SemanticError::CANNOT_REOPEN_FILE, 0, 0, name);
            delete [] name;
        }
    }

    if (lex_stream -> file_symbol -> semantic == control.system_semantic ||
        lex_stream -> InputBuffer())
    {
        SortMessages();
        for (int k = 0; k < error.Length(); k++)
        {
            if (warning[error[k].msg_code] != 1 || ! control.option.nowarn)
                reportError(k);
        }
        lex_stream -> DestroyInput();
    }

    Coutput.flush();
    return return_code;
}

void SemanticError::reportError(int k)
{
    error[k].Initialize(lex_stream,
                        (print_message[error[k].msg_code]) (error[k], lex_stream, control),
                        (warning[error[k].msg_code] == 1
                         ? ErrorInfo::JIKES_WARNING
                         : ((warning[error[k].msg_code] == 2 &&
                             ! control.option.zero_defect)
                            ? ErrorInfo::JIKES_CAUTION
                            : ErrorInfo::JIKES_ERROR))
    );

    JikesAPI::getInstance() -> reportError(&error[k]);
}


//
// These "print_" procedures are invoked to print specific
// error messages. The parameter err identifies the error to
// be processed.
//
wchar_t *SemanticError::PrintBAD_ERROR(ErrorInfo &err,
                                       LexStream *lex_stream,
                                       Control &control)
{
    ErrorString s;
    s << "chaos: Error code " << err.msg_code
      << " is not a valid error message code.";
    return s.Array();
}


wchar_t *SemanticError::PrintDEFAULT_ERROR(ErrorInfo &err,
                                           LexStream *lex_stream,
                                           Control &control)
{
    ErrorString s;

    if (err.insert1)
        s << err.insert1;
    if (err.insert2)
        s << err.insert2;
    if (err.insert3)
        s << err.insert3;
    if (err.insert4)
        s << err.insert4;
    if (err.insert5)
        s << err.insert5;
    if (err.insert6)
        s << err.insert6;
    if (err.insert7)
        s << err.insert7;
    if (err.insert8)
        s << err.insert8;
    if (err.insert9)
        s << err.insert9;

    return s.Array();
}


wchar_t *SemanticError::PrintNO_CURRENT_DIRECTORY(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "Could not open current directory.";

    return s.Array();
}


wchar_t *SemanticError::PrintCANNOT_OPEN_ZIP_FILE(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The file \"" << err.insert1
      << "\" does not exist or else is not a valid zip file.";

    return s.Array();
}


wchar_t *SemanticError::PrintCANNOT_OPEN_PATH_DIRECTORY(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The file \"" << err.insert1 << "\" is not a valid directory.";

    return s.Array();
}


wchar_t *SemanticError::PrintPACKAGE_NOT_FOUND(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "You need to modify your classpath, sourcepath, bootclasspath, "
      << "and/or extdirs setup. Package \""
      << err.insert1 << "\" could not be found in:" << endl;

    for (int i = 1; i < control.classpath.Length(); i++)
    {
        PathSymbol *path_symbol = control.classpath[i];
        wchar_t *path = path_symbol -> Name();
        s << "                " << path << endl;
    }

    return s.Array();
}


wchar_t *SemanticError::PrintCANNOT_OPEN_DIRECTORY(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "Unable to open directory \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintBAD_INPUT_FILE(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "The input file \"" << err.insert1
      << "\" does not have the \".java\" extension.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNREADABLE_INPUT_FILE(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The input file \"" << err.insert1 << "\" was not found.";

    return s.Array();
}


wchar_t *SemanticError::PrintNON_STANDARD_LIBRARY_TYPE(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "A non-standard version of the type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\" was found. Class files that depend on this type may not "
      << "have been generated.";

    return s.Array();
}


wchar_t *SemanticError::PrintLIBRARY_METHOD_NOT_FOUND(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "A class file was not generated for the type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\" because a library method that it depends on was not found. "
      << "See system messages for more information.";

    return s.Array();
}


wchar_t *SemanticError::PrintCANNOT_REOPEN_FILE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "Unable to reopen file \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintCANNOT_WRITE_FILE(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "Unable to write file \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintCONSTANT_POOL_OVERFLOW(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\" produced a constant pool that exceeded the limit of 65535 "
      << "elements.";

    return s.Array();
}


wchar_t *SemanticError::PrintINTERFACES_OVERFLOW(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\" directly implemented more than limit of 65535 interfaces.";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHODS_OVERFLOW(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" declared more than the limit of 65535 methods.";

    return s.Array();
}


wchar_t *SemanticError::PrintSTRING_OVERFLOW(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\" generated one or more strings whose length exceeds the maximum "
      << "length of 65535 bytes when encoded in Utf8. Lengthy identifiers, "
      << "method signatures, and string literals can all cause this problem.";

    return s.Array();
}


wchar_t *SemanticError::PrintPARAMETER_OVERFLOW(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "Method \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3
      << "\" contained more than the limit of 255 formal parameters. Note "
      << "that a parameter of type long or double counts as 2 parameters.";

    return s.Array();
}


wchar_t *SemanticError::PrintARRAY_OVERFLOW(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "The number of dimensions in an array is limited to 255.";

    return s.Array();
}


wchar_t *SemanticError::PrintFIELDS_OVERFLOW(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" declared more than the limit of 65535 fields.";

    return s.Array();
}


wchar_t *SemanticError::PrintLOCAL_VARIABLES_OVERFLOW(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "Method \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3
      << "\" contained more than the limit of 65535 local variables.";

    return s.Array();
}


wchar_t *SemanticError::PrintSTACK_OVERFLOW(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "Method \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3
      << "\" required a stack that exceeds the limit of 65535 positions.";

    return s.Array();
}


wchar_t *SemanticError::PrintCODE_OVERFLOW(ErrorInfo &err,
                                           LexStream *lex_stream,
                                           Control &control)
{
    ErrorString s;

    s << "Method \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" produced a code attribute that exceeds the "
      << "code limit of 65535 elements.";

    return s.Array();
}


wchar_t *SemanticError::PrintNEGATIVE_ARRAY_SIZE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "Array initialization will fail with a negative dimension.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNNECESSARY_PARENTHESIS(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "Parenthesis surrounding a variable are syntactically unnecessary. "
      << "While legal now, they were illegal in previous versions of Java.";

    return s.Array();
}


wchar_t *SemanticError::PrintEMPTY_DECLARATION(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "An EmptyDeclaration is a deprecated feature that should not be used "
      << "- \";\" ignored.";

    return s.Array();
}


wchar_t *SemanticError::PrintREDUNDANT_MODIFIER(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The use of the \"" << err.insert1 << "\" modifier in this context "
      << "is redundant and strongly discouraged as a matter of style.";

    return s.Array();
}


wchar_t *SemanticError::PrintRECOMMENDED_MODIFIER_ORDER(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "While it is legal to list modifiers in any order, it is recommended "
      << "as a matter of style to list \"" << err.insert1 << "\" before \""
      << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintOBSOLESCENT_BRACKETS(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The use of empty bracket pairs following a MethodDeclarator should "
      << "not be used in new Java programs.";

    return s.Array();
}


wchar_t *SemanticError::PrintNO_TYPES(ErrorInfo &err,
                                      LexStream *lex_stream,
                                      Control &control)
{
    ErrorString s;

    s << "This compilation unit contains no type declaration.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_IN_MULTIPLE_FILES(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "The file \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << ".java\" contains type \""
      << err.insert4 << "\" which conflicts with file \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << ".java\".";

    return s.Array();
}


wchar_t *SemanticError::PrintPACKAGE_TYPE_CONFLICT(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" contained in file \""
      << err.insert3 << "\" conflicts with the package \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDIRECTORY_FILE_CONFLICT(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "The type \"" << err.insert1 << "\" contained in file \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << ".java\" conflicts with the directory \""
      << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintFILE_FILE_CONFLICT(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "Cannot write class file \"" << err.insert1
      << ".class\" because that name conflicts with the class file \""
      << err.insert2 << "\" in directory \"" << err.insert3
      << "\". This is illegal because file names are case-insensitive "
      << "in this system.";

    return s.Array();
}


wchar_t *SemanticError::PrintMULTIPLE_PUBLIC_TYPES(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The type \"" << err.insert1
      << "\" is declared public in compilation unit \""
      << lex_stream -> FileName()
      << "\" which also contains the public type, \"" << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_TYPE_AND_FILE_NAMES(ErrorInfo &err,
                                                            LexStream *lex_stream,
                                                            Control &control)
{
    ErrorString s;

    s << "The public type \"" << err.insert1
      << "\" does not match the name of its containing file \""
      << lex_stream -> FileName() << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintREFERENCE_TO_TYPE_IN_MISMATCHED_FILE(ErrorInfo &err,
                                                                  LexStream *lex_stream,
                                                                  Control &control)
{
    ErrorString s;

    s << "The type \"" << err.insert1 << "\" is defined in the file \""
      << err.insert2 << ".java\" but referenced in the file \""
      << lex_stream -> FileName()
      << "\". It is recommended that it be redefined in \""
      << err.insert1 << ".java\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_INNER_TYPE_NAME(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "The nested type name \"" << err.insert1
      << "\" is illegal, as it is enclosed in another class with the same "
      << "simple name at location " << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_TYPE_DECLARATION(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of type \"" << err.insert1
      << "\". The other occurrence is at location " << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_IMPORT_NAME(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The imported simple name \"" << err.insert1
      << "\" names a different type than the other use of the name at location "
      << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintUNNECESSARY_TYPE_IMPORT(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "Unnecessary import of type \"" << err.insert1
      << "\". The type is declared at location " << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintUNINITIALIZED_FIELD(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "The field \"" << err.insert1 << "\" is not initialized.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_MODIFIER(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "Duplicate specification of the modifier \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_ACCESS_MODIFIER(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "Duplicate specification of an access modifier. "
      << "Only one instance of \"public\", \"private\", or \"protected\" "
      << "may appear in a declaration.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_TOP_LEVEL_CLASS_MODIFIER(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1
      << "\" is not a valid modifier for a top-level class.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_INNER_CLASS_MODIFIER(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1
      << "\" is not a valid modifier for an inner class.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_STATIC_INNER_CLASS_MODIFIER(ErrorInfo &err,
                                                                 LexStream *lex_stream,
                                                                 Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1
      << "\" is not a valid modifier for an inner class that is enclosed "
      << "in an interface.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_LOCAL_CLASS_MODIFIER(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1
      << "\" is not a valid modifier for a local inner class.";

    return s.Array();
}


wchar_t *SemanticError::PrintFINAL_ABSTRACT_CLASS(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "A class may not be declared both \"final\" and \"abstract\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_INTERFACE_MODIFIER(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1 << "\" is not a valid interface modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintVOLATILE_FINAL_FIELD(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "A field may not be both \"volatile\" and \"final\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_FIELD_MODIFIER(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1 << "\" is not a valid field modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_LOCAL_MODIFIER(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1
      << "\" is not a valid local variable or parameter modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_METHOD_MODIFIER(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1 << "\" is not a valid method modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_SIGNATURE_MODIFIER(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1 << "\" is not a valid signature modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CONSTRUCTOR_MODIFIER(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1 << "\" is not a valid constructor modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CONSTANT_MODIFIER(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1 << "\" is not a valid interface field modifier.";

    return s.Array();
}


wchar_t *SemanticError::PrintRECOMPILATION(ErrorInfo &err,
                                           LexStream *lex_stream,
                                           Control &control)
{
    ErrorString s;

    s << "The type associated with this construct depends on file ";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << ".class which, in turn, depends on file ";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4
      << ".java. All files that depend on this source file, in particular, ";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << ".java should be recompiled.";

    return s.Array();
}


wchar_t *SemanticError::PrintPACKAGE_NOT_TYPE(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "Package \"" << err.insert1
      << "\" was found when a type was expected.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_FOUND(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "Type ";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << " was not found.";

    return s.Array();
}


wchar_t *SemanticError::PrintIMPORT_FROM_UNNAMED_PACKAGE(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "Type \"" << err.insert1 << "\" exists in the unnamed package, and "
      << "cannot be imported. Consider putting it into a named package.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_ON_DEMAND_IMPORT(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "Type " << err.insert1 << " is imported on demand from "
      << err.insert2 << " and " << err.insert3 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintUNKNOWN_ON_DEMAND_IMPORT(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "The import \"" << err.insert1
      << "\" is not valid, since it does not name a type in a package.";

    return s.Array();
}


wchar_t *SemanticError::PrintIMPORT_NOT_CANONICAL(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The import for nested type \"" << err.insert1
      << "\" is not valid, since it does not use the canonical name \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '.';
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintNOT_A_TYPE(ErrorInfo &err,
                                        LexStream *lex_stream,
                                        Control &control)
{
    ErrorString s;

    s << "A type is expected here.";

    return s.Array();
}


wchar_t *SemanticError::PrintNOT_A_CLASS(ErrorInfo &err,
                                         LexStream *lex_stream,
                                         Control &control)
{
    ErrorString s;

    s << "Interface \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" cannot be used where a class is expected.";

    return s.Array();
}


wchar_t *SemanticError::PrintNOT_AN_INTERFACE(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "Class ";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << " cannot be used where an interface is expected.";

    return s.Array();
}


wchar_t *SemanticError::PrintSUPER_IS_FINAL(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "The super class \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" is final, and cannot have subclasses.";

    return s.Array();
}


wchar_t *SemanticError::PrintOBJECT_WITH_SUPER_TYPE(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "The type \"java.lang.Object\" must not have an extends or implements "
      << "clause, as it has no supertype.";

    return s.Array();
}


wchar_t *SemanticError::PrintOBJECT_HAS_NO_SUPER_TYPE(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "The type \"java.lang.Object\" does not have a supertype.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_FIELD(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of field \"" << err.insert1
      << "\" in type \"" << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_METHOD(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of method \"" << err.insert1
      << "\" in type \"" << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_INHERITED_METHOD(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "The return type of method \"" << err.insert1
      << "\" does not match the return type of the accessible method \""
      << err.insert2 << "\" declared in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_IMPLICIT_METHOD(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The return type of method \"" << err.insert1
      << "\" does not match the return type of method \"" << err.insert2
      << "\" declared implicitly for interfaces.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNIMPLEMENTABLE_INTERFACE(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "Interface \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" is legal, but cannot be implemented: method \""
      << err.insert3 << "\" has a different return type than \""
      << err.insert4 << "\" declared in java.lang.Object.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNIMPLEMENTABLE_CLASS(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "Class \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" cannot be implemented: method \"" << err.insert3
      << "\" declared in \"";
    if (NotDot(err.insert4))
        s << err.insert4 << '/';
    s << err.insert5 << "\" has a different return type than the non-inherited "
      << "default access abstract method \"" << err.insert6
      << "\" declared in the superclass \"";
    if (NotDot(err.insert7))
        s << err.insert7 << '/';
    s << err.insert8 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_INHERITED_METHOD_EXTERNALLY(ErrorInfo &err,
                                                                    LexStream *lex_stream,
                                                                    Control &control)
{
    ErrorString s;

    s << "In type \"" << err.insert1 << "\", the method \"" << err.insert2
      << "\", inherited from type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4
      << "\", does not have the same return type as the method \""
      << err.insert5 << "\", inherited from type \"";
    if (NotDot(err.insert6))
        s << err.insert6 << "/";
    s << err.insert7 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_CONSTRUCTOR(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of this constructor signature in type \""
      << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISSPELLED_CONSTRUCTOR_NAME(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "The name of the constructor \"" << err.insert1
      << "\" does not match the name of the class \"" << err.insert2
      << "\". Assuming it is misspelled.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_CONSTRUCTOR_NAME(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "The name of the constructor \"" << err.insert1
      << "\" does not match the name of the class \"" << err.insert2
      << "\". Assuming it is a method with missing return type.";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_WITH_CONSTRUCTOR_NAME(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "The name of this method \"" << err.insert1
      << "\" matches the name of the containing class. However, the method "
      << "is not a constructor since its declarator is qualified with a type.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_FORMAL_PARAMETER(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of formal parameter " << err.insert1 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_LOCAL_VARIABLE_DECLARATION(ErrorInfo &err,
                                                                  LexStream *lex_stream,
                                                                  Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of local variable \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_LOCAL_TYPE_DECLARATION(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of local class \"" << err.insert1
      << "\". The other occurrence is at location " << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintMULTIPLE_DEFAULT_LABEL(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "Multiple specification of default label in switch statement.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNDECLARED_LABEL(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << err.insert1 << " is an undeclared label.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_LABEL(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "Duplicate declaration of label \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_THROWABLE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is not a subclass of \"java.lang.Throwable\".";

    return s.Array();
}


wchar_t *SemanticError::PrintCATCH_PRIMITIVE_TYPE(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "A primitive type cannot be used to declare a catch clause "
      << "parameter.";

    return s.Array();
}


wchar_t *SemanticError::PrintCATCH_ARRAY_TYPE(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "A array type cannot be used to declare a catch clause "
      << "parameter.";

    return s.Array();
}


wchar_t *SemanticError::PrintAMBIGUOUS_FIELD(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "Ambiguous access of field \"" << err.insert1
      << "\". At least two fields are accessible from here: "
      << "one declared in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" and one declared in type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintAMBIGUOUS_TYPE(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "Ambiguous use of type name \"" << err.insert1
      << "\". At least two member types are accessible from here: "
      << "one declared in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" and one declared in type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintFIELD_IS_TYPE(ErrorInfo &err,
                                           LexStream *lex_stream,
                                           Control &control)
{
    ErrorString s;

    s << "The name \"" << err.insert1
      << "\" cannot be dereferenced as it represents a type.";

    return s.Array();
}


wchar_t *SemanticError::PrintFIELD_NOT_FOUND(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "No field named \"" << err.insert1 << "\" was found in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintFIELD_NAME_MISSPELLED(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "No field named \"" << err.insert1 << "\" was found in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\". However, there is an accessible field \""
      << err.insert4 << "\" whose name closely matches the name \""
      << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintNAME_NOT_YET_AVAILABLE(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "Illegal use of name \"" << err.insert1
      << "\" which has not yet been fully declared at this point.";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_OVERLOAD_NOT_FOUND(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "No applicable overload for the method named \"" << err.insert1
      << "\" was found in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\". Perhaps you wanted the overloaded version \""
      << err.insert4 << "\" instead?";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_NOT_FOUND(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "No method named \"" << err.insert1 << "\" was found in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_NAME_MISSPELLED(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "No method named \"" << err.insert1 << "\" was found in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\". However, there is an accessible method \""
      << err.insert4 << "\" whose name closely matches the name \""
      << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintHIDDEN_METHOD_IN_ENCLOSING_CLASS(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1
      << "\" contained in the enclosing type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" is a perfect match for this method call. "
      << "However, it is not visible in this nested class because a "
      << "method with the same name in an intervening class is hiding it.";

    return s.Array();
}


wchar_t *SemanticError::PrintFIELD_NOT_METHOD(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The name \"" << err.insert1
      << "\" is not a method name but the name of a field member of the "
      << "type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_METHOD(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "The keyword \"new\" is expected before this name, \"" << err.insert1
      << "\", as it is not the name of a method but the name of a type.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_FIELD(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "A type \"" << err.insert1 << "\" was found where a field name "
      << "or method call was expected. Did you mean to write \""
      << err.insert1 << ".xxx\", or \"new " << err.insert1
      << "()\", or ... ?";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_NOT_FIELD(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The name \"" << err.insert1
      << "\" is not a field name but the name of a method declared in the "
      << "type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintAMBIGUOUS_CONSTRUCTOR_INVOCATION(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "Ambiguous invocation of constructor \"" << err.insert1
      << "\". At least two constructors are accessible from here: \""
      << err.insert2 << "\" and \"" << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintAMBIGUOUS_METHOD_INVOCATION(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "Ambiguous invocation of method \"" << err.insert1
      << "\". At least two methods are accessible from here: \""
      << err.insert2 << "\" declared in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\" and \"" << err.insert5
      << "\" declared in type \"";
    if (NotDot(err.insert6))
        s << err.insert6 << '/';
    s << err.insert7 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintNAME_NOT_CLASS_VARIABLE(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "The field \"" << err.insert1
      << "\" is not static, and cannot be accessed in this static context.";

    return s.Array();
}


wchar_t *SemanticError::PrintNOT_A_VARIABLE(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "The left-hand side of an assignment must be a variable.";

    return s.Array();
}


wchar_t *SemanticError::PrintNOT_A_NUMERIC_VARIABLE(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "Only a variable of numeric type can appear in this context.";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_NOT_CLASS_METHOD(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1
      << "\" is not static, and cannot be accessed in this static context.";

    return s.Array();
}


wchar_t *SemanticError::PrintABSTRACT_TYPE_CREATION(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "Attempt to instantiate an abstract class \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintCONSTRUCTOR_NOT_FOUND(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "No match was found for constructor \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_FOUND_FOR_CONSTRUCTOR(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "No match was found for constructor \"" << err.insert1
      << "\". However, a method with the same name was found at location "
      << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintCONSTRUCTOR_OVERLOAD_NOT_FOUND(ErrorInfo &err,
                                                            LexStream *lex_stream,
                                                            Control &control)
{
    ErrorString s;

    s << "No applicable overload was found for a constructor of type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\". Perhaps you wanted the overloaded version \""
      << err.insert3 << "\" instead?";

    return s.Array();
}


wchar_t *SemanticError::PrintINCOMPATIBLE_TYPE_FOR_INITIALIZATION(ErrorInfo &err,
                                                                  LexStream *lex_stream,
                                                                  Control &control)
{
    ErrorString s;

    s << "The type of the initializer, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\", is not assignable to the variable, of type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINCOMPATIBLE_TYPE_FOR_ASSIGNMENT(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "The type of the right sub-expression, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\", is not assignable to the variable, of type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINCOMPATIBLE_TYPE_FOR_CONDITIONAL_EXPRESSION(ErrorInfo &err,
                                                                          LexStream *lex_stream,
                                                                          Control &control)
{
    ErrorString s;

    s << "In the conditional, the type of the true sub-expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2
      << "\", is not compatible with the type of the false sub-expression, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintVOID_ARRAY(ErrorInfo &err,
                                        LexStream *lex_stream,
                                        Control &control)
{
    ErrorString s;

    s << "Arrays of type \"void\" are not legal.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_THROWS_CLAUSE_CLASS(ErrorInfo &err,
                                                           LexStream *lex_stream,
                                                           Control &control)
{
    ErrorString s;

    s << "The duplicate listing of type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2
      << "\" in the throws clause is not necessary.";

    return s.Array();
}


wchar_t *SemanticError::PrintREDUNDANT_THROWS_CLAUSE_CLASS(ErrorInfo &err,
                                                           LexStream *lex_stream,
                                                           Control &control)
{
    ErrorString s;

    s << "The listing of type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2
      << "\" in the throws clause is not necessary, since its superclass, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\", is also listed.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNCHECKED_THROWS_CLAUSE_CLASS(ErrorInfo &err,
                                                           LexStream *lex_stream,
                                                           Control &control)
{
    ErrorString s;

    s << "Since type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" is an unchecked exception, it does not need to be "
      << "listed in the throws clause.";

    return s.Array();
}


wchar_t *SemanticError::PrintINCOMPATIBLE_TYPE_FOR_BINARY_EXPRESSION(ErrorInfo &err,
                                                                     LexStream *lex_stream,
                                                                     Control &control)
{
    ErrorString s;

    s << "The type of the left sub-expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", is not compatible with the type of the "
      << "right sub-expression, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_INSTANCEOF_CONVERSION(ErrorInfo &err,
                                                           LexStream *lex_stream,
                                                           Control &control)
{
    ErrorString s;

    s << "The type of the left sub-expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", cannot possibly be an instance of type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CAST_CONVERSION(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "An expression of type \"" << err.insert1
      << "\" cannot be cast into type \"" << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CAST_TYPE(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "Expression found where a type is expected.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_INTEGRAL(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "The type of this expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", is not an integral type.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_NUMERIC(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The type of this expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", is not numeric.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_INTEGER(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The type of this expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2
      << "\", is not assignable to \"int\".";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_BOOLEAN(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The type of this expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", is not \"boolean\".";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_ARRAY(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "The type of this expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", is not an array type.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_REFERENCE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The type of this expression, \"" << err.insert1
      << "\", is not a reference type.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_IS_VOID(ErrorInfo &err,
                                          LexStream *lex_stream,
                                          Control &control)
{
    ErrorString s;

    s << "An expression of type \"void\" is not valid in this context where "
      << "a value is expected.";

    return s.Array();
}


wchar_t *SemanticError::PrintVALUE_NOT_REPRESENTABLE_IN_SWITCH_TYPE(ErrorInfo &err,
                                                                    LexStream *lex_stream,
                                                                    Control &control)
{
    ErrorString s;

    s << "The value of this expression, " << err.insert1
      << ", cannot be represented in the type of the switch statement "
      << "expression, \"" << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_CASE_VALUE(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The value of this expression, " << err.insert1
      << ", has already been used in this switch statement.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_THIS_EXPRESSION(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "A \"this\" expression may only be used in the body of an instance "
      << "method, constructor (after the explicit constructor invocation, if "
      << "any), initializer block, or in the initializer expression of an "
      << "instance variable.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_SUPER_EXPRESSION(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "A \"super\" expression may only be used in the body of an instance "
      << "method, constructor (after any explicit constructor invocation), "
      << "initializer block, or in an instance variable initializer.";

    return s.Array();
}


wchar_t *SemanticError::PrintVARIABLE_NOT_DEFINITELY_UNASSIGNED_IN_LOOP(ErrorInfo &err,
                                                                        LexStream *lex_stream,
                                                                        Control &control)
{
    ErrorString s;

    s << "The blank final variable \"" << err.insert1
      << "\" cannot be assigned within the body of a loop that may execute "
      << "more than once.";

    return s.Array();
}


wchar_t *SemanticError::PrintVARIABLE_NOT_DEFINITELY_UNASSIGNED(ErrorInfo &err,
                                                                LexStream *lex_stream,
                                                                Control &control)
{
    ErrorString s;

    s << "Possible attempt to reassign a value to the blank final variable \""
      << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintFINAL_VARIABLE_NOT_BLANK(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "The final variable \"" << err.insert1
      << "\" is not a blank final in this context, so it may not be assigned.";

    return s.Array();
}


wchar_t *SemanticError::PrintFINAL_FIELD_ASSIGNMENT_NOT_SIMPLE(ErrorInfo &err,
                                                               LexStream *lex_stream,
                                                               Control &control)
{
    ErrorString s;

    s << "The final field \"" << err.insert1
      << "\" may not be assigned in a qualified expression. Use a simple name";
    if (err.insert2)
        s << " or this." << err.insert2;
    s << " instead.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNINITIALIZED_FINAL_VARIABLE(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "The blank final field \"" << err.insert1
      << "\" must be initialized in an instance initializer block or instance "
      << "field initializer, since this class has no explicit constructor.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNINITIALIZED_STATIC_FINAL_VARIABLE(ErrorInfo &err,
                                                                 LexStream *lex_stream,
                                                                 Control &control)
{
    ErrorString s;

    s << "The blank static final field \"" << err.insert1
      << "\" must be initialized in a static initializer block or static "
      << "field initializer.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNINITIALIZED_FINAL_VARIABLE_IN_CONSTRUCTOR(ErrorInfo &err,
                                                                         LexStream *lex_stream,
                                                                         Control &control)
{
    ErrorString s;

    s << "The blank final field \"" << err.insert1
      << "\" must be initialized in this and every constructor which does not "
      << "call a form of this(); or else once in an instance initializer "
      << "block or instance field initializer.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNINITIALIZED_FINAL_VARIABLE_IN_INTERFACE(ErrorInfo &err,
                                                                       LexStream *lex_stream,
                                                                       Control &control)
{
    ErrorString s;

    s << "The interface field \"" << err.insert1
      << "\" must have an initializer.";

    return s.Array();
}


wchar_t *SemanticError::PrintINIT_SCALAR_WITH_ARRAY(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "An array initializer cannot be used to initialize a variable of "
      << "type \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINIT_ARRAY_WITH_SCALAR(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "A single expression cannot be used to initialize an array variable "
      << "of type \"" << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_BYTE_VALUE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "A byte value must be an integer value in the range -128..127.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_SHORT_VALUE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "A short value must be an integer value in the range -32768..32767.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CHARACTER_VALUE(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "A character must be an the range 0..65535 ('\\u0000'..'\\uffff').";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_INT_VALUE(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "The value of an \"int\" literal must be a decimal value in the "
      << "range -2147483648..2147483647 or a hexadecimal or octal literal "
      << "that fits in 32 bits.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_LONG_VALUE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The value of a long literal must be a decimal value in the range "
      << "-9223372036854775808L..9223372036854775807L or a hexadecimal or "
      << "octal literal that fits in 64 bits.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_FLOAT_VALUE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "The value of a float literal must not round to infinity or zero.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_DOUBLE_VALUE(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The value of a double literal must not round to infinity or zero.";

    return s.Array();
}


wchar_t *SemanticError::PrintRETURN_STATEMENT_IN_INITIALIZER(ErrorInfo &err,
                                                             LexStream *lex_stream,
                                                             Control &control)
{
    ErrorString s;

    s << "A return statement may not appear in an initializer block.";

    return s.Array();
}


wchar_t *SemanticError::PrintABRUPT_INITIALIZER(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "An initializer block must be able to complete normally.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_RETURN_WITH_EXPRESSION(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "A return statement with expression must be contained in a method "
      << "declaration that is declared to return a value.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_RETURN_WITH_NO_EXPRESSION(ErrorInfo &err,
                                                                 LexStream *lex_stream,
                                                                 Control &control)
{
    ErrorString s;

    s << "A return statement with no expression may only appear in void "
      << "method or a constructor.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_RETURN_AND_METHOD_TYPE(ErrorInfo &err,
                                                               LexStream *lex_stream,
                                                               Control &control)
{
    ErrorString s;

    s << "The type of this return expression, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", does not match the return type of the method, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << '/';
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintEXPRESSION_NOT_THROWABLE(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "The expression in a throw statement must be assignable to the "
      << "type \"java.lang.Throwable.\"";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_BREAK_STATEMENT(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "A \"break\" statement must be enclosed in a \"switch\", \"while\", "
      << "\"do\" or \"for\" statement.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_CONTINUE_STATEMENT(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "A \"continue\" statement must be enclosed in a \"while\", \"do\" "
      << "or \"for\" statement.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISPLACED_EXPLICIT_CONSTRUCTOR(ErrorInfo &err,
                                                            LexStream *lex_stream,
                                                            Control &control)
{
    ErrorString s;

    s << "Misplaced explicit constructor invocation. It may only be the "
      << "first statement in constructors.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CONTINUE_TARGET(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "The statement labeled \"" << err.insert1
      << "\" cannot be continued since it is not a \"while\", \"do\" or "
      << "\"for\" statement.";

    return s.Array();
}


wchar_t *SemanticError::PrintNON_ABSTRACT_TYPE_CONTAINS_ABSTRACT_METHOD(ErrorInfo &err,
                                                                        LexStream *lex_stream,
                                                                        Control &control)
{
    ErrorString s;

    s << "The abstract method \"" << err.insert1
      << "\" is enclosed in class \"" << err.insert2
      << "\" which is not abstract.";

    return s.Array();
}


wchar_t *SemanticError::PrintNON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD(ErrorInfo &err,
                                                                        LexStream *lex_stream,
                                                                        Control &control)
{
    ErrorString s;

    s << "The abstract method \"" << err.insert1
      << "\", inherited from type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3 << "\", is not implemented in the non-abstract class \"";
    if (NotDot(err.insert4))
        s << err.insert4 << '/';
    s << err.insert5 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintNON_ABSTRACT_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD(ErrorInfo &err,
                                                                                       LexStream *lex_stream,
                                                                                       Control &control)
{
    ErrorString s;

    s << "The abstract method \"" << err.insert1
      << "\", belonging to the superclass \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3 << "\", has default access, so it is not inherited and "
      << "cannot be implemented in this package. Therefore, class \"";
    if (NotDot(err.insert4))
        s << err.insert4 << '/';
    s << err.insert5 << "\" must be abstract.";

    return s.Array();
}


wchar_t *SemanticError::PrintANONYMOUS_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD(ErrorInfo &err,
                                                                                    LexStream *lex_stream,
                                                                                    Control &control)
{
    ErrorString s;

    s << "The abstract method \"" << err.insert1
      << "\", belonging to the class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3 << "\", has default access, so it is not inherited and "
      << "cannot be implemented in this package. Therefore, an anonymous "
      << "subclass cannot be created here.";

    return s.Array();
}


wchar_t *SemanticError::PrintDUPLICATE_INTERFACE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "Duplicate specification of interface \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" in definition of type \"" << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintUNKNOWN_AMBIGUOUS_NAME(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "\"" << err.insert1
      << "\" is either a misplaced package name or a non-existent entity. "
      << "An expression name is expected in this context.";

    return s.Array();
}


wchar_t *SemanticError::PrintCIRCULAR_CLASS(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "The class \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" may not have a superclass or superinterface "
      << "which extends itself, or which is enclosed by itself or a subclass.";

    return s.Array();
}


wchar_t *SemanticError::PrintCIRCULAR_INTERFACE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The interface \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" may not have a superinterface which extends "
      << "itself, or which is enclosed by itself or any subtype.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_ACCESSIBLE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\" has " << err.insert3
      << " access and is not accessible here.";

    return s.Array();
}


wchar_t *SemanticError::PrintFIELD_NOT_ACCESSIBLE(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The field \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3 << "\" has " << err.insert4
      << " access and is not accessible here.";

    return s.Array();
}


wchar_t *SemanticError::PrintPROTECTED_INSTANCE_FIELD_NOT_ACCESSIBLE(ErrorInfo &err,
                                                                     LexStream *lex_stream,
                                                                     Control &control)
{
    ErrorString s;

    s << "The instance field \"" << err.insert1 << "\" in class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3
      << "\" has protected access, but the qualifying expression is not of "
      << "type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << '/';
    s << err.insert5 << "\" or any of its enclosing types.";

    return s.Array();
}


wchar_t *SemanticError::PrintMETHOD_NOT_ACCESSIBLE(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3 << "\" has " << err.insert4
      << " access and is not accessible here.";

    return s.Array();
}


wchar_t *SemanticError::PrintPROTECTED_INSTANCE_METHOD_NOT_ACCESSIBLE(ErrorInfo &err,
                                                                      LexStream *lex_stream,
                                                                      Control &control)
{
    ErrorString s;

    s << "The instance method \"" << err.insert1 << "\" in class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err.insert3
      << "\" has protected access, but the qualifying expression is not of "
      << "type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << '/';
    s << err.insert5 << "\" or any of its enclosing types.";

    return s.Array();
}


wchar_t *SemanticError::PrintPROTECTED_INTERFACE_METHOD_NOT_ACCESSIBLE(ErrorInfo &err,
                                                                       LexStream *lex_stream,
                                                                       Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1 << "\" only has protected access in "
      << "\"java.lang.Object\", so it is not accessible from an interface.";

    return s.Array();
}


wchar_t *SemanticError::PrintCONSTRUCTOR_NOT_ACCESSIBLE(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The constructor \"" << err.insert1 << "\" in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << '/';
    s << err. insert3 << "\" has " << err.insert4
      << " access and is not accessible here.";

    return s.Array();
}


wchar_t *SemanticError::PrintPARAMETER_REDECLARED(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The name of a formal parameter, \"" << err.insert1
      << "\", may not be used to declare a local variable or an exception "
      << "parameter.";

    return s.Array();
}


wchar_t *SemanticError::PrintBAD_ABSTRACT_METHOD_MODIFIER(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "A method declaration that contains the keyword \"abstract\" may "
      << "not contain any of the keywords: \"private\", \"static\", "
      << "\"final\", \"native\", \"strictfp\" or \"synchronized\".";

    return s.Array();
}


wchar_t *SemanticError::PrintABSTRACT_METHOD_MODIFIER_CONFLICT(ErrorInfo &err,
                                                               LexStream *lex_stream,
                                                               Control &control)
{
    ErrorString s;

    s << "An \"abstract\" method may not also contain the keyword \""
      << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintSTRICTFP_NATIVE_METHOD(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "A \"native\" method method may not also be \"strictfp\".";

    return s.Array();
}


wchar_t *SemanticError::PrintABSTRACT_METHOD_INVOCATION(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "An abstract method, \"" << err.insert1 << "\", cannot be invoked.";

    return s.Array();
}


wchar_t *SemanticError::PrintFINAL_IMPLICIT_METHOD_OVERRIDE(ErrorInfo &err,
                                                            LexStream *lex_stream,
                                                            Control &control)
{
    ErrorString s;

    s << "The explicit method \"" << err.insert1
      << "\" is not allowed in an interface, because it conflicts with the "
      << "final method \"" << err.insert2
      << "\" declared implicitly for interfaces.";

    return s.Array();
}


wchar_t *SemanticError::PrintFINAL_METHOD_OVERRIDE(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1
      << "\" cannot replace the accessible final method \""
      << err.insert2 << "\" declared in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintCLASS_METHOD_OVERRIDE(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The instance method \"" << err.insert1
      << "\" cannot override the accessible static method \""
      << err.insert2 << "\" declared in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINSTANCE_METHOD_OVERRIDE(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "The static method \"" << err.insert1
      << "\" cannot hide the accessible instance method \"" << err.insert2
      << "\" declared in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINSTANCE_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &err,
                                                                 LexStream *lex_stream,
                                                                 Control &control)
{
    ErrorString s;

    s << "In class \"" << err.insert1 << "\", the static method \""
      << err.insert2 << "\", inherited from the superclass \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\", conflicts with the abstract instance method \""
      << err.insert5 << "\", inherited from the interface \"";
    if (NotDot(err.insert6))
        s << err.insert6 << "/";
    s << err.insert7 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintBAD_ACCESS_METHOD_OVERRIDE(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1 << "\" with " << err.insert2
      << " access cannot replace the accessible method \"" << err.insert3
      << "\" with " << err.insert4 << " access declared in type \"";
    if (NotDot(err.insert5))
        s << err.insert5 << "/";
    s << err.insert6 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintBAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY(ErrorInfo &err,
                                                                   LexStream *lex_stream,
                                                                   Control &control)
{
    ErrorString s;

    s << "In class \"" << err.insert1 << "\", the method \"" << err.insert2
      << "\" with " << err.insert3 << " access, inherited from type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\", cannot override the method \"" << err.insert6
      << "\" with " << err.insert7 << " access, inherited from type \"";
    if (NotDot(err.insert8))
        s << err.insert8 << "/";
    s << err.insert9 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_OVERRIDDEN_EXCEPTION(ErrorInfo &err,
                                                             LexStream *lex_stream,
                                                             Control &control)
{
    ErrorString s;

    s << "The checked exception \"" << err.insert1
      << "\" is not assignable to any exception in the throws clause of the "
      << "accessible method \"" << err.insert2 << "\" declared in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_IMPLICIT_OVERRIDDEN_EXCEPTION(ErrorInfo &err,
                                                                      LexStream *lex_stream,
                                                                      Control &control)
{
    ErrorString s;

    s << "The checked exception \"" << err.insert1
      << "\" is not compatible with the throws clause in the method \""
      << err.insert2 << "\" declared implicitly for interfaces.";

    return s.Array();
}


wchar_t *SemanticError::PrintMISMATCHED_OVERRIDDEN_EXCEPTION_EXTERNALLY(ErrorInfo &err,
                                                                        LexStream *lex_stream,
                                                                        Control &control)
{
    ErrorString s;

    s << "In type \"" << err.insert1 << "\", the checked exception \""
      << err.insert2 << "\" specified by method \"" << err.insert3
      << "\", inherited from type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\", is not assignable to any exception in the "
      << "throws clause of the overridden method \""
      << err.insert6 << "\" declared in type \"";
    if (NotDot(err.insert7))
        s << err.insert7 << "/";
    s << err.insert8 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintABSTRACT_METHOD_WITH_BODY(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "The declaration of the abstract or native method, \""
      << err.insert1 << "\", must not contain a method body.";

    return s.Array();
}


wchar_t *SemanticError::PrintNON_ABSTRACT_METHOD_WITHOUT_BODY(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "The declaration of the non-abstract and non-native method, \""
      << err.insert1 << "\", must contain a method body.";

    return s.Array();
}


wchar_t *SemanticError::PrintCIRCULAR_THIS_CALL(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The constructor \"" << err.insert1
      << "\" may not directly or indirectly invoke itself.";

    return s.Array();
}


wchar_t *SemanticError::PrintINSTANCE_VARIABLE_IN_EXPLICIT_CONSTRUCTOR(ErrorInfo &err,
                                                                       LexStream *lex_stream,
                                                                       Control &control)
{
    ErrorString s;

    s << "The instance variable \"" << err.insert1 << "\" declared in class \""
      << err.insert2
      << "\" is not accessible in an explicit constructor invocation.";

    return s.Array();
}


wchar_t *SemanticError::PrintINSTANCE_METHOD_IN_EXPLICIT_CONSTRUCTOR(ErrorInfo &err,
                                                                     LexStream *lex_stream,
                                                                     Control &control)
{
    ErrorString s;

    s << "The instance method \"" << err.insert1 << "\" declared in class \""
      << err.insert2
      << "\" is not accessible in an explicit constructor invocation.";

    return s.Array();
}


wchar_t *SemanticError::PrintSYNTHETIC_VARIABLE_ACCESS(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "Illegal attempt to access the synthetic field \"" << err.insert1
      << "\" contained in class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintSYNTHETIC_METHOD_INVOCATION(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "Illegal attempt to invoke the synthetic method \""
      << err.insert1 << "\" contained in class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintSYNTHETIC_CONSTRUCTOR_INVOCATION(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "Illegal attempt to invoke the synthetic constructor \""
      << err.insert1 << "\" from class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintSELF_IN_EXPLICIT_CONSTRUCTOR(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "The expression \"" << err.insert1
      << "\" is not yet initialized here.";

    return s.Array();
}


wchar_t *SemanticError::PrintINNER_CONSTRUCTOR_IN_EXPLICIT_CONSTRUCTOR(ErrorInfo &err,
                                                                       LexStream *lex_stream,
                                                                       Control &control)
{
    ErrorString s;

    s << "The constructor for class \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is not accessible from an explicit constructor "
      << "invocation in the immediately enclosing class \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintEXPRESSION_NOT_CONSTANT(ErrorInfo &err,
                                                     LexStream *lex_stream,
                                                     Control &control)
{
    ErrorString s;

    s << "A constant expression is expected in this context. A constant "
      << "expression is built from literals, operators, and constant "
      << "variables referenced by 'id' or 'Classname.id'.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNCAUGHT_METHOD_EXCEPTION(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1
      << "\" can throw the checked exception \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\", so its invocation" << err.insert4;

    return s.Array();
}


wchar_t *SemanticError::PrintUNCAUGHT_CONSTRUCTOR_EXCEPTION(ErrorInfo &err,
                                                            LexStream *lex_stream,
                                                            Control &control)
{
    ErrorString s;

    s << "The constructor \"" << err.insert1
      << "\" can throw the checked exception \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\", so the class creation" << err.insert4;

    return s.Array();
}


wchar_t *SemanticError::PrintUNCAUGHT_ANONYMOUS_CONSTRUCTOR_EXCEPTION(ErrorInfo &err,
                                                                      LexStream *lex_stream,
                                                                      Control &control)
{
    ErrorString s;

    s << "The constructor in the anonymous subclass of \"" << err.insert1
      << "\" can throw the checked exception \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\", so the class creation" << err.insert4;

    return s.Array();
}


wchar_t *SemanticError::PrintUNCAUGHT_THROWN_EXCEPTION(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "This throw statement throws the checked exception \"";
    if (NotDot(err.insert1))
        s << err.insert1 << '/';
    s << err.insert2 << "\", so it" << err.insert3;

    return s.Array();
}


wchar_t *SemanticError::PrintUNCAUGHT_EXPLICIT_THIS_EXCEPTION(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "This constructor must declare the checked exception \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" thrown by the explicit this() call.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNCAUGHT_EXPLICIT_SUPER_EXCEPTION(ErrorInfo &err,
                                                               LexStream *lex_stream,
                                                               Control &control)
{
    ErrorString s;

    s << "This constructor must declare the checked exception \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" thrown by the explicit super() call to type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintUNREACHABLE_CATCH_CLAUSE(ErrorInfo &err,
                                                      LexStream *lex_stream,
                                                      Control &control)
{
    ErrorString s;

    s << "This catch block is unreachable because there is no exception "
      << "whose type is assignable to \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" that can be thrown during execution of the "
      << "body of the try block.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNREACHABLE_STATEMENT(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "This statement is unreachable.";

    return s.Array();
}


wchar_t *SemanticError::PrintUNREACHABLE_STATEMENTS(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "These statements are unreachable.";

    return s.Array();
}


wchar_t *SemanticError::PrintBLOCKED_CATCH_CLAUSE(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "This catch block is unreachable: the exception \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is a subclass of the type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\", caught previously at location "
      << err.insert5 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintVARIABLE_NOT_DEFINITELY_ASSIGNED(ErrorInfo &err,
                                                              LexStream *lex_stream,
                                                              Control &control)
{
    ErrorString s;

    s << "The variable \"" << err.insert1
      << "\" may be accessed here before having been definitely assigned a "
      << "value.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPED_METHOD_WITH_NO_RETURN(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1
      << "\" must contain a return statement with an expression compatible "
      << "with type \"" << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDEFAULT_METHOD_NOT_OVERRIDDEN(ErrorInfo &err,
                                                           LexStream *lex_stream,
                                                           Control &control)
{
    ErrorString s;

    s << "Method \"" << err.insert1 << "\" in class \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" does not override or hide the corresponding "
      << "method with default access in class \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_IN_UNNAMED_PACKAGE(ErrorInfo &err,
                                                         LexStream *lex_stream,
                                                         Control &control)
{
    ErrorString s;

    s << "The file \"" << err.insert1 << ".class\" was found in directory \""
      << err.insert2 << "\" specified in the CLASSPATH. However, that class "
      << "file specifies a type associated with the named package \""
      << err.insert3 << "\" instead of the unnamed package.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_IN_WRONG_PACKAGE(ErrorInfo &err,
                                                   LexStream *lex_stream,
                                                   Control &control)
{
    ErrorString s;

    s << "The type \"" << err.insert1 << "\" was found in package \""
      << err.insert2 << "\". However, that type is associated with ";
    if (wcslen(err.insert3) == 0)
        s << "the unnamed package";
    else
        s << "another named package, \"" << err.insert3 << "\"";
    s << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NAME_MISMATCH(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The name of the type specified, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\", does not match the name found in the class file: \""
      << err.insert3 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintDEPRECATED_TYPE(ErrorInfo &err,
                                             LexStream *lex_stream,
                                             Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" has been deprecated.";

    return s.Array();
}


wchar_t *SemanticError::PrintDEPRECATED_FIELD(ErrorInfo &err,
                                              LexStream *lex_stream,
                                              Control &control)
{
    ErrorString s;

    s << "The variable \"" << err.insert1 << "\" declared in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" has been deprecated.";

    return s.Array();
}


wchar_t *SemanticError::PrintDEPRECATED_METHOD(ErrorInfo &err,
                                               LexStream *lex_stream,
                                               Control &control)
{
    ErrorString s;

    s << "The method \"" << err.insert1 << "\" declared in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" has been deprecated.";

    return s.Array();
}


wchar_t *SemanticError::PrintDEPRECATED_CONSTRUCTOR(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "The constructor \"" << err.insert1 << "\" declared in type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\" has been deprecated.";

    return s.Array();
}


wchar_t *SemanticError::PrintONE_UNNAMED_PACKAGE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "The type \"" << err.insert1 << "\" was found in directory \""
      << err.insert2 << "\" specified in the CLASSPATH. It is accessible "
      << "here only because, by default, this compiler uses one unnamed "
      << "package. In a compiler that associates an unnamed package with "
      << "each directory (as this compiler does with the +P option) this "
      << "access would be illegal.";

    return s.Array();
}



wchar_t *SemanticError::PrintCOMPRESSED_ZIP_FILE(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "The file " << err.insert1 << "(" << err.insert2 << "/"
      << err.insert3
      << ") is in an unsupported compressed format. (Unzip and) Rezip \""
      << err.insert1 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_CLASS_FILE(ErrorInfo &err,
                                                LexStream *lex_stream,
                                                Control &control)
{
    ErrorString s;

    s << "The class file \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << ".class\" has an invalid format.";

    return s.Array();
}


wchar_t *SemanticError::PrintCANNOT_OPEN_CLASS_FILE(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "Unable to open file associated with type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINTERFACE_NOT_INNER_CLASS(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "The interface \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is not an inner class.";

    return s.Array();
}


wchar_t *SemanticError::PrintSTATIC_NOT_INNER_CLASS(ErrorInfo &err,
                                                    LexStream *lex_stream,
                                                    Control &control)
{
    ErrorString s;

    s << "The interface \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is not an inner class.";

    return s.Array();
}


wchar_t *SemanticError::PrintTYPE_NOT_INNER_CLASS(ErrorInfo &err,
                                                  LexStream *lex_stream,
                                                  Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << "\", is not an inner class that is immediately enclosed in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintSUPER_TYPE_NOT_INNER_CLASS(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The super type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" of this type, \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4
      << "\", is not an inner class that is immediately enclosed in type \"";
    if (NotDot(err.insert5))
        s << err.insert5 << "/";
    s << err.insert6 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintSTATIC_FIELD_IN_INNER_CLASS_NOT_FINAL(ErrorInfo &err,
                                                                   LexStream *lex_stream,
                                                                   Control &control)
{
    ErrorString s;

    s << "This static variable declaration is invalid, because it is not "
      << "final, but is enclosed in an inner class, \"" << err.insert1
      << "\", located at " << err.insert2 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintSTATIC_FIELD_IN_INNER_CLASS_NOT_CONSTANT(ErrorInfo &err,
                                                                      LexStream *lex_stream,
                                                                      Control &control)
{
    ErrorString s;

    s << "The static final field \"" << err.insert1
      << "\" is invalid, because it does not represent a compile-time "
      << "constant, but is enclosed in an inner class, \""
      << err.insert2 << "\", located at " << err.insert3 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintSTATIC_METHOD_IN_INNER_CLASS(ErrorInfo &err,
                                                          LexStream *lex_stream,
                                                          Control &control)
{
    ErrorString s;

    s << "The static method \"" << err.insert1
      << "\" is invalid, because it is enclosed in an inner class, \""
      << err.insert2 << "\", located at " << err.insert3 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintSTATIC_TYPE_IN_INNER_CLASS(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The static type \"" << err.insert1
      << "\" is invalid, because it is enclosed in an inner class, \""
      << err.insert2 << "\", located at " << err.insert3 << '.';

    return s.Array();
}


wchar_t *SemanticError::PrintSTATIC_INITIALIZER_IN_INNER_CLASS(ErrorInfo &err,
                                                               LexStream *lex_stream,
                                                               Control &control)
{
    ErrorString s;

    s << "This static initializer is invalid, because it is enclosed in "
      << "an inner class, \"" << err.insert1 << "\", located at "
      << err.insert2 << '.';

    return s.Array();
}



wchar_t *SemanticError::PrintINNER_CLASS_REFERENCE_TO_NON_FINAL_LOCAL_VARIABLE(ErrorInfo &err,
                                                                               LexStream *lex_stream,
                                                                               Control &control)
{
    ErrorString s;

    s << "Invalid reference in inner class \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" to a non-final local variable, \""
      << err.insert3 << "\", declared in method \"" << err.insert4 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL(ErrorInfo &err,
                                                                                 LexStream *lex_stream,
                                                                                 Control &control)
{
    ErrorString s;

    s << "The unqualified usage of \"" << err.insert1
      << "\" refers to the member inherited from type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3
      << "\", and not the local version in the enclosing method \""
      << err.insert4 << "\". Renaming the local version is suggested.";

    return s.Array();
}


wchar_t *SemanticError::PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER(ErrorInfo &err,
                                                                                  LexStream *lex_stream,
                                                                                  Control &control)
{
    ErrorString s;

    s << "The unqualified usage of \"" << err.insert1
      << "\" refers to the member inherited from type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3
      << "\", and not the version declared in the enclosing type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\". Explicit qualification is suggested.";

    return s.Array();
}


wchar_t *SemanticError::PrintINHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_TYPE(ErrorInfo &err,
                                                                                LexStream *lex_stream,
                                                                                Control &control)
{
    ErrorString s;

    s << "The unqualified usage of \"" << err.insert1
      << "\" refers to the inherited member type \"";
    if (NotDot(err.insert2))
        s << err.insert2 << "/";
    s << err.insert3 << "\", and not the enclosing type \"";
    if (NotDot(err.insert4))
        s << err.insert4 << "/";
    s << err.insert5 << "\". Explicit qualification is suggested.";

    return s.Array();
}


wchar_t *SemanticError::PrintILLEGAL_THIS_FIELD_ACCESS(ErrorInfo &err,
                                                       LexStream *lex_stream,
                                                       Control &control)
{
    ErrorString s;

    s << "The type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is either not an outer type of type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4 << "\" or it is not accessible because this "
      << "expression appears in a static region.";

    return s.Array();
}


wchar_t *SemanticError::PrintENCLOSING_INSTANCE_ACCESS_FROM_CONSTRUCTOR_INVOCATION(ErrorInfo &err,
                                                                                   LexStream *lex_stream,
                                                                                   Control &control)
{
    ErrorString s;

    s << "The innermost enclosing instance of type \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\" is \"this\", which is not yet initialized here.";

    return s.Array();
}


wchar_t *SemanticError::PrintENCLOSING_INSTANCE_ACCESS_ACROSS_STATIC_REGION(ErrorInfo &err,
                                                                            LexStream *lex_stream,
                                                                            Control &control)
{
    ErrorString s;

    s << "An instance of \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2
      << ".this\" exists, but is not accessible at this location because an "
      << "intermediate anonymous type occurs in an explicit constructor call.";

    return s.Array();
}


wchar_t *SemanticError::PrintENCLOSING_INSTANCE_NOT_ACCESSIBLE(ErrorInfo &err,
                                                               LexStream *lex_stream,
                                                               Control &control)
{
    ErrorString s;

    s << "An instance of \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << ".this\" is not accessible here. In general, an "
      << "enclosing instance is accessible only in the body of an instance "
      << "method, constructor (after the explicit constructor invocation, if "
      << "any), initializer block, or in the initializer expression of an "
      << "instance variable.";

    return s.Array();
}


wchar_t *SemanticError::PrintZERO_DIVIDE_CAUTION(ErrorInfo &err,
                                                 LexStream *lex_stream,
                                                 Control &control)
{
    ErrorString s;

    s << "Integer division will fail with division by zero.";

    return s.Array();
}


wchar_t *SemanticError::PrintVOID_TO_STRING(ErrorInfo &err,
                                            LexStream *lex_stream,
                                            Control &control)
{
    ErrorString s;

    s << "Attempt to convert a void expression into java.lang.String.";

    return s.Array();
}


wchar_t *SemanticError::PrintINVALID_ENCLOSING_INSTANCE(ErrorInfo &err,
                                                        LexStream *lex_stream,
                                                        Control &control)
{
    ErrorString s;

    s << "The super type of this type, \"";
    if (NotDot(err.insert1))
        s << err.insert1 << "/";
    s << err.insert2 << "\", is immediately enclosed in type \"";
    if (NotDot(err.insert3))
        s << err.insert3 << "/";
    s << err.insert4
      << "\" which does not match the type of this primary expression, \"";
    if (NotDot(err.insert5))
        s << err.insert5 << "/";
    s << err.insert6 << "\".";

    return s.Array();
}


wchar_t *SemanticError::PrintCONSTRUCTOR_FOUND_IN_ANONYMOUS_CLASS(ErrorInfo &err,
                                                                  LexStream *lex_stream,
                                                                  Control &control)
{
    ErrorString s;

    s << "An anonymous class cannot have a constructor. Assuming that \""
      << err.insert1 << "\" is a method with missing return type.";

    return s.Array();
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

