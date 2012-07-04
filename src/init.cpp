// $Id: init.cpp,v 1.15 2001/09/14 05:31:33 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"
#include "semantic.h"
#include "control.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

void Semantic::ProcessVariableInitializer(AstVariableDeclarator *variable_declarator)
{
    if (! variable_declarator -> variable_initializer_opt)
        return;

    VariableSymbol *symbol = variable_declarator -> symbol;

    AstArrayInitializer *array_initializer = variable_declarator -> variable_initializer_opt -> ArrayInitializerCast();
    if (array_initializer)
    {
        //
        // TODO: This code is not needed
        //
        // REMOVE:
        //
        // This operation may throw OutOfMemoryError
        //
        // SymbolSet *exception_set = TryExceptionTableStack().Top();
        // if (exception_set)
        // {
        //     exception_set -> AddElement(control.RuntimeException());
        //     exception_set -> AddElement(control.Error());
        // }

        ProcessArrayInitializer(array_initializer, symbol -> Type());
    }
    else
    {
        AstExpression *init = (AstExpression *) variable_declarator -> variable_initializer_opt;
        ProcessExpressionOrStringConstant(init);
        TypeSymbol *field_type = symbol -> Type();

        if (field_type != init -> Type() && init -> Type() != control.no_type)
        {
            if (CanAssignmentConvert(field_type, init))
            {
                init = ConvertToType(init, field_type);
                variable_declarator -> variable_initializer_opt = init;
            }
            else if (init -> IsConstant() && control.IsSimpleIntegerValueType(init -> Type())
                                          && control.IsSimpleIntegerValueType(field_type))
            {
                if (field_type == control.byte_type)
                     ReportSemError(SemanticError::INVALID_BYTE_VALUE,
                                    init -> LeftToken(),
                                    init -> RightToken());
                else if (field_type == control.char_type)
                     ReportSemError(SemanticError::INVALID_CHARACTER_VALUE,
                                    init -> LeftToken(),
                                    init -> RightToken());
                else ReportSemError(SemanticError::INVALID_SHORT_VALUE,
                                    init -> LeftToken(),
                                    init -> RightToken());
            }
            else
            {
                ReportSemError(SemanticError::INCOMPATIBLE_TYPE_FOR_ASSIGNMENT,
                               variable_declarator -> LeftToken(),
                               init -> RightToken(),
                               field_type -> ContainingPackage() -> PackageName(),
                               field_type -> ExternalName(),
                               init -> Type() -> ContainingPackage() -> PackageName(),
                               init -> Type() -> ExternalName());
            }
        }
    
        if (symbol -> ACC_FINAL() &&
            (field_type -> Primitive() || field_type == control.String()))
        {
            if (init -> IsConstant())
            {
                symbol -> initial_value = init -> value;
            }
            else if (symbol -> ACC_STATIC() && ThisType() -> IsInner())
            {
                ReportSemError(SemanticError::STATIC_FIELD_IN_INNER_CLASS_NOT_CONSTANT,
                               variable_declarator -> LeftToken(),
                               variable_declarator -> RightToken(),
                               lex_stream -> NameString(variable_declarator -> variable_declarator_name -> identifier_token),
                               ThisType() -> Name(),
                               ThisType() -> FileLoc());
            }
        }
    }

    return;
}


void Semantic::ProcessArrayInitializer(AstArrayInitializer *array_initializer, TypeSymbol *type)
{
    if (! type -> IsArray())
    {
        ReportSemError(SemanticError::INIT_SCALAR_WITH_ARRAY,
                       array_initializer -> LeftToken(),
                       array_initializer -> RightToken(),
                       type -> Name());
    }
    else
    {
        for (int i = 0; i < array_initializer -> NumVariableInitializers(); i++)
        {
            AstArrayInitializer *sub_array_initializer = array_initializer -> VariableInitializer(i) -> ArrayInitializerCast();
            TypeSymbol *array_subtype = type -> ArraySubtype();
            if (sub_array_initializer)
                 ProcessArrayInitializer(sub_array_initializer, array_subtype);
            else
            {
                AstExpression *init = (AstExpression *) array_initializer -> VariableInitializer(i);
                ProcessExpressionOrStringConstant(init);

                if (array_subtype != init -> Type())
                {
                    if (CanAssignmentConvert(array_subtype, init))
                        array_initializer -> VariableInitializer(i) = ConvertToType(init, array_subtype);
                    else if (array_subtype -> IsArray() && init -> Type() -> Primitive())
                    {
                        ReportSemError(SemanticError::INIT_ARRAY_WITH_SCALAR,
                                       init -> LeftToken(),
                                       init -> RightToken(),
                                       array_subtype -> Name());
                    }
                    else if (init -> IsConstant() && control.IsSimpleIntegerValueType(init -> Type())
                                                  && control.IsSimpleIntegerValueType(array_subtype))
                    {
                        if (array_subtype == control.byte_type)
                             ReportSemError(SemanticError::INVALID_BYTE_VALUE,
                                            init -> LeftToken(),
                                            init -> RightToken());
                        else if (array_subtype == control.char_type)
                             ReportSemError(SemanticError::INVALID_CHARACTER_VALUE,
                                            init -> LeftToken(),
                                            init -> RightToken());
                        else ReportSemError(SemanticError::INVALID_SHORT_VALUE,
                                            init -> LeftToken(),
                                            init -> RightToken());
                    }
                    else
                    {
                        ReportSemError(SemanticError::INCOMPATIBLE_TYPE_FOR_INITIALIZATION,
                                       init -> LeftToken(),
                                       init -> RightToken(),
                                       array_subtype -> ContainingPackage() -> PackageName(),
                                       array_subtype -> ExternalName(),
                                       init -> Type() -> ContainingPackage() -> PackageName(),
                                       init -> Type() -> ExternalName());
                    }
                }
            }
        }
    }

    return;
}


LiteralValue *Semantic::ComputeFinalValue(AstVariableDeclarator *variable_declarator)
{
    LiteralValue *value = NULL;

    VariableSymbol *variable = variable_declarator -> symbol;
    TypeSymbol *type = (TypeSymbol *) variable -> owner;

    state_stack.Push(type -> semantic_environment);
    if (! error)
        error = new SemanticError(control, source_file_symbol);
    error -> EnteringClone();
    variable_declarator -> pending = true;

    AstExpression *init_expr = (AstExpression *) variable_declarator -> variable_initializer_opt;
    AstExpression *init_clone = (AstExpression *) init_expr -> Clone(compilation_unit -> ast_pool);
    ProcessExpressionOrStringConstant(init_clone);
    if (variable -> Type() != init_clone -> Type() && init_clone -> Type() != control.no_type)
    {
        if (CanAssignmentConvert(variable -> Type(), init_clone))
             init_clone = ConvertToType(init_clone, variable -> Type());
        else init_clone -> value = NULL;
    }
    value = init_clone -> value;

//
// STG:
//        delete init_clone; // destroy the clone
//

    variable_declarator -> pending = false;
    error -> ExitingClone();
    state_stack.Pop();

    return value;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

