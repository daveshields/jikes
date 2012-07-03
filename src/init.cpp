// $Id: init.cpp,v 1.7 1999/09/13 14:21:16 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include "semantic.h"
#include "control.h"

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

        if (symbol -> Type() != init -> Type() && init -> Type() != control.no_type)
        {
            if (CanAssignmentConvert(symbol -> Type(), init))
            {
                init = ConvertToType(init, symbol -> Type());
                variable_declarator -> variable_initializer_opt = init;
            }
            else if (init -> IsConstant() && control.IsSimpleIntegerValueType(init -> Type())
                                          && control.IsSimpleIntegerValueType(symbol -> Type()))
            {
                if (symbol -> Type() == control.byte_type)
                     ReportSemError(SemanticError::INVALID_BYTE_VALUE,
                                    init -> LeftToken(),
                                    init -> RightToken());
                else if (symbol -> Type() == control.char_type)
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
                               symbol -> Type() -> ContainingPackage() -> PackageName(),
                               symbol -> Type() -> ExternalName(),
                               init -> Type() -> ContainingPackage() -> PackageName(),
                               init -> Type() -> ExternalName());
            }
        }

        if (symbol -> ACC_FINAL() && init -> IsConstant())
            symbol -> initial_value = init -> value;
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
