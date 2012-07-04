// $Id: modifier.cpp,v 1.30 2002/12/07 00:27:20 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"
#include "semantic.h"
#include "option.h"
#include "javasym.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


//
// Process modifiers, returning the AccessFlags they represent.
//
// Diagnostics are emitted in the following cases:
// . Invalid modifiers based on context.
// . Duplicated mutually exclusive modifiers (public, protected, private).
// . Duplicated general modifiers.
// . Modifier combinations not legal (such as final abstract).
// . Modifiers not in the recommended order (controlled by +Pmodifier-order).
// . Explicit specification of implicit modifier (controlled by
//   +Predundant-modifiers).
//
AccessFlags Semantic::ProcessModifiers(AstModifiers* modifiers,
                                       const wchar_t* declaration_kind_name,
                                       u2 valid_flags, u2 implicit_flags)
{
    if (! modifiers)
        return AccessFlags(implicit_flags);
    AccessFlags access_flags;
    for (LexStream::TokenIndex index = modifiers -> left_modifier_token;
         index <= modifiers -> right_modifier_token; index++)
    {
        u2 flag;
        switch (lex_stream -> Kind(index))
        {
        case TK_abstract: flag = AccessFlags::ACCESS_ABSTRACT; break;
        case TK_final: flag = AccessFlags::ACCESS_FINAL; break;
        case TK_native: flag = AccessFlags::ACCESS_NATIVE; break;
        case TK_public: flag = AccessFlags::ACCESS_PUBLIC; break;
        case TK_protected: flag = AccessFlags::ACCESS_PROTECTED; break;
        case TK_private: flag = AccessFlags::ACCESS_PRIVATE; break;
        case TK_static: flag = AccessFlags::ACCESS_STATIC; break;
        case TK_strictfp: flag = AccessFlags::ACCESS_STRICTFP; break;
        case TK_synchronized: flag = AccessFlags::ACCESS_SYNCHRONIZED; break;
        case TK_transient: flag = AccessFlags::ACCESS_TRANSIENT; break;
        case TK_volatile: flag = AccessFlags::ACCESS_VOLATILE; break;
        default:
            assert(false && "token was not modifier");
        }
        if ((flag & valid_flags) == 0)
        {
            ReportSemError(SemanticError::INVALID_MODIFIER, index,
                           lex_stream -> NameString(index),
                           declaration_kind_name);
        }
        else if ((flag & AccessFlags::ACCESS_ACCESS) &&
                 (access_flags.Flags() & AccessFlags::ACCESS_ACCESS))
        {
            ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER, index);
        }
        else if (access_flags.IsSet(flag))
        {
            ReportSemError(SemanticError::DUPLICATE_MODIFIER, index,
                       lex_stream -> NameString(index));
        }
        else
        {
            // We have a valid flag if it is alone.
            if ((flag & implicit_flags) != 0)
            {
                ReportSemError(SemanticError::REDUNDANT_MODIFIER, index,
                               lex_stream -> NameString(index),
                               declaration_kind_name);
            }
            if (! access_flags.RecommendedOrder(flag))
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               index, lex_stream -> NameString(index));
            }
            access_flags.SetFlags(flag);
            if (access_flags.ACC_FINAL())
            {
                if (access_flags.ACC_VOLATILE())
                {
                    // We know it's a field because of volatile.
                    ReportSemError(SemanticError::VOLATILE_FINAL_FIELD,
                                   index, lex_stream -> NameString(index));
                    access_flags.ResetFlags(flag);
                }
                else if (access_flags.ACC_ABSTRACT())
                {
                    ReportSemError(SemanticError::FINAL_ABSTRACT_ENTITY,
                                   index, declaration_kind_name);
                    access_flags.ResetFlags(flag);
                }
            }
            else if (access_flags.ACC_ABSTRACT() &&
                     (valid_flags & AccessFlags::ACCESS_NATIVE))
            {
                //
                // Classes can be private static abstract strictfp, but methods
                // cannot. Hence, we checked that we are dealing with a method
                // by looking for NATIVE in the legal flags.
                //
                if (access_flags.ACC_SYNCHRONIZED() ||
                    access_flags.ACC_NATIVE() || access_flags.ACC_PRIVATE() ||
                    access_flags.ACC_STATIC() || access_flags.ACC_STRICTFP())
                {
                    ReportSemError(SemanticError::BAD_ABSTRACT_METHOD_MODIFIER,
                                   index);
                    access_flags.ResetFlags(flag);
                }
            }
            else if (access_flags.ACC_STRICTFP() && access_flags.ACC_NATIVE())
            {
                ReportSemError(SemanticError::STRICTFP_NATIVE_METHOD, index);
                access_flags.ResetFlags(flag);
            }
        }
    }
    access_flags.SetFlags(implicit_flags);
    return access_flags;
}

//
// Process modifiers of top-level types.
//
AccessFlags Semantic::ProcessTopLevelTypeModifiers(AstDeclaredType* declaration)
{
    AstClassDeclaration* class_declaration =
        declaration -> ClassDeclarationCast();
    if (class_declaration)
        return ProcessModifiers(class_declaration -> modifiers_opt,
                                L"a top-level class",
                                (AccessFlags::ACCESS_ABSTRACT |
                                 AccessFlags::ACCESS_FINAL |
                                 AccessFlags::ACCESS_PUBLIC |
                                 AccessFlags::ACCESS_STRICTFP));
    AstInterfaceDeclaration* interface_declaration =
        declaration -> InterfaceDeclarationCast();
    assert(interface_declaration);
    return ProcessModifiers(interface_declaration -> modifiers_opt,
                            L"a top-level interface",
                            (AccessFlags::ACCESS_ABSTRACT |
                             AccessFlags::ACCESS_PUBLIC |
                             AccessFlags::ACCESS_STRICTFP),
                            (AccessFlags::ACCESS_INTERFACE |
                             AccessFlags::ACCESS_ABSTRACT));
}


//
// Process modifiers of local classes declared as a statement in a method.
//
AccessFlags Semantic::ProcessLocalClassModifiers(AstClassDeclaration* class_declaration)
{
    return ProcessModifiers(class_declaration -> modifiers_opt,
                            L"a local inner class",
                            (AccessFlags::ACCESS_ABSTRACT |
                             AccessFlags::ACCESS_FINAL |
                             AccessFlags::ACCESS_STRICTFP));
}


//
// Process modifiers of nested and inner types.
//
AccessFlags Semantic::ProcessNestedTypeModifiers(TypeSymbol* containing_type,
                                                 AstDeclaredType* declaration)
{
    u2 valid_flags;
    u2 implicit_flags = 0;
    const wchar_t* context;
    AstClassDeclaration* class_declaration =
        declaration -> ClassDeclarationCast();
    if (class_declaration)
    {
        if (containing_type -> ACC_INTERFACE())
        {
            valid_flags = AccessFlags::ACCESS_ABSTRACT |
                AccessFlags::ACCESS_FINAL | AccessFlags::ACCESS_PUBLIC |
                AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_STRICTFP;
            implicit_flags = AccessFlags::ACCESS_STATIC |
                AccessFlags::ACCESS_PUBLIC;
        }
        else
        {
            valid_flags = AccessFlags::ACCESS_ABSTRACT |
                AccessFlags::ACCESS_FINAL | AccessFlags::ACCESS_ACCESS |
                AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_STRICTFP;
        }
        context = L"a member class";
    }
    else
    {
        AstInterfaceDeclaration* interface_declaration =
            declaration -> InterfaceDeclarationCast();
        assert(interface_declaration);
        if (containing_type -> ACC_INTERFACE())
        {
            valid_flags = AccessFlags::ACCESS_ABSTRACT |
                AccessFlags::ACCESS_PUBLIC | AccessFlags::ACCESS_STATIC |
                AccessFlags::ACCESS_STRICTFP;
            implicit_flags = AccessFlags::ACCESS_INTERFACE |
                AccessFlags::ACCESS_ABSTRACT | AccessFlags::ACCESS_STATIC |
                AccessFlags::ACCESS_PUBLIC;
        }
        else
        {
            valid_flags = AccessFlags::ACCESS_ABSTRACT |
                AccessFlags::ACCESS_ACCESS | AccessFlags::ACCESS_STATIC |
                AccessFlags::ACCESS_STRICTFP;
            implicit_flags = AccessFlags::ACCESS_INTERFACE |
                AccessFlags::ACCESS_ABSTRACT | AccessFlags::ACCESS_STATIC;
        }
        context = L"a member interface";
    }
    return ProcessModifiers(declaration -> modifiers_opt, context,
                            valid_flags, implicit_flags);
}


//
// Process modifiers of fields declared in classes.
//
AccessFlags Semantic::ProcessFieldModifiers(AstFieldDeclaration* field_declaration)
{
    return ProcessModifiers(field_declaration -> modifiers_opt,
                            L"a class's member field",
                            (AccessFlags::ACCESS_ACCESS |
                             AccessFlags::ACCESS_STATIC |
                             AccessFlags::ACCESS_FINAL |
                             AccessFlags::ACCESS_TRANSIENT |
                             AccessFlags::ACCESS_VOLATILE));
}


//
// Process modifiers of local variables.
// Note: Technically, this could be factored out from the grammar, since
// only final is valid.
//
AccessFlags Semantic::ProcessLocalModifiers(AstLocalVariableDeclarationStatement* local_declaration)
{
    return ProcessModifiers(local_declaration -> modifiers_opt,
                            L"a local variable", AccessFlags::ACCESS_FINAL);
}


//
// Process modifiers of parameters.
// Note: Technically, this could be factored out from the grammar, since
// only final is valid.
//
AccessFlags Semantic::ProcessFormalModifiers(AstFormalParameter* parameter_declaration)
{
    return ProcessModifiers(parameter_declaration -> modifiers_opt,
                            L"a formal parameter", AccessFlags::ACCESS_FINAL);
}


//
// Process modifiers of methods declared in classes.
//
AccessFlags Semantic::ProcessMethodModifiers(AstMethodDeclaration* method_declaration)
{
    return ProcessModifiers(method_declaration -> modifiers_opt,
                            L"a class's member method",
                            (AccessFlags::ACCESS_ACCESS |
                             AccessFlags::ACCESS_STATIC |
                             AccessFlags::ACCESS_STRICTFP |
                             AccessFlags::ACCESS_ABSTRACT |
                             AccessFlags::ACCESS_FINAL |
                             AccessFlags::ACCESS_NATIVE |
                             AccessFlags::ACCESS_SYNCHRONIZED));
}


//
// Process modifiers of methods declared in interfaces.
//
AccessFlags Semantic::ProcessInterfaceMethodModifiers(AstMethodDeclaration* method_declaration)
{
    return ProcessModifiers(method_declaration -> modifiers_opt,
                            L"an interface's member method",
                            (AccessFlags::ACCESS_PUBLIC |
                             AccessFlags::ACCESS_ABSTRACT),
                            (AccessFlags::ACCESS_PUBLIC |
                             AccessFlags::ACCESS_ABSTRACT));
}


//
// Process modifiers of constructors.
//
AccessFlags Semantic::ProcessConstructorModifiers(AstConstructorDeclaration* constructor_declaration)
{
    return ProcessModifiers(constructor_declaration -> modifiers_opt,
                            L"a constructor", AccessFlags::ACCESS_ACCESS);
}


//
// Process modifiers of fields declared in interfaces.
//
AccessFlags Semantic::ProcessInterfaceFieldModifiers(AstFieldDeclaration* field_declaration)
{
    return ProcessModifiers(field_declaration -> modifiers_opt,
                            L"an interface's member field",
                            (AccessFlags::ACCESS_PUBLIC |
                             AccessFlags::ACCESS_STATIC |
                             AccessFlags::ACCESS_FINAL),
                            (AccessFlags::ACCESS_PUBLIC |
                             AccessFlags::ACCESS_STATIC |
                             AccessFlags::ACCESS_FINAL));
}

//
// Process static and instance initializer modifiers.
//
AccessFlags Semantic::ProcessInitializerModifiers(AstInitializerDeclaration* initializer)
{
    return ProcessModifiers(initializer -> modifiers_opt,
                            L"an initializer block",
                            AccessFlags::ACCESS_STATIC);
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

