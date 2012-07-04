// $Id: modifier.cpp,v 1.26 2002/11/06 00:58:23 ericb Exp $
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

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


//
// Converts an Ast::Kind to an AccessFlags::constant
//
static u2 AstKindToFlag(Ast::Kind kind)
{
    switch (kind)
    {
    case Ast::ABSTRACT: return AccessFlags::ACCESS_ABSTRACT;
    case Ast::FINAL: return AccessFlags::ACCESS_FINAL;
    case Ast::NATIVE: return AccessFlags::ACCESS_NATIVE;
    case Ast::PUBLIC: return AccessFlags::ACCESS_PUBLIC;
    case Ast::PROTECTED: return AccessFlags::ACCESS_PROTECTED;
    case Ast::PRIVATE: return AccessFlags::ACCESS_PRIVATE;
    case Ast::STATIC: return AccessFlags::ACCESS_STATIC;
    case Ast::STRICTFP: return AccessFlags::ACCESS_STRICTFP;
    case Ast::SYNCHRONIZED: return AccessFlags::ACCESS_SYNCHRONIZED;
    case Ast::TRANSIENT: return AccessFlags::ACCESS_TRANSIENT;
    case Ast::VOLATILE: return AccessFlags::ACCESS_VOLATILE;
    default:
        assert(false && "Ast::Kind not modifier");
    }
    return 0;
}


//
// Processs a modifier, possibly modifying the AccessFlags object it modifies.
//
// Diagnostics are emitted in the following cases:
// . Invalid modifiers based on context.
// . Duplicated mutually exclusive modifiers (public, protected, private).
// . Duplicated general modifiers.
// . Modifier combinations not legal (such as final abstract).
// . Modifiers not in the recommended order (pedantic_modifier_order only).
// . Explicit specification of implicit modifier (pedantic only).
//
void Semantic::ProcessAccessFlag(AccessFlags& access_flags,
                                 LexStream::TokenIndex token_index,
                                 Ast::Kind kind,
                                 const wchar_t* declaration_kind_name,
                                 u2 valid_flags, u2 implicit_flags)
{
    u2 flag = AstKindToFlag(kind);
    if ((flag & valid_flags) == 0)
    {
        ReportSemError(SemanticError::INVALID_MODIFIER, token_index,
                       lex_stream -> NameString(token_index),
                       declaration_kind_name);
    }
    else if ((flag & AccessFlags::ACCESS_ACCESS) &&
             (access_flags.Flags() & AccessFlags::ACCESS_ACCESS))
    {
        ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER, token_index);
    }
    else if (access_flags.IsSet(flag))
    {
        ReportSemError(SemanticError::DUPLICATE_MODIFIER, token_index,
                       lex_stream -> NameString(token_index));
    }
    else
    {
        // We have a valid flag if it is alone.
        if (control.option.pedantic)
        {
            if ((flag & implicit_flags) != 0)
            {
                ReportSemError(SemanticError::REDUNDANT_MODIFIER, token_index,
                               lex_stream -> NameString(token_index),
                               declaration_kind_name);
            }
            if (! access_flags.RecommendedOrder(flag))
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               token_index,
                               lex_stream -> NameString(token_index));
            }
        }
        access_flags.SetFlags(flag);
        if (access_flags.ACC_FINAL())
        {
            if (access_flags.ACC_VOLATILE())
            {
                // We know it's a field because of volatile.
                ReportSemError(SemanticError::VOLATILE_FINAL_FIELD,
                               token_index,
                               lex_stream -> NameString(token_index));
                access_flags.ResetFlags(flag);
            }
            else if (access_flags.ACC_ABSTRACT())
            {
                ReportSemError(SemanticError::FINAL_ABSTRACT_ENTITY,
                               token_index, declaration_kind_name);
                access_flags.ResetFlags(flag);
            }
        }
        else if (access_flags.ACC_ABSTRACT() &&
                 (valid_flags & AccessFlags::ACCESS_NATIVE))
        {
            //
            // Classes can be private static abstract strictfp, but methods
            // cannot. Hence, we checked that we are dealing with a method by
            // looking for NATIVE in the legal flags.
            //
            if (access_flags.ACC_SYNCHRONIZED() || access_flags.ACC_NATIVE() ||
                access_flags.ACC_PRIVATE() || access_flags.ACC_STATIC() ||
                access_flags.ACC_STRICTFP())
            {
                ReportSemError(SemanticError::BAD_ABSTRACT_METHOD_MODIFIER,
                               token_index);
                access_flags.ResetFlags(flag);
            }
        }
        else if (access_flags.ACC_STRICTFP() && access_flags.ACC_NATIVE())
        {
            ReportSemError(SemanticError::STRICTFP_NATIVE_METHOD, token_index);
            access_flags.ResetFlags(flag);
        }
    }
}

//
// Process modifiers of top-level classes.
//
AccessFlags Semantic::ProcessClassModifiers(AstClassDeclaration* class_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_FINAL | AccessFlags::ACCESS_PUBLIC |
        AccessFlags::ACCESS_STRICTFP;
    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier* modifier = class_declaration -> ClassModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a top-level class",
                          valid_flags);
    }
    return access_flags;
}


//
// Process modifiers of local classes declared as a statement in a method.
//
AccessFlags Semantic::ProcessLocalClassModifiers(AstClassDeclaration* class_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_FINAL | AccessFlags::ACCESS_STRICTFP;
    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a local inner class",
                          valid_flags);
    }
    return access_flags;
}


//
// Process modifiers of nested and inner classes occuring in a class.
//
AccessFlags Semantic::ProcessNestedClassModifiers(AstClassDeclaration* class_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_FINAL | AccessFlags::ACCESS_ACCESS |
        AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_STRICTFP;
    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a class's member class",
                          valid_flags);
    }
    return access_flags;
}


//
// Process modifiers of nested classes occuring in an interface.
//
AccessFlags Semantic::ProcessStaticNestedClassModifiers(AstClassDeclaration* class_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_FINAL | AccessFlags::ACCESS_PUBLIC |
        AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_STRICTFP;
    const u2 implicit_flags = AccessFlags::ACCESS_STATIC |
        AccessFlags::ACCESS_PUBLIC;
    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"an interface's member class",
                          valid_flags, implicit_flags);
    }
    access_flags.SetFlags(implicit_flags);
    return access_flags;
}


//
// Process modifiers of top-level interfaces.
//
AccessFlags Semantic::ProcessInterfaceModifiers(AstInterfaceDeclaration* interface_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_PUBLIC | AccessFlags::ACCESS_STRICTFP;
    const u2 implicit_flags = AccessFlags::ACCESS_INTERFACE |
        AccessFlags::ACCESS_ABSTRACT;
    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a top-level interface",
                          valid_flags, implicit_flags);
    }
    access_flags.SetFlags(implicit_flags);
    return access_flags;
}

//
// Process modifiers for interface contained in another interface.
//
AccessFlags Semantic::ProcessStaticNestedInterfaceModifiers(AstInterfaceDeclaration* interface_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_PUBLIC | AccessFlags::ACCESS_STATIC |
        AccessFlags::ACCESS_STRICTFP;
    const u2 implicit_flags = AccessFlags::ACCESS_INTERFACE |
        AccessFlags::ACCESS_ABSTRACT | AccessFlags::ACCESS_STATIC |
        AccessFlags::ACCESS_PUBLIC;
    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"an interface's member interface",
                          valid_flags, implicit_flags);
    }
    access_flags.SetFlags(implicit_flags);
    return access_flags;
}


//
// Process modifiers of interfaces nested in classes.
//
AccessFlags Semantic::ProcessNestedInterfaceModifiers(AstInterfaceDeclaration* interface_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ABSTRACT |
        AccessFlags::ACCESS_ACCESS | AccessFlags::ACCESS_STATIC |
        AccessFlags::ACCESS_STRICTFP;
    const u2 implicit_flags = AccessFlags::ACCESS_INTERFACE |
        AccessFlags::ACCESS_ABSTRACT | AccessFlags::ACCESS_STATIC;
    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a class's member interface",
                          valid_flags, implicit_flags);
    }
    access_flags.SetFlags(implicit_flags);
    return access_flags;
}


//
// Process modifiers of fields declared in classes.
//
AccessFlags Semantic::ProcessFieldModifiers(AstFieldDeclaration* field_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ACCESS |
        AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_FINAL |
        AccessFlags::ACCESS_TRANSIENT | AccessFlags::ACCESS_VOLATILE;
    for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
    {
        AstModifier *modifier = field_declaration -> VariableModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a class's member field",
                          valid_flags);
    }
    return access_flags;
}


//
// Process modifiers of local variables.
// Note: Technically, this could be factored out from the grammar, since
// only final is valid.
//
AccessFlags Semantic::ProcessLocalModifiers(AstLocalVariableDeclarationStatement* local_declaration)
{
    AccessFlags access_flags;
    for (int i = 0; i < local_declaration -> NumLocalModifiers(); i++)
    {
        AstModifier *modifier = local_declaration -> LocalModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a local variable",
                          AccessFlags::ACCESS_FINAL);
    }
    return access_flags;
}


//
// Process modifiers of parameters.
// Note: Technically, this could be factored out from the grammar, since
// only final is valid.
//
AccessFlags Semantic::ProcessFormalModifiers(AstFormalParameter* parameter_declaration)
{
    AccessFlags access_flags;
    for (int i = 0; i < parameter_declaration -> NumParameterModifiers(); i++)
    {
        AstModifier *modifier = parameter_declaration -> ParameterModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a formal parameter",
                          AccessFlags::ACCESS_FINAL);
    }
    return access_flags;
}


//
// Process modifiers of methods declared in classes.
//
AccessFlags Semantic::ProcessMethodModifiers(AstMethodDeclaration* method_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_ACCESS |
        AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_STRICTFP |
        AccessFlags::ACCESS_ABSTRACT | AccessFlags::ACCESS_FINAL |
        AccessFlags::ACCESS_NATIVE | AccessFlags::ACCESS_SYNCHRONIZED;
    for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
    {
        AstModifier *modifier = method_declaration -> MethodModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a class's member method",
                          valid_flags);
    }
    return access_flags;
}


//
// Process modifiers of methods declared in interfaces.
//
AccessFlags Semantic::ProcessInterfaceMethodModifiers(AstMethodDeclaration* method_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_PUBLIC |
        AccessFlags::ACCESS_ABSTRACT;
    for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
    {
        AstModifier *modifier = method_declaration -> MethodModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"an interface's member method",
                          valid_flags, valid_flags);
    }
    access_flags.SetFlags(valid_flags);
    return access_flags;
}


//
// Process modifiers of constructors.
//
AccessFlags Semantic::ProcessConstructorModifiers(AstConstructorDeclaration* constructor_declaration)
{
    AccessFlags access_flags;
    for (int i = 0; i < constructor_declaration -> NumConstructorModifiers(); i++)
    {
        AstModifier *modifier = constructor_declaration -> ConstructorModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"a constructor",
                          AccessFlags::ACCESS_ACCESS);
    }
    return access_flags;
}


//
// Process modifiers of fields declared in interfaces.
//
AccessFlags Semantic::ProcessInterfaceFieldModifiers(AstFieldDeclaration* field_declaration)
{
    AccessFlags access_flags;
    const u2 valid_flags = AccessFlags::ACCESS_PUBLIC |
        AccessFlags::ACCESS_STATIC | AccessFlags::ACCESS_FINAL;
    for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
    {
        AstModifier *modifier = field_declaration -> VariableModifier(i);
        ProcessAccessFlag(access_flags, modifier -> modifier_kind_token,
                          modifier -> kind, L"an interface's member field",
                          valid_flags, valid_flags);
    }
    access_flags.SetFlags(valid_flags);
    return access_flags;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

