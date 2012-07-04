// $Id: modifier.cpp,v 1.21 2002/05/22 08:23:28 ericb Exp $
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

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

//
// Process modifiers of top-level classes.
//
AccessFlags Semantic::ProcessClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else access_flags.SetACC_ABSTRACT();

            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public);
            else access_flags.SetACC_PUBLIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               lex_stream -> NameString(modifier -> modifier_kind_token));
            }
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_TOP_LEVEL_CLASS_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // All classes have the SUPER modifier set, by JVMS2 4.1.
    //
    access_flags.SetACC_SUPER();

    return access_flags;
}


//
// Process modifiers of local classes declared as a statement in a method.
//
AccessFlags Semantic::ProcessLocalClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else access_flags.SetACC_ABSTRACT();

            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_LOCAL_CLASS_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // All classes have the SUPER modifier set, by JVMS2 4.1.
    //
    access_flags.SetACC_SUPER();

    return access_flags;
}


//
// Process modifiers of nested and inner classes occuring in a class.
//
AccessFlags Semantic::ProcessNestedClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else access_flags.SetACC_ABSTRACT();

            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PUBLIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PRIVATE:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PRIVATE();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PROTECTED:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PROTECTED();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else access_flags.SetACC_STATIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_INNER_CLASS_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // All classes have the SUPER modifier set, by JVMS2 4.1.
    //
    access_flags.SetACC_SUPER();

    return access_flags;
}


//
// Process modifiers of nested classes occuring in an interface.
//
AccessFlags Semantic::ProcessStaticNestedClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else access_flags.SetACC_ABSTRACT();

            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public);
                access_flags.SetACC_PUBLIC(); // detect duplicates
            }

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static);
                access_flags.SetACC_STATIC(); // detect duplicates
            }

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_final);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_INNER_CLASS_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // A type that is a member of an interface is implicitly deemed to
    // be static and public, whether it is explicitly marked so or not.
    // All classes have the SUPER modifier set, by JVMS2 4.1.
    //
    access_flags.SetACC_STATIC();
    access_flags.SetACC_PUBLIC();
    access_flags.SetACC_SUPER();

    return access_flags;
}


//
// Process modifiers of top-level interfaces.
//
AccessFlags Semantic::ProcessInterfaceModifiers(AstInterfaceDeclaration *interface_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else
            {
                //
                // JLS 9.1.1.1 claims this is obsolescent, but the Java Spec
                // Report argues that it is useful to mark an interface
                // abstract for ease of converting it back to an abstract
                // class. Therefore, we only give a pedantic warning.
                //
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract);
                access_flags.SetACC_ABSTRACT(); // detect duplicates
            }

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public);
            else access_flags.SetACC_PUBLIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_INTERFACE_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // Every interface is implicitly abstract.
    //
    access_flags.SetACC_INTERFACE();
    access_flags.SetACC_ABSTRACT();

    return access_flags;
}

//
// Process modifiers for interface contained in another interface.
//
AccessFlags Semantic::ProcessStaticNestedInterfaceModifiers(AstInterfaceDeclaration *interface_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else
            {
                //
                // JLS 9.1.1.1 claims this is obsolescent, but the Java Spec
                // Report argues that it is useful to mark an interface
                // abstract for ease of converting it back to an abstract
                // class. Therefore, we only give a pedantic warning.
                //
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract);
                access_flags.SetACC_ABSTRACT(); // detect duplicates
            }

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public);
            else 
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public);
                access_flags.SetACC_PUBLIC(); // detect duplicates
            }

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static);
                access_flags.SetACC_STATIC(); // detect duplicates
            }

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_INTERFACE_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // Interfaces nested in another interface are implicitly public, abstract,
    // and static.
    //
    access_flags.SetACC_INTERFACE();
    access_flags.SetACC_ABSTRACT();
    access_flags.SetACC_STATIC();
    access_flags.SetACC_PUBLIC();

    return access_flags;
}


//
// Process modifiers of interfaces nested in classes.
//
AccessFlags Semantic::ProcessNestedInterfaceModifiers(AstInterfaceDeclaration *interface_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);

        switch (modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else
            {
                //
                // JLS 9.1.1.1 claims this is obsolescent, but the Java Spec
                // Report argues that it is useful to mark an interface
                // abstract for ease of converting it back to an abstract
                // class. Therefore, we only give a pedantic warning.
                //
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract);
                access_flags.SetACC_ABSTRACT(); // detect duplicates
            }

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PUBLIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PRIVATE:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PRIVATE();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PROTECTED:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PROTECTED();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_static);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static);
                access_flags.SetACC_STATIC(); // detect duplicates
            }

            if (access_flags.ACC_STRICTFP() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static,
                               StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();
            break;
        default:
            ReportSemError(SemanticError::INVALID_INTERFACE_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // Interfaces nested in another class are implicitly abstract and static.
    //
    access_flags.SetACC_INTERFACE();
    access_flags.SetACC_ABSTRACT();
    access_flags.SetACC_STATIC();

    return access_flags;
}


//
// Process modifiers of fields declared in classes.
//
AccessFlags Semantic::ProcessFieldModifiers(AstFieldDeclaration *field_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
    {
        AstModifier *modifier = field_declaration -> VariableModifier(i);

        switch (modifier -> kind)
        {
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PUBLIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_final);
                if (access_flags.ACC_TRANSIENT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_transient);
                if (access_flags.ACC_VOLATILE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_volatile);
            }
            break;
        case Ast::PROTECTED:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PROTECTED();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_final);
                if (access_flags.ACC_TRANSIENT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_transient);
                if (access_flags.ACC_VOLATILE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_volatile);
            }
            break;
        case Ast::PRIVATE:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PRIVATE();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_final);
                if (access_flags.ACC_TRANSIENT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_transient);
                if (access_flags.ACC_VOLATILE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_volatile);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else access_flags.SetACC_STATIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_final);
                if (access_flags.ACC_TRANSIENT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_transient);
                if (access_flags.ACC_VOLATILE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_volatile);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();

            if (access_flags.ACC_VOLATILE())
                ReportSemError(SemanticError::VOLATILE_FINAL_FIELD,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_TRANSIENT() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final,
                               StringConstant::US_transient);
            }
            break;
        case Ast::TRANSIENT:
            if (access_flags.ACC_TRANSIENT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_transient);
            else access_flags.SetACC_TRANSIENT();

            if (access_flags.ACC_VOLATILE() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_transient,
                               StringConstant::US_volatile);
            }
            break;
        case Ast::VOLATILE:
            if (access_flags.ACC_VOLATILE())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_volatile);
            else access_flags.SetACC_VOLATILE();

            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::VOLATILE_FINAL_FIELD,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            break;
        default:
            ReportSemError(SemanticError::INVALID_FIELD_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    return access_flags;
}


//
// Process modifiers of local variables.
// TODO: Technically, this could be factored out from the grammar, since
// only final is valid.
//
AccessFlags Semantic::ProcessLocalModifiers(AstLocalVariableDeclarationStatement *local_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < local_declaration -> NumLocalModifiers(); i++)
    {
        AstModifier *modifier = local_declaration -> LocalModifier(i);

        if (modifier -> kind == Ast::FINAL)
        {
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();
        }
        else
            ReportSemError(SemanticError::INVALID_LOCAL_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
    }

    return access_flags;
}


//
// Process modifiers of parameters.
// TODO: Technically, this could be factored out from the grammar, since
// only final is valid.
//
AccessFlags Semantic::ProcessFormalModifiers(AstFormalParameter *parameter_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < parameter_declaration -> NumParameterModifiers(); i++)
    {
        AstModifier *modifier = parameter_declaration -> ParameterModifier(i);

        if (modifier -> kind == Ast::FINAL)
        {
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();
        }
        else
            ReportSemError(SemanticError::INVALID_LOCAL_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
    }

    return access_flags;
}


//
// Process modifiers of methods declared in classes.
//
AccessFlags Semantic::ProcessMethodModifiers(AstMethodDeclaration *method_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
    {
        AstModifier *modifier = method_declaration -> MethodModifier(i);

        switch (modifier -> kind)
        {
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PUBLIC();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_final);
                if (access_flags.ACC_SYNCHRONIZED())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_synchronized);
                if (access_flags.ACC_NATIVE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_native);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PROTECTED:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PROTECTED();

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_ABSTRACT())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_abstract);
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_final);
                if (access_flags.ACC_SYNCHRONIZED())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_synchronized);
                if (access_flags.ACC_NATIVE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_native);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_protected,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::PRIVATE:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PRIVATE();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_private);

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_final);
                if (access_flags.ACC_SYNCHRONIZED())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_synchronized);
                if (access_flags.ACC_NATIVE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_native);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_private,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else access_flags.SetACC_STATIC();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_final);
                if (access_flags.ACC_SYNCHRONIZED())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_synchronized);
                if (access_flags.ACC_NATIVE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_native);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);
            else access_flags.SetACC_STRICTFP();

            if (access_flags.ACC_NATIVE())
                ReportSemError(SemanticError::STRICTFP_NATIVE_METHOD,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_strictfp);

            break;
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else access_flags.SetACC_ABSTRACT();

            if (access_flags.ACC_PRIVATE() || access_flags.ACC_STATIC() ||
                access_flags.ACC_FINAL() || access_flags.ACC_NATIVE() ||
                access_flags.ACC_SYNCHRONIZED() || access_flags.ACC_STRICTFP())
            {
                ReportSemError(SemanticError::BAD_ABSTRACT_METHOD_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else access_flags.SetACC_FINAL();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_SYNCHRONIZED())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_final,
                                   StringConstant::US_synchronized);
                if (access_flags.ACC_NATIVE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_final,
                                   StringConstant::US_native);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_final,
                                   StringConstant::US_strictfp);
            }
            break;
        case Ast::NATIVE:
            if (access_flags.ACC_NATIVE())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_native);
            else access_flags.SetACC_NATIVE();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_native);

            if (access_flags.ACC_STRICTFP())
                ReportSemError(SemanticError::STRICTFP_NATIVE_METHOD,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            
            break;
        case Ast::SYNCHRONIZED:
            if (access_flags.ACC_SYNCHRONIZED())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_synchronized);
            else access_flags.SetACC_SYNCHRONIZED();

            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_synchronized);

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_NATIVE())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_synchronized,
                                   StringConstant::US_native);
                if (access_flags.ACC_STRICTFP())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_synchronized,
                                   StringConstant::US_strictfp);
            }
            break;
        default:
            ReportSemError(SemanticError::INVALID_METHOD_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }
    
    return access_flags;
}


//
// Process modifiers of methods declared in interfaces.
//
AccessFlags Semantic::ProcessInterfaceMethodModifiers(AstMethodDeclaration *method_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
    {
        AstModifier *modifier = method_declaration -> MethodModifier(i);

        switch (modifier -> kind)
        {
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public);
                access_flags.SetACC_PUBLIC(); // detect duplicates
            }

            if (access_flags.ACC_ABSTRACT() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public,
                               StringConstant::US_abstract);
            }
            break;
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_abstract);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_abstract);
                access_flags.SetACC_ABSTRACT(); // detect duplicates
            }
            break;
        default:
            ReportSemError(SemanticError::INVALID_SIGNATURE_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // Every method in an interface is implicitly public and abstract.
    //
    access_flags.SetACC_ABSTRACT();
    access_flags.SetACC_PUBLIC();

    return access_flags;
}


//
// Process modifiers of constructors.
//
AccessFlags Semantic::ProcessConstructorModifiers(AstConstructorDeclaration *constructor_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < constructor_declaration -> NumConstructorModifiers(); i++)
    {
        AstModifier *modifier = constructor_declaration -> ConstructorModifier(i);

        switch (modifier -> kind)
        {
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PUBLIC();
            break;
        case Ast::PROTECTED:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PROTECTED();
            break;
        case Ast::PRIVATE:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() ||
                access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PRIVATE();
            break;
        default:
            ReportSemError(SemanticError::INVALID_CONSTRUCTOR_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    return access_flags;
}


//
// Process modifiers of fields declared in interfaces.
//
AccessFlags Semantic::ProcessInterfaceFieldModifiers(AstFieldDeclaration *field_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
    {
        AstModifier *modifier = field_declaration -> VariableModifier(i);

        switch (modifier -> kind)
        {
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_public);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public);
                access_flags.SetACC_PUBLIC(); // detect duplicates
            }

            if (control.option.pedantic_modifier_order)
            {
                if (access_flags.ACC_STATIC())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_static);
                if (access_flags.ACC_FINAL())
                    ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_public,
                                   StringConstant::US_final);
            }
            break;
        case Ast::STATIC:
            if (access_flags.ACC_STATIC())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_static);
                access_flags.SetACC_STATIC(); // detect duplicates
            }

            if (access_flags.ACC_FINAL() &&
                control.option.pedantic_modifier_order)
            {
                ReportSemError(SemanticError::RECOMMENDED_MODIFIER_ORDER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_static,
                               StringConstant::US_final);
            }
            break;
        case Ast::FINAL:
            if (access_flags.ACC_FINAL())
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token,
                               StringConstant::US_final);
            else
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_MODIFIER,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token,
                                   StringConstant::US_final);
                access_flags.SetACC_FINAL(); // detect duplicates
            }
            break;
        default:
            ReportSemError(SemanticError::INVALID_CONSTANT_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
            break;
        }
    }

    //
    // Every field declaration in an interface is implicitly public,
    // static and final.
    //
    access_flags.SetACC_PUBLIC();
    access_flags.SetACC_STATIC();
    access_flags.SetACC_FINAL();

    return access_flags;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

