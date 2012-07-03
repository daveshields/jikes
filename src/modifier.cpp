// $Id: modifier.cpp,v 1.12 2000/07/25 11:32:33 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"
#include "semantic.h"

#ifdef	HAVE_NAMESPACES
using namespace Jikes;
#endif

AccessFlags Semantic::ProcessClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch(modifier -> kind)
        {
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_ABSTRACT();

                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_FINAL();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PUBLIC();
                 break;
            case Ast::STRICTFP:
                 if (access_flags.ACC_STRICTFP())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
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

    return access_flags;
}


AccessFlags Semantic::ProcessLocalClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch(modifier -> kind)
        {
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_ABSTRACT();

                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_FINAL();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::STRICTFP:
                 if (access_flags.ACC_STRICTFP())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
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

    return access_flags;
}


AccessFlags Semantic::ProcessNestedClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch(modifier -> kind)
        {
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_ABSTRACT();

                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_FINAL();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PUBLIC();
                 break;
            case Ast::PRIVATE:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PRIVATE();
                 break;
            case Ast::PROTECTED:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PROTECTED();
                 break;
            case Ast::STATIC:
                 if (access_flags.ACC_STATIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_STATIC();
                 break;
            case Ast::STRICTFP:
                 if (access_flags.ACC_STRICTFP())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
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

    return access_flags;
}


AccessFlags Semantic::ProcessStaticNestedClassModifiers(AstClassDeclaration *class_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < class_declaration -> NumClassModifiers(); i++)
    {
        AstModifier *modifier = class_declaration -> ClassModifier(i);

        switch(modifier -> kind)
        {
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_ABSTRACT();

                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_FINAL();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::FINAL_ABSTRACT_CLASS,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_PUBLIC,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_PUBLIC();
                 }
                 break;
            case Ast::STATIC:
                 if (access_flags.ACC_STATIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_STATIC,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_STATIC();
                 }
                 break;
            case Ast::STRICTFP:
                 if (access_flags.ACC_STRICTFP())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
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
    //
    access_flags.SetACC_STATIC();
    access_flags.SetACC_PUBLIC();

    return access_flags;
}


AccessFlags Semantic::ProcessInterfaceModifiers(AstInterfaceDeclaration *interface_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);

        switch(modifier -> kind)
        {
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     ReportSemError(SemanticError::OBSOLESCENT_ABSTRACT,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                     access_flags.SetACC_ABSTRACT();
                 }
                 break;
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PUBLIC();
                 break;
            case Ast::STRICTFP:
                 if (access_flags.ACC_STRICTFP())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
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

    access_flags.SetACC_INTERFACE();
    access_flags.SetACC_ABSTRACT(); // every interface is implicitly abstract

    return access_flags;
}

/**
 * Process modifieres for interface contained in another interface.
 */
AccessFlags Semantic::ProcessStaticNestedInterfaceModifiers(AstInterfaceDeclaration *interface_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);

        switch(modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else
            {
                ReportSemError(SemanticError::OBSOLESCENT_ABSTRACT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            break;
        case Ast::PUBLIC:
            if(access_flags.ACC_PUBLIC())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else 
            {
                if (control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_PUBLIC,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token);
                access_flags.SetACC_PUBLIC();
            }
            break;
        case Ast::STATIC: // TODO: Need to confirm that this is valid !!!
            if(access_flags.ACC_STATIC())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else 
            {
                if(control.option.pedantic)
                    ReportSemError(SemanticError::REDUNDANT_STATIC,
                                   modifier -> modifier_kind_token,
                                   modifier -> modifier_kind_token);
                access_flags.SetACC_STATIC();
            }
            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
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

    access_flags.SetACC_INTERFACE();
    access_flags.SetACC_ABSTRACT(); // every interface is implicitly abstract
    access_flags.SetACC_STATIC();   // every inner interface is implicitly static
    access_flags.SetACC_PUBLIC();   // interface contained interfaces are implicitly public

    return access_flags;
}

AccessFlags Semantic::ProcessNestedInterfaceModifiers(AstInterfaceDeclaration *interface_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < interface_declaration -> NumInterfaceModifiers(); i++)
    {
        AstModifier *modifier = interface_declaration -> InterfaceModifier(i);

        switch(modifier -> kind)
        {
        case Ast::ABSTRACT:
            if (access_flags.ACC_ABSTRACT())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else
            {
                ReportSemError(SemanticError::OBSOLESCENT_ABSTRACT,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            break;
        case Ast::PUBLIC:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PUBLIC();
            break;
        case Ast::PRIVATE:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PRIVATE();
            break;
        case Ast::PROTECTED:
            if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
            {
                ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_PROTECTED();
            break;
        case Ast::STATIC: // TODO: Need to confirm that this is valid !!!
            if (access_flags.ACC_STATIC())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else if (control.option.pedantic)
            {
                ReportSemError(SemanticError::REDUNDANT_STATIC,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
                access_flags.SetACC_STATIC();
            }

            break;
        case Ast::STRICTFP:
            if (access_flags.ACC_STRICTFP())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
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

    access_flags.SetACC_INTERFACE();
    access_flags.SetACC_ABSTRACT(); // every interface is implicitly abstract
    access_flags.SetACC_STATIC();   // every inner interface is implicitly static

    return access_flags;
}


AccessFlags Semantic::ProcessFieldModifiers(AstFieldDeclaration *field_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
    {
        AstModifier *modifier = field_declaration -> VariableModifier(i);

        switch(modifier -> kind)
        {
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PUBLIC();
                 break;
            case Ast::PROTECTED:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PROTECTED();
                 break;
            case Ast::PRIVATE:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PRIVATE();
                 break;
            case Ast::STATIC:
                 if (access_flags.ACC_STATIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_STATIC();
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_FINAL();

                 if (access_flags.ACC_VOLATILE())
                 {
                     ReportSemError(SemanticError::FINAL_VOLATILE,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::TRANSIENT:
                 if (access_flags.ACC_TRANSIENT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_TRANSIENT();
                 break;
            case Ast::VOLATILE:
                 if (access_flags.ACC_VOLATILE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_VOLATILE();

                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::VOLATILE_FINAL,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
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


AccessFlags Semantic::ProcessLocalModifiers(AstLocalVariableDeclarationStatement *local_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < local_declaration -> NumLocalModifiers(); i++)
    {
        AstModifier *modifier = local_declaration -> LocalModifier(i);

        if (modifier -> kind == Ast::FINAL)
        {
            if (access_flags.ACC_FINAL())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_FINAL();
        }
        else
        {
            ReportSemError(SemanticError::INVALID_LOCAL_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
        }
    }

    return access_flags;
}


AccessFlags Semantic::ProcessFormalModifiers(AstFormalParameter *parameter_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < parameter_declaration -> NumParameterModifiers(); i++)
    {
        AstModifier *modifier = parameter_declaration -> ParameterModifier(i);

        if (modifier -> kind == Ast::FINAL)
        {
            if (access_flags.ACC_FINAL())
            {
                ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                               modifier -> modifier_kind_token,
                               modifier -> modifier_kind_token);
            }
            else access_flags.SetACC_FINAL();
        }
        else
        {
            ReportSemError(SemanticError::INVALID_LOCAL_MODIFIER,
                           modifier -> modifier_kind_token,
                           modifier -> modifier_kind_token,
                           lex_stream -> NameString(modifier -> modifier_kind_token));
        }
    }

    return access_flags;
}


AccessFlags Semantic::ProcessMethodModifiers(AstMethodDeclaration *method_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
    {
        AstModifier *modifier = method_declaration -> MethodModifier(i);

        switch(modifier -> kind)
        {
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PUBLIC();
                 break;
            case Ast::PROTECTED:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PROTECTED();
                 break;
            case Ast::PRIVATE:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PRIVATE();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token,
                                    lex_stream -> NameString(modifier -> modifier_kind_token));
                 }
                 break;
            case Ast::STATIC:
                 if (access_flags.ACC_STATIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_STATIC();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token,
                                    lex_stream -> NameString(modifier -> modifier_kind_token));
                 }
                 break;
            case Ast::STRICTFP:
                 if (access_flags.ACC_STRICTFP())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_STRICTFP();
                 break;
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_ABSTRACT();

                 if (access_flags.ACC_PRIVATE() || access_flags.ACC_STATIC() ||
                     access_flags.ACC_FINAL()   || access_flags.ACC_NATIVE() || access_flags.ACC_SYNCHRONIZED())
                 {
                     ReportSemError(SemanticError::BAD_ABSTRACT_METHOD_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_FINAL();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token,
                                    lex_stream -> NameString(modifier -> modifier_kind_token));
                 }
                 break;
            case Ast::NATIVE:
                 if (access_flags.ACC_NATIVE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_NATIVE();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token,
                                    lex_stream -> NameString(modifier -> modifier_kind_token));
                 }
                 break;
            case Ast::SYNCHRONIZED:
                 if (access_flags.ACC_SYNCHRONIZED())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_SYNCHRONIZED();

                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::ABSTRACT_METHOD_MODIFIER_CONFLICT,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token,
                                    lex_stream -> NameString(modifier -> modifier_kind_token));
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


AccessFlags Semantic::ProcessInterfaceMethodModifiers(AstMethodDeclaration *method_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
    {
        AstModifier *modifier = method_declaration -> MethodModifier(i);

        switch(modifier -> kind)
        {
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_PUBLIC,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_PUBLIC();
                 }
                 break;
            case Ast::ABSTRACT:
                 if (access_flags.ACC_ABSTRACT())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_ABSTRACT,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_ABSTRACT();
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
    // Every method declaration in the body of an interface is implicitly "public" and "abstract"
    //
    access_flags.SetACC_ABSTRACT();
    access_flags.SetACC_PUBLIC();

    return access_flags;
}


AccessFlags Semantic::ProcessConstructorModifiers(AstConstructorDeclaration *constructor_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < constructor_declaration -> NumConstructorModifiers(); i++)
    {
        AstModifier *modifier = constructor_declaration -> ConstructorModifier(i);

        switch(modifier -> kind)
        {
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PUBLIC();
                 break;
            case Ast::PROTECTED:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
                 {
                     ReportSemError(SemanticError::DUPLICATE_ACCESS_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else access_flags.SetACC_PROTECTED();
                 break;
            case Ast::PRIVATE:
                 if (access_flags.ACC_PUBLIC() || access_flags.ACC_PROTECTED() || access_flags.ACC_PRIVATE())
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


AccessFlags Semantic::ProcessConstantModifiers(AstFieldDeclaration *field_declaration)
{
    AccessFlags access_flags;

    for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
    {
        AstModifier *modifier = field_declaration -> VariableModifier(i);

        switch(modifier -> kind)
        {
            case Ast::PUBLIC:
                 if (access_flags.ACC_PUBLIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_PUBLIC,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_PUBLIC();
                 }
                 break;
            case Ast::STATIC:
                 if (access_flags.ACC_STATIC())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_STATIC,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_STATIC();
                 }
                 break;
            case Ast::FINAL:
                 if (access_flags.ACC_FINAL())
                 {
                     ReportSemError(SemanticError::DUPLICATE_MODIFIER,
                                    modifier -> modifier_kind_token,
                                    modifier -> modifier_kind_token);
                 }
                 else
                 {
                     if (control.option.pedantic)
                         ReportSemError(SemanticError::REDUNDANT_FINAL,
                                        modifier -> modifier_kind_token,
                                        modifier -> modifier_kind_token);
                     access_flags.SetACC_FINAL();
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
    // Every constant (field) declaration in the body of an interface is implicitly "public", "static" and "final"
    //
    access_flags.SetACC_PUBLIC();
    access_flags.SetACC_STATIC();
    access_flags.SetACC_FINAL();

    return access_flags;
}
