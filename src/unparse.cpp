// $Id: unparse.cpp,v 1.10 2000/07/26 08:28:50 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "platform.h"
#include "ast.h"

//FIXME: need to move to platform.h
#include <iostream.h>
#include <fstream.h>

#ifdef	HAVE_NAMESPACES
using namespace Jikes;
#endif

#ifdef JIKES_DEBUG
    bool Ast::debug_unparse = false;
#endif


#ifdef JIKES_DEBUG
// Special top-level form
void AstCompilationUnit::Unparse(LexStream& lex_stream, char *directory)
{
    char *in_file_name = lex_stream.FileName();
    // char *suffix = ".unparse";
    char *suffix = "";
    char *out_file_name = ::strcat3(directory, in_file_name, suffix);
    // Create the directory if necessary
    for (int i=strlen(out_file_name); i>=0; i--) {
       if (out_file_name[i] == U_SLASH) {
           out_file_name[i] = U_NULL;
           if (! ::SystemIsDirectory(out_file_name))
           {
               Ostream() << "making directory " << out_file_name << "\n";
               ::SystemMkdirhier(out_file_name);
           }
           out_file_name[i] = U_SLASH;
           break;
       }
    }
    ofstream os_base(out_file_name);
    if (!os_base)
    {
        Ostream() << "Cannot open output file " << out_file_name << "\n";
        abort();
    }
    Ostream os(&os_base);
    this -> Unparse(os, lex_stream);
    delete[] out_file_name;
}

void Ast::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (debug_unparse)
        os << "/*Ast:#" << this-> id << "*/";
    os << "***** TO DO *****";
    os << "#" << this -> id << " (Ast):  ";
    os << "Node kind " << (int) kind << " does not contain an unparse routine\n";
    if (debug_unparse)
        os << "/*:Ast#" << this-> id << "*/";
}

void AstBlock::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBlock:#" << this-> id << "*/";
    if (Ast::debug_unparse)
        os << "/*no_braces:" << no_braces << "*/";
    for (int il = 0; il < this -> NumLabels(); il++)
    {
        os << lex_stream.NameString(this -> Label(il)) << ": ";
    }

    if (!no_braces)
        os << "{\n";
    for (int is = 0; is < this -> NumStatements(); is++)
    {
        this -> Statement(is) -> Unparse(os, lex_stream);
    }
    if (!no_braces)
        os << "}\n";
    if (Ast::debug_unparse)
        os << "/*:AstBlock#" << this-> id << "*/";
}

void AstPrimitiveType::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPrimitiveType:#" << this-> id << "*/";
    os << lex_stream.NameString(primitive_kind_token);
    if (Ast::debug_unparse)
        os << "/*:AstPrimitiveType#" << this-> id << "*/";
}

void AstArrayType::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayType:#" << this-> id << "*/";
    type -> Unparse(os, lex_stream);
    for (int i = 0; i < this -> NumBrackets(); i++)
        os << "[]";
    if (Ast::debug_unparse)
        os << "/*:AstArrayType#" << this-> id << "*/";
}

void AstSimpleName::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSimpleName:#" << this-> id << "*/";
    os << lex_stream.NameString(identifier_token);
    if (Ast::debug_unparse)
        os << "/*:AstSimpleName#" << this-> id << "*/";
}

void AstPackageDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPackageDeclaration:#" << this-> id << "*/";
    os << lex_stream.NameString(package_token);
    os << " ";
    name -> Unparse(os, lex_stream);
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstPackageDeclaration#" << this-> id << "*/";
}

void AstImportDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstImportDeclaration:#" << this-> id << "*/";
    os << lex_stream.NameString(import_token);
    os << " ";
    name -> Unparse(os, lex_stream);
    os << (star_token_opt ? "." : "");
    if (star_token_opt)
        os << lex_stream.NameString(star_token_opt);
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstImportDeclaration#" << this-> id << "*/";
}

void AstCompilationUnit::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse) os << "/*AstCompilationUnit:#" << this-> id << "*/";
    // The file is
    // os << lex_stream.FileName();
    if (package_declaration_opt)
        package_declaration_opt -> Unparse(os, lex_stream);
    for (int m = 0; m < this -> NumImportDeclarations(); m++)
        this -> ImportDeclaration(m) -> Unparse(os, lex_stream);
    for (int n = 0; n < this -> NumTypeDeclarations(); n++)
        this -> TypeDeclaration(n) -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstCompilationUnit#" << this-> id << "*/";
}

void AstModifier::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstModifier:#" << this-> id << "*/";
    os << lex_stream.NameString(modifier_kind_token);
    os << " ";
    if (Ast::debug_unparse)
       os << "/*:AstModifier#" << this-> id << "*/";
}

void AstEmptyDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
       os << "/*AstEmptyDeclaration:#" << this-> id << "*/";
    os << lex_stream.NameString(semicolon_token);
    os << "\n";
    if (Ast::debug_unparse)
    os << "/*:AstEmptyDeclaration#" << this-> id << "*/";
}

void AstClassBody::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstClassBody:#" << this-> id << "*/";
    os << "{\n";
    for (int k = 0; k < this -> NumClassBodyDeclarations(); k++)
        this -> ClassBodyDeclaration(k) -> Unparse(os, lex_stream);
    os << "}\n\n";
    if (Ast::debug_unparse)
        os << "/*:AstClassBody#" << this-> id << "*/";
}

void AstClassDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstClassDeclaration:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumClassModifiers(); i++)
    {
        os << lex_stream.NameString(this -> ClassModifier(i) -> modifier_kind_token);
        os << " ";
    }
    os << lex_stream.NameString(class_token);
    os << " ";
    os << lex_stream.NameString(identifier_token);
    os << " ";
    if (super_opt)
    {
        os << "extends ";
        super_opt -> Unparse(os, lex_stream);
        os << " ";
    }
    if (NumInterfaces() > 0)
    {
        os << "implements ";
        for (int j = 0; j < NumInterfaces(); j++)
        {
            if (j>0)
                os << ", ";
            this -> Interface(j) -> Unparse(os, lex_stream);
        }
        os << " ";
    }
    // os << ") #" << class_body -> id << "\n";
    class_body -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstClassDeclaration#" << this-> id << "*/";
}

void AstArrayInitializer::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayInitializer:#" << this-> id << "*/";
    os << "\n{ ";
    for (int k = 0; k < NumVariableInitializers(); k++)
    {
        if (k>0)
            os << ", ";
        this -> VariableInitializer(k) -> Unparse(os, lex_stream);
    }
    os << " }";
    if (Ast::debug_unparse)
        os << "/*:AstArrayInitializer#" << this-> id << "*/";
}

void AstBrackets::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBrackets:#" << this-> id << "*/";
    os << "[]";
    if (Ast::debug_unparse)
        os << "/*:AstBrackets#" << this-> id << "*/";
}

void AstVariableDeclaratorId::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstVariableDeclaratorId:#" << this-> id << "*/";
    os << lex_stream.NameString(identifier_token);
    for (int i = 0; i < NumBrackets(); i++)
        os << "[]";
    if (Ast::debug_unparse)
        os << "/*:AstVariableDeclaratorId#" << this-> id << "*/";
}

void AstVariableDeclarator::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstVariableDeclarator:#" << this-> id << "*/";
    variable_declarator_name -> Unparse(os, lex_stream);
    if (variable_initializer_opt)
    {
        os << " = ";
        variable_initializer_opt -> Unparse(os, lex_stream);
    }
    if (Ast::debug_unparse)
        os << "/*:AstVariableDeclarator#" << this-> id << "*/";
}

void AstFieldDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFieldDeclaration:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumVariableModifiers(); i++)
    {
        os << lex_stream.NameString(this -> VariableModifier(i) -> modifier_kind_token);
        os << " ";
    }
    type -> Unparse(os, lex_stream);
    os << " ";
    for (int k = 0; k < this -> NumVariableDeclarators(); k++)
    {
        if (k>0)
            os << " ,";
        this -> VariableDeclarator(k) -> Unparse(os, lex_stream);
    }
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstFieldDeclaration#" << this-> id << "*/";
}

void AstFormalParameter::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFormalParameter:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumParameterModifiers(); i++)
    {
        os << lex_stream.NameString(this -> ParameterModifier(i) -> modifier_kind_token);
        os << " ";
    }
    type -> Unparse(os, lex_stream);
    os << " ";
    formal_declarator -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstFormalParameter#" << this-> id << "*/";
}

void AstMethodDeclarator::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodDeclarator:#" << this-> id << "*/";
    os << lex_stream.NameString(identifier_token);
    os << " (";
    for (int k = 0; k < this -> NumFormalParameters(); k++)
    {
        if (k>0)
            os << ", ";
        this -> FormalParameter(k) -> Unparse(os, lex_stream);
    }
    os << ") ";
    for (int i = 0; i < NumBrackets(); i++)
        os << "[]";
    if (Ast::debug_unparse)
        os << "/*:AstMethodDeclarator#" << this-> id << "*/";
}

void AstMethodDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodDeclaration:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumMethodModifiers(); i++)
    {
        os << lex_stream.NameString(this -> MethodModifier(i) -> modifier_kind_token);
        os << " ";
    }
    type -> Unparse(os, lex_stream);
    os << " ";
    method_declarator -> Unparse(os, lex_stream);
    if (NumThrows() > 0)
    {
        os << " throws ";
        for (int k = 0; k < this -> NumThrows(); k++)
        {
            if (k>0)
                os << ", ";
            this -> Throw(k) -> Unparse(os, lex_stream);
        }
    }
    method_body -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstMethodDeclaration#" << this-> id << "*/";
}

void AstStaticInitializer::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstStaticInitializer:#" << this-> id << "*/";
    os << lex_stream.NameString(static_token);
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstStaticInitializer#" << this-> id << "*/";
}

void AstThisCall::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstThisCall:#" << this-> id << "*/";
    os << lex_stream.NameString(this_token);
    os << " (";
    for (int i = 0; i < this -> NumArguments(); i++)
    {
        if (i>0)
            os << ", ";
        this -> Argument(i) -> Unparse(os, lex_stream);
    }
    os << ");\n";
    if (Ast::debug_unparse)
        os << "/*:AstThisCall#" << this-> id << "*/";
}

void AstSuperCall::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSuperCall:#" << this-> id << "*/";
    if (wcscmp(lex_stream.NameString(super_token), L"super") == 0)
    {
        if (base_opt)
        {
            base_opt -> Unparse(os, lex_stream);
            os << lex_stream.NameString(dot_token_opt);
        }
        os << lex_stream.NameString(super_token);
        os << lex_stream.NameString(left_parenthesis_token);
        for (int j = 0; j < NumArguments(); j++)
        {
            if (j>0)
                os << ", ";
            this -> Argument(j) -> Unparse(os, lex_stream);
        }
        os << lex_stream.NameString(right_parenthesis_token);
        os << lex_stream.NameString(semicolon_token);
        os << "\n";
    }
    if (Ast::debug_unparse)
         os << "/*:AstSuperCall#" << this-> id << "*/";
}

void AstConstructorBlock::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstConstructorBlock:#" << this-> id << "*/";
    if (explicit_constructor_invocation_opt)
    {
        os << "{\n";
        explicit_constructor_invocation_opt -> Unparse(os, lex_stream);
        // os << ";\n";
    }
    block -> Unparse(os, lex_stream);
    if (explicit_constructor_invocation_opt)
        os << "}\n";
    if (Ast::debug_unparse)
        os << "/*:AstConstructorBlock#" << this-> id << "*/";
}

void AstConstructorDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstConstructorDeclaration:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumConstructorModifiers(); i++)
    {
        os << lex_stream.NameString(this -> ConstructorModifier(i) -> modifier_kind_token);
        os << " ";
    }
    constructor_declarator -> Unparse(os, lex_stream);
    if (NumThrows() > 0)
    {
        os << " throws ";
        for (int k = 0; k < this -> NumThrows(); k++)
        {
            if (k>0) os << ", ";
            this -> Throw(k) -> Unparse(os, lex_stream);
        }
    }
    constructor_body -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstConstructorDeclaration#" << this-> id << "*/";
}

void AstInterfaceDeclaration::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstInterfaceDeclaration:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumInterfaceModifiers(); i++)
    {
        os << lex_stream.NameString(this -> InterfaceModifier(i) -> modifier_kind_token);
        os << " ";
    }
    os << lex_stream.NameString(interface_token);
    os << " ";
    os << lex_stream.NameString(identifier_token);
    if (NumExtendsInterfaces() > 0)
    {
        os << " extends ";
        for (int j = 0; j < NumExtendsInterfaces(); j++)
        {
            if (j>0)
                os << ", ";
            this -> ExtendsInterface(j) -> Unparse(os, lex_stream);
        }
    }
    os << " {\n";
    for (int k = 0; k < NumInterfaceMemberDeclarations(); k++)
    {
        this -> InterfaceMemberDeclaration(k) -> Unparse(os, lex_stream);
        os << "\n";
    }
    os << "}\n";
    if (Ast::debug_unparse)
        os << "/*:AstInterfaceDeclaration#" << this-> id << "*/";
}

void AstLocalVariableDeclarationStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstLocalVariableDeclarationStatement:#" << this-> id << "*/";
    for (int i = 0; i < this -> NumLocalModifiers(); i++)
    {
        os << lex_stream.NameString(this -> LocalModifier(i) -> modifier_kind_token);
        os << " ";
    }
    type -> Unparse(os, lex_stream);
    os << " ";
    for (int k = 0; k < this -> NumVariableDeclarators(); k++)
    {
        if (k>0)
            os << ",";
        this -> VariableDeclarator(k) -> Unparse(os, lex_stream);
    }
    if (semicolon_token_opt)
        os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstLocalVariableDeclarationStatement#" << this-> id << "*/";
}

void AstIfStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstIfStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(if_token);
    AstParenthesizedExpression *parenth = expression -> ParenthesizedExpressionCast();
    if (!parenth)
        os << "(";
    expression -> Unparse(os, lex_stream);
    if (!parenth)
        os << ")";
    os << "\n";
    true_statement -> Unparse(os, lex_stream);
    if (false_statement_opt)
    {
        os << "else\n";
        false_statement_opt -> Unparse(os, lex_stream);
    }
    os << "\n";
    if (Ast::debug_unparse)
        os << "/*:AstIfStatement#" << this-> id << "*/";
}

void AstEmptyStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstEmptyStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(semicolon_token);
    os << "\n";
    if (Ast::debug_unparse)
        os << "/*:AstEmptyStatement#" << this-> id << "*/";
}

void AstExpressionStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstExpressionStatement:#" << this-> id << "*/";
    expression -> Unparse(os, lex_stream);
    if (semicolon_token_opt)
        os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstExpressionStatement#" << this-> id << "*/";
}

void AstCaseLabel::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCaseLabel:#" << this-> id << "*/";
    os << lex_stream.NameString(case_token);
    os << " ";
    expression -> Unparse(os, lex_stream);
    os << ":\n";
    if (Ast::debug_unparse)
        os << "/*:AstCaseLabel#" << this-> id << "*/";
}

void AstDefaultLabel::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDefaultLabel:#" << this-> id << "*/";
    os << lex_stream.NameString(default_token);
    os << ":\n";
    if (Ast::debug_unparse)
        os << "/*:AstDefaultLabel#" << this-> id << "*/";
}

void AstSwitchBlockStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSwitchBlockStatement:#" << this-> id << "*/";
    for (int j = 0; j < NumSwitchLabels(); j++)
        this -> SwitchLabel(j) -> Unparse(os, lex_stream);
    for (int l = 0; l < NumStatements(); l++)
        this -> Statement(l) -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstSwitchBlockStatement#" << this-> id << "*/";
}

void AstSwitchStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSwitchStatement:#" << this-> id << "*/";
  // What about the label_opt??
    os << lex_stream.NameString(switch_token);
    AstParenthesizedExpression *parenth = expression -> ParenthesizedExpressionCast();
    if (!parenth)
        os << "(";
    expression -> Unparse(os, lex_stream);
    if (!parenth)
        os << ")";
    // I think that switch_block will output its own braces.
    // os << "{\n";
    switch_block -> Unparse(os, lex_stream);
    // what about switch_labels_opt?
    // os << "}\n";
    if (Ast::debug_unparse)
        os << "/*:AstSwitchStatement#" << this-> id << "*/";
}

void AstWhileStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstWhileStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(while_token);
    // What about Label_opt?
    os << " ";
    AstParenthesizedExpression *parenth = expression -> ParenthesizedExpressionCast();
    if (!parenth)
        os << "(";
    expression -> Unparse(os, lex_stream);
    if (!parenth)
        os << ")";
    os << "\n";
    statement -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstWhileStatement#" << this-> id << "*/";
}

void AstDoStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDoStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(do_token);
    os << "\n";
    statement -> Unparse(os, lex_stream);
    os << lex_stream.NameString(while_token);
    AstParenthesizedExpression *parenth = expression -> ParenthesizedExpressionCast();
    if (!parenth)
        os << "(";
    expression -> Unparse(os, lex_stream);
    if (!parenth)
        os << ")";
    os << lex_stream.NameString(semicolon_token);
    os << "\n";
    if (Ast::debug_unparse)
        os << "/*:AstDoStatement#" << this-> id << "*/";
}

void AstForStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstForStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(for_token);
    os << " (";
    for (int i = 0; i < this -> NumForInitStatements(); i++)
    {
        if (i>0)
            os << ", ";
        this -> ForInitStatement(i) -> Unparse(os, lex_stream);
    }
    os << "; ";
    if (end_expression_opt)
        end_expression_opt -> Unparse(os, lex_stream);
    os << "; ";
    for (int k = 0; k < this -> NumForUpdateStatements(); k++)
    {
        if (k>0)
            os << ", ";
        this -> ForUpdateStatement(k) -> Unparse(os, lex_stream);
    }
    os << ")\n";
    statement -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstForStatement#" << this-> id << "*/";
}

void AstBreakStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBreakStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(break_token);
    if (identifier_token_opt)
    {
        os << " ";
        os << lex_stream.NameString(identifier_token_opt);
    }
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstBreakStatement#" << this-> id << "*/";
}

void AstContinueStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstContinueStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(continue_token);
    if (identifier_token_opt)
    {
        os << " ";
        os << lex_stream.NameString(identifier_token_opt);
    }
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstContinueStatement#" << this-> id << "*/";
}

void AstReturnStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstReturnStatement:#" << this-> id << "*/";
    // Do NOT use this; when the return statement is not literally
    // present in the source, the return_token points at the next "}".
    // os << lex_stream.NameString(return_token);
    os << "return";
    if (expression_opt)
    {
        os << " ";
        expression_opt -> Unparse(os, lex_stream);
    }
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstReturnStatement#" << this-> id << "*/";
}

void AstThrowStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstThrowStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(throw_token);
    os << " ";
    expression -> Unparse(os, lex_stream);
    os << ";\n";
    if (Ast::debug_unparse)
        os << "/*:AstThrowStatement#" << this-> id << "*/";
}

void AstSynchronizedStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSynchronizedStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(synchronized_token);
    os << " ";
    AstParenthesizedExpression *parenth = expression -> ParenthesizedExpressionCast();
    if (!parenth)
        os << "(";
    expression -> Unparse(os, lex_stream);
    if (!parenth)
        os << ")";
    os << "\n";
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstSynchronizedStatement#" << this-> id << "*/";
}

void AstCatchClause::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCatchClause:#" << this-> id << "*/";
    os << lex_stream.NameString(catch_token);
    os << " (";
    formal_parameter -> Unparse(os, lex_stream);
    os << ")\n";
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstCatchClause#" << this-> id << "*/";
}

void AstFinallyClause::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFinallyClause:#" << this-> id << "*/";
    os << lex_stream.NameString(finally_token);
    os << "\n";
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse) os << "/*:AstFinallyClause#" << this-> id << "*/";
}

void AstTryStatement::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstTryStatement:#" << this-> id << "*/";
    os << lex_stream.NameString(try_token);
    os << "\n";
    block -> Unparse(os, lex_stream);
    for (int k = 0; k < this -> NumCatchClauses(); k++)
        this -> CatchClause(k) -> Unparse(os, lex_stream);
    if (finally_clause_opt)
        finally_clause_opt -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstTryStatement#" << this-> id << "*/";
}

void AstIntegerLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstIntegerLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(integer_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstIntegerLiteral#" << this-> id << "*/";
}

void AstLongLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstLongLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(long_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstLongLiteral#" << this-> id << "*/";
}

void AstFloatingPointLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFloatingPointLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(floating_point_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstFloatingPointLiteral#" << this-> id << "*/";
}

void AstDoubleLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDoubleLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(double_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstDoubleLiteral#" << this-> id << "*/";
}

void AstTrueLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstTrueLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(true_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstTrueLiteral#" << this-> id << "*/";
}

void AstFalseLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFalseLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(false_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstFalseLiteral#" << this-> id << "*/";
}

void AstStringLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstStringLiteral:#" << this-> id << "*/";
    {
        bool old_expand = os.ExpandWchar();
        os.SetExpandWchar(true);
        os << lex_stream.NameString(string_literal_token), lex_stream.NameStringLength(string_literal_token);
        os.SetExpandWchar(old_expand);
    }
    if (Ast::debug_unparse)
        os << "/*:AstStringLiteral#" << this-> id << "*/";
}

void AstCharacterLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCharacterLiteral:#" << this-> id << "*/";
    {
        bool old_expand = os.ExpandWchar();
        os.SetExpandWchar(true);
        os << lex_stream.NameString(character_literal_token), lex_stream.NameStringLength(character_literal_token);
        os.SetExpandWchar(old_expand);
    }
    if (Ast::debug_unparse)
        os << "/*:AstCharacterLiteral#" << this-> id << "*/";
}

void AstNullLiteral::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstNullLiteral:#" << this-> id << "*/";
    os << lex_stream.NameString(null_token);
    if (Ast::debug_unparse)
        os << "/*:AstNullLiteral#" << this-> id << "*/";
}

void AstThisExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstThisExpression:#" << this-> id << "*/";
    os << lex_stream.NameString(this_token);
    if (Ast::debug_unparse)
        os << "/*:AstThisExpression#" << this-> id << "*/";
}

void AstSuperExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSuperExpression:#" << this-> id << "*/";
    os << lex_stream.NameString(super_token);
    if (Ast::debug_unparse)
        os << "/*:AstSuperExpression#" << this-> id << "*/";
}

void AstParenthesizedExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstParenthesizedExpression:#" << this-> id << "*/";
    os << lex_stream.NameString(left_parenthesis_token);
    expression -> Unparse(os, lex_stream);
    os << lex_stream.NameString(right_parenthesis_token);
    if (Ast::debug_unparse)
        os << "/*:AstParenthesizedExpression#" << this-> id << "*/";
}

void AstTypeExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstTypeExpression:#" << this-> id << "*/";
    type -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstTypeExpression#" << this-> id << "*/";
}

void AstClassInstanceCreationExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstClassInstanceCreationExpression:#" << this-> id << "*/";
    if (dot_token_opt /* base_opt - see ast.h for explanation */)
        base_opt -> Unparse(os, lex_stream);
    os << lex_stream.NameString(new_token);
    os << " ";
    class_type -> Unparse(os, lex_stream);
    os << "( ";
    for (int j = 0; j < NumArguments(); j++)
    {
        if (j>0)
            os << ", ";
        this -> Argument(j) -> Unparse(os, lex_stream);
    }
    os << " )";

    if (class_body_opt)
        class_body_opt -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstClassInstanceCreationExpression#" << this-> id << "*/";
}

void AstDimExpr::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDimExpr:#" << this-> id << "*/";
    os << "[";
    expression -> Unparse(os, lex_stream);
    os << "]";
    if (Ast::debug_unparse)
        os << "/*:AstDimExpr#" << this-> id << "*/";
}

void AstArrayCreationExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayCreationExpression:#" << this-> id << "*/";
    os << lex_stream.NameString(new_token);
    os << " ";
    array_type -> Unparse(os, lex_stream);
    for (int i = 0; i < NumDimExprs(); i++)
        DimExpr(i) -> Unparse(os, lex_stream);
    for (int k = 0; k < NumBrackets(); k++)
       os << "[]";
    if (array_initializer_opt)
        array_initializer_opt -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstArrayCreationExpression#" << this-> id << "*/";
}

void AstFieldAccess::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFieldAccess:#" << this-> id << "*/";
    base -> Unparse(os, lex_stream);
    os << lex_stream.NameString(dot_token);
    os << lex_stream.NameString(identifier_token);
    if (Ast::debug_unparse)
        os << "/*:AstFieldAccess#" << this-> id << "*/";
}

void AstMethodInvocation::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodInvocation:#" << this-> id << "*/";
    method -> Unparse(os, lex_stream);
    os << "(";
    for (int i = 0; i < this -> NumArguments(); i++)
    {
        if (i>0)
            os << ", ";
        this -> Argument(i) -> Unparse(os, lex_stream);
    }
    os << ")";
    if (Ast::debug_unparse)
        os << "/*:AstMethodInvocation#" << this-> id << "*/";
}

void AstArrayAccess::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayAccess:#" << this-> id << "*/";
    base -> Unparse(os, lex_stream);
    os << "[";
    expression -> Unparse(os, lex_stream);
    os << "]";
    if (Ast::debug_unparse)
        os << "/*:AstArrayAccess#" << this-> id << "*/";
}

void AstPostUnaryExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPostUnaryExpression:#" << this-> id << "*/";
    expression -> Unparse(os, lex_stream);
    os << lex_stream.NameString(post_operator_token);
    if (Ast::debug_unparse)
        os << "/*:AstPostUnaryExpression#" << this-> id << "*/";
}

void AstPreUnaryExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPreUnaryExpression:#" << this-> id << "*/";
    os << lex_stream.NameString(pre_operator_token);
    expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstPreUnaryExpression#" << this-> id << "*/";
}

void AstCastExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCastExpression:#" << this-> id << "*/";
    if (left_parenthesis_token_opt && type_opt)
    {
        os << "(";
        type_opt -> Unparse(os, lex_stream);
        for (int i = 0; i < NumBrackets(); i++)
            os << "[]";
        os << ")";
    }

    expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstCastExpression#" << this-> id << "*/";
}

void AstBinaryExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBinaryExpression:#" << this-> id << "*/";
    left_expression -> Unparse(os, lex_stream);
    os << " ";
    os << lex_stream.NameString(binary_operator_token);
    os << " ";
    right_expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstBinaryExpression#" << this-> id << "*/";
}

void AstConditionalExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstConditionalExpression:#" << this-> id << "*/";
    test_expression -> Unparse(os, lex_stream);
    os << " ? ";
    true_expression -> Unparse(os, lex_stream);
    os << " : ";
    false_expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstConditionalExpression#" << this-> id << "*/";
}

void AstAssignmentExpression::Unparse(Ostream& os, LexStream& lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstAssignmentExpression:#" << this-> id << "*/";
    left_hand_side -> Unparse(os, lex_stream);
    os << " ";
    os << lex_stream.NameString(assignment_operator_token);
    os << " ";
    expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstAssignmentExpression#" << this-> id << "*/";
}
#endif
