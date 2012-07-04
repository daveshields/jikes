// $Id: unparse.cpp,v 1.30 2002/08/05 23:56:30 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "ast.h"

#ifdef JIKES_DEBUG

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


bool Ast::debug_unparse = false;

// Special top-level form
void AstCompilationUnit::Unparse(LexStream* lex_stream, const char* const directory)
{
    char *in_file_name = lex_stream -> FileName();
    // char *suffix = ".unparse";
    char *suffix = "";
    char *out_file_name = strcat3(directory, in_file_name, suffix);
    // Create the directory if necessary
    SystemMkdirhierForFile(out_file_name);
    ofstream os_base(out_file_name);
    if (! os_base)
    {
        Ostream() << "Cannot open output file " << out_file_name << endl;
        abort();
    }
    Ostream os(&os_base);
    Unparse(os, lex_stream);
    delete[] out_file_name;
}

void Ast::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (debug_unparse)
        os << "/*Ast:#" << id << "*/";
    os << "***** TO DO *****";
    os << "#" << id << " (Ast):  "
       << "Node kind " << (int) kind << " does not contain an unparse routine"
       << endl;
    if (debug_unparse)
        os << "/*:Ast#" << id << "*/";
}

void AstBlock::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBlock:#" << id << "*/"
           << "/*no_braces:" << no_braces << "*/";
    if (label_opt)
        os << lex_stream -> NameString(label_opt) << ": ";
    if (! no_braces)
        os << '{' << endl;
    for (int is = 0; is < NumStatements(); is++)
        Statement(is) -> Unparse(os, lex_stream);
    if (! no_braces)
        os << '}' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstBlock#" << id << "*/";
}

void AstPrimitiveType::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPrimitiveType:#" << id << "*/";
    os << lex_stream -> NameString(primitive_kind_token);
    if (Ast::debug_unparse)
        os << "/*:AstPrimitiveType#" << id << "*/";
}

void AstArrayType::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayType:#" << id << "*/";
    type -> Unparse(os, lex_stream);
    for (int i = 0; i < NumBrackets(); i++)
        Brackets(i) -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstArrayType#" << id << "*/";
}

void AstSimpleName::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSimpleName:#" << id << "*/";
    os << lex_stream -> NameString(identifier_token);
    if (Ast::debug_unparse)
        os << "/*:AstSimpleName#" << id << "*/";
}

void AstPackageDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPackageDeclaration:#" << id << "*/";
    os << lex_stream -> NameString(package_token) << ' ';
    name -> Unparse(os, lex_stream);
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstPackageDeclaration#" << id << "*/";
}

void AstImportDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstImportDeclaration:#" << id << "*/";
    os << lex_stream -> NameString(import_token) << ' ';
    name -> Unparse(os, lex_stream);
    os << (star_token_opt ? "." : "");
    if (star_token_opt)
        os << lex_stream -> NameString(star_token_opt);
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstImportDeclaration#" << id << "*/";
}

void AstCompilationUnit::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCompilationUnit:#" << id << "*/";
    os << "// " << lex_stream -> FileName() << endl;
    if (package_declaration_opt)
        package_declaration_opt -> Unparse(os, lex_stream);
    for (int m = 0; m < NumImportDeclarations(); m++)
        ImportDeclaration(m) -> Unparse(os, lex_stream);
    for (int n = 0; n < NumTypeDeclarations(); n++)
        TypeDeclaration(n) -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstCompilationUnit#" << id << "*/";
}

void AstModifier::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstModifier:#" << id << "*/";
    os << lex_stream -> NameString(modifier_kind_token) << ' ';
    if (Ast::debug_unparse)
       os << "/*:AstModifier#" << id << "*/";
}

void AstEmptyDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
       os << "/*AstEmptyDeclaration:#" << id << "*/";
    os << lex_stream -> NameString(semicolon_token) << endl;
    if (Ast::debug_unparse)
    os << "/*:AstEmptyDeclaration#" << id << "*/";
}

void AstClassBody::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstClassBody:#" << id << "*/";
    os << '{' << endl;
    for (int k = 0; k < NumClassBodyDeclarations(); k++)
        ClassBodyDeclaration(k) -> Unparse(os, lex_stream);
    os << '}' << endl << endl;
    if (Ast::debug_unparse)
        os << "/*:AstClassBody#" << id << "*/";
}

void AstClassDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstClassDeclaration:#" << id << "*/";
    if (lex_stream -> IsDeprecated(lex_stream -> Previous(LeftToken())))
        os << "/**@deprecated*/ ";
    for (int i = 0; i < NumClassModifiers(); i++)
    {
        os << lex_stream -> NameString(ClassModifier(i) -> modifier_kind_token)
           << ' ';
    }
    os << lex_stream -> NameString(class_token) << ' '
       << lex_stream -> NameString(identifier_token) << ' ';
    if (super_opt)
    {
        os << "extends ";
        super_opt -> Unparse(os, lex_stream);
        os << ' ';
    }
    if (NumInterfaces() > 0)
    {
        os << "implements ";
        for (int j = 0; j < NumInterfaces(); j++)
        {
            if (j > 0)
                os << ", ";
            Interface(j) -> Unparse(os, lex_stream);
        }
        os << ' ';
    }
    class_body -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstClassDeclaration#" << id << "*/";
}

void AstArrayInitializer::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayInitializer:#" << id << "*/";
    os << endl << "{ ";
    for (int k = 0; k < NumVariableInitializers(); k++)
    {
        if (k > 0)
            os << ", ";
        VariableInitializer(k) -> Unparse(os, lex_stream);
    }
    os << " }";
    if (Ast::debug_unparse)
        os << "/*:AstArrayInitializer#" << id << "*/";
}

void AstBrackets::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBrackets:#" << id << "*/";
    os << "[]";
    if (Ast::debug_unparse)
        os << "/*:AstBrackets#" << id << "*/";
}

void AstVariableDeclaratorId::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstVariableDeclaratorId:#" << id << "*/";
    os << lex_stream -> NameString(identifier_token);
    for (int i = 0; i < NumBrackets(); i++)
        Brackets(i) -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstVariableDeclaratorId#" << id << "*/";
}

void AstVariableDeclarator::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstVariableDeclarator:#" << id << "*/";
    variable_declarator_name -> Unparse(os, lex_stream);
    if (variable_initializer_opt)
    {
        os << " = ";
        variable_initializer_opt -> Unparse(os, lex_stream);
    }
    if (Ast::debug_unparse)
        os << "/*:AstVariableDeclarator#" << id << "*/";
}

void AstFieldDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFieldDeclaration:#" << id << "*/";
    if (lex_stream -> IsDeprecated(lex_stream -> Previous(LeftToken())))
        os << "/**@deprecated*/ ";
    for (int i = 0; i < NumVariableModifiers(); i++)
    {
        os << lex_stream -> NameString(VariableModifier(i) -> modifier_kind_token)
           << ' ';
    }
    type -> Unparse(os, lex_stream);
    os << ' ';
    for (int k = 0; k < NumVariableDeclarators(); k++)
    {
        if (k > 0)
            os << ", ";
        VariableDeclarator(k) -> Unparse(os, lex_stream);
    }
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstFieldDeclaration#" << id << "*/";
}

void AstFormalParameter::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFormalParameter:#" << id << "*/";
    for (int i = 0; i < NumParameterModifiers(); i++)
    {
        os << lex_stream -> NameString(ParameterModifier(i) -> modifier_kind_token)
           << ' ';
    }
    type -> Unparse(os, lex_stream);
    os << ' ';
    formal_declarator -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstFormalParameter#" << id << "*/";
}

void AstMethodDeclarator::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodDeclarator:#" << id << "*/";
    os << lex_stream -> NameString(identifier_token) << '(';
    for (int k = 0; k < NumFormalParameters(); k++)
    {
        if (k > 0)
            os << ", ";
        FormalParameter(k) -> Unparse(os, lex_stream);
    }
    os << ')';
    for (int i = 0; i < NumBrackets(); i++)
        Brackets(i) -> Unparse(os, lex_stream);
    os << ' ';
    if (Ast::debug_unparse)
        os << "/*:AstMethodDeclarator#" << id << "*/";
}

void AstMethodDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodDeclaration:#" << id << "*/";
    if (lex_stream -> IsDeprecated(lex_stream -> Previous(LeftToken())))
        os << "/**@deprecated*/ ";
    for (int i = 0; i < NumMethodModifiers(); i++)
    {
        os << lex_stream -> NameString(MethodModifier(i) -> modifier_kind_token)
           << ' ';
    }
    type -> Unparse(os, lex_stream);
    os << ' ';
    method_declarator -> Unparse(os, lex_stream);
    if (NumThrows() > 0)
    {
        os << "throws ";
        for (int k = 0; k < NumThrows(); k++)
        {
            if (k > 0)
                os << ", ";
            Throw(k) -> Unparse(os, lex_stream);
        }
        os << ' ';
    }
    method_body -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstMethodDeclaration#" << id << "*/";
}

void AstStaticInitializer::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstStaticInitializer:#" << id << "*/";
    os << lex_stream -> NameString(static_token);
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstStaticInitializer#" << id << "*/";
}

void AstThisCall::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstThisCall:#" << id << "*/";
    os << lex_stream -> NameString(this_token) << " (";
    for (int i = 0; i < NumArguments(); i++)
    {
        if (i > 0)
            os << ", ";
        Argument(i) -> Unparse(os, lex_stream);
    }
    os << ");" << endl;
    if (Ast::debug_unparse)
        os << "/*:AstThisCall#" << id << "*/";
}

void AstSuperCall::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSuperCall:#" << id << "*/";
    if (wcscmp(lex_stream -> NameString(super_token), L"super") == 0)
    {
        // If base_opt is set but dot_token_opt is not, then base_opt
        // is itself "super" and so it need not be specially output.
        if (base_opt && dot_token_opt)
        {
            base_opt -> Unparse(os, lex_stream);
            os << lex_stream -> NameString(dot_token_opt);
        }
        os << lex_stream -> NameString(super_token)
           << lex_stream -> NameString(left_parenthesis_token);
        for (int j = 0; j < NumArguments(); j++)
        {
            if (j > 0)
                os << ", ";
            Argument(j) -> Unparse(os, lex_stream);
        }
        os << lex_stream -> NameString(right_parenthesis_token)
           << lex_stream -> NameString(semicolon_token) << endl;
    }
    if (Ast::debug_unparse)
         os << "/*:AstSuperCall#" << id << "*/";
}

void AstMethodBody::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodBody:#" << id << "*/";
    os << '{' << endl;
    if (explicit_constructor_opt)
        explicit_constructor_opt -> Unparse(os, lex_stream);
    AstBlock::Unparse(os, lex_stream);
    os << '}' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstMethodBody#" << id << "*/";
}

void AstConstructorDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstConstructorDeclaration:#" << id << "*/";
    if (lex_stream -> IsDeprecated(lex_stream -> Previous(LeftToken())))
        os << "/**@deprecated*/ ";
    for (int i = 0; i < NumConstructorModifiers(); i++)
    {
        os << lex_stream -> NameString(ConstructorModifier(i) -> modifier_kind_token);
        os << ' ';
    }
    constructor_declarator -> Unparse(os, lex_stream);
    if (NumThrows() > 0)
    {
        os << " throws ";
        for (int k = 0; k < NumThrows(); k++)
        {
            if (k > 0)
                os << ", ";
            Throw(k) -> Unparse(os, lex_stream);
        }
    }
    constructor_body -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstConstructorDeclaration#" << id << "*/";
}

void AstInterfaceDeclaration::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstInterfaceDeclaration:#" << id << "*/";
    if (lex_stream -> IsDeprecated(lex_stream -> Previous(LeftToken())))
        os << "/**@deprecated*/ ";
    for (int i = 0; i < NumInterfaceModifiers(); i++)
    {
        os << lex_stream -> NameString(InterfaceModifier(i) -> modifier_kind_token)
           << ' ';
    }
    os << lex_stream -> NameString(interface_token) << ' '
       << lex_stream -> NameString(identifier_token);
    if (NumExtendsInterfaces() > 0)
    {
        os << " extends ";
        for (int j = 0; j < NumExtendsInterfaces(); j++)
        {
            if (j > 0)
                os << ", ";
            ExtendsInterface(j) -> Unparse(os, lex_stream);
        }
    }
    os << " {" << endl;
    for (int k = 0; k < NumInterfaceMemberDeclarations(); k++)
    {
        InterfaceMemberDeclaration(k) -> Unparse(os, lex_stream);
        os << endl;
    }
    os << '}' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstInterfaceDeclaration#" << id << "*/";
}

void AstLocalVariableDeclarationStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstLocalVariableDeclarationStatement:#" << id << "*/";
    for (int i = 0; i < NumLocalModifiers(); i++)
    {
        os << lex_stream -> NameString(LocalModifier(i) -> modifier_kind_token)
           << ' ';
    }
    type -> Unparse(os, lex_stream);
    os << ' ';
    for (int k = 0; k < NumVariableDeclarators(); k++)
    {
        if (k > 0)
            os << ',';
        VariableDeclarator(k) -> Unparse(os, lex_stream);
    }
    if (semicolon_token_opt)
        os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstLocalVariableDeclarationStatement#" << id << "*/";
}

void AstIfStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstIfStatement:#" << id << "*/";
    os << lex_stream -> NameString(if_token) << '(';
    expression -> Unparse(os, lex_stream);
    os << ')' << endl;
    true_statement -> Unparse(os, lex_stream);
    if (false_statement_opt)
    {
        os << "else" << endl;
        false_statement_opt -> Unparse(os, lex_stream);
    }
    os << endl;
    if (Ast::debug_unparse)
        os << "/*:AstIfStatement#" << id << "*/";
}

void AstEmptyStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstEmptyStatement:#" << id << "*/";
    os << lex_stream -> NameString(semicolon_token) << endl;
    if (Ast::debug_unparse)
        os << "/*:AstEmptyStatement#" << id << "*/";
}

void AstExpressionStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstExpressionStatement:#" << id << "*/";
    expression -> Unparse(os, lex_stream);
    if (semicolon_token_opt)
        os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstExpressionStatement#" << id << "*/";
}

void AstCaseLabel::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCaseLabel:#" << id << "*/";
    os << lex_stream -> NameString(case_token) << ' ';
    expression -> Unparse(os, lex_stream);
    os << ':' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstCaseLabel#" << id << "*/";
}

void AstDefaultLabel::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDefaultLabel:#" << id << "*/";
    os << lex_stream -> NameString(default_token) << ':' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstDefaultLabel#" << id << "*/";
}

void AstSwitchBlockStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSwitchBlockStatement:#" << id << "*/";
    for (int j = 0; j < NumSwitchLabels(); j++)
        SwitchLabel(j) -> Unparse(os, lex_stream);
    AstBlock::Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstSwitchBlockStatement#" << id << "*/";
}

void AstSwitchStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSwitchStatement:#" << id << "*/";
    // What about the label_opt??
    os << lex_stream -> NameString(switch_token) << '(';
    expression -> Unparse(os, lex_stream);
    os << ')';
    switch_block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstSwitchStatement#" << id << "*/";
}

void AstWhileStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstWhileStatement:#" << id << "*/";
    os << lex_stream -> NameString(while_token) << " (";
    expression -> Unparse(os, lex_stream);
    os << ')' << endl;
    statement -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstWhileStatement#" << id << "*/";
}

void AstDoStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDoStatement:#" << id << "*/";
    os << lex_stream -> NameString(do_token) << endl;
    statement -> Unparse(os, lex_stream);
    os << lex_stream -> NameString(while_token) << " (";
    expression -> Unparse(os, lex_stream);
    os << ')' << lex_stream -> NameString(semicolon_token) << endl;
    if (Ast::debug_unparse)
        os << "/*:AstDoStatement#" << id << "*/";
}

void AstForStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstForStatement:#" << id << "*/";
    os << lex_stream -> NameString(for_token) << " (";
    for (int i = 0; i < NumForInitStatements(); i++)
    {
        if (i > 0)
            os << ", ";
        ForInitStatement(i) -> Unparse(os, lex_stream);
    }
    os << "; ";
    if (end_expression_opt)
        end_expression_opt -> Unparse(os, lex_stream);
    os << "; ";
    for (int k = 0; k < NumForUpdateStatements(); k++)
    {
        if (k > 0)
            os << ", ";
        ForUpdateStatement(k) -> Unparse(os, lex_stream);
    }
    os << ')' << endl;
    statement -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstForStatement#" << id << "*/";
}

void AstBreakStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBreakStatement:#" << id << "*/";
    os << lex_stream -> NameString(break_token);
    if (identifier_token_opt)
    {
        os << ' ' << lex_stream -> NameString(identifier_token_opt);
    }
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstBreakStatement#" << id << "*/";
}

void AstContinueStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstContinueStatement:#" << id << "*/";
    os << lex_stream -> NameString(continue_token);
    if (identifier_token_opt)
    {
        os << ' ' << lex_stream -> NameString(identifier_token_opt);
    }
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstContinueStatement#" << id << "*/";
}

void AstReturnStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstReturnStatement:#" << id << "*/";
    // Do NOT use this; when the return statement is not literally
    // present in the source, the return_token points at the next "}".
    // os << lex_stream -> NameString(return_token);
    os << "return";
    if (expression_opt)
    {
        os << ' ';
        expression_opt -> Unparse(os, lex_stream);
    }
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstReturnStatement#" << id << "*/";
}

void AstThrowStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstThrowStatement:#" << id << "*/";
    os << lex_stream -> NameString(throw_token) << ' ';
    expression -> Unparse(os, lex_stream);
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstThrowStatement#" << id << "*/";
}

void AstSynchronizedStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSynchronizedStatement:#" << id << "*/";
    os << lex_stream -> NameString(synchronized_token) << " (";
    expression -> Unparse(os, lex_stream);
    os << ')' << endl;
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstSynchronizedStatement#" << id << "*/";
}

void AstAssertStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstAssertStatement:#" << id << "*/";
    os << lex_stream -> NameString(assert_token) << ' ';
    condition -> Unparse(os, lex_stream);
    if (message_opt)
    {
        os << " : ";
        message_opt -> Unparse(os, lex_stream);
    }
    os << ';' << endl;
    if (Ast::debug_unparse)
        os << "/*:AstAssertStatement#" << id << "*/";
}

void AstCatchClause::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCatchClause:#" << id << "*/";
    os << lex_stream -> NameString(catch_token) << " (";
    formal_parameter -> Unparse(os, lex_stream);
    os << ')' << endl;
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstCatchClause#" << id << "*/";
}

void AstFinallyClause::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFinallyClause:#" << id << "*/";
    os << lex_stream -> NameString(finally_token) << endl;
    block -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstFinallyClause#" << id << "*/";
}

void AstTryStatement::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstTryStatement:#" << id << "*/";
    os << lex_stream -> NameString(try_token) << endl;
    block -> Unparse(os, lex_stream);
    for (int k = 0; k < NumCatchClauses(); k++)
        CatchClause(k) -> Unparse(os, lex_stream);
    if (finally_clause_opt)
        finally_clause_opt -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstTryStatement#" << id << "*/";
}

void AstIntegerLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstIntegerLiteral:#" << id << "*/";
    os << lex_stream -> NameString(integer_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstIntegerLiteral#" << id << "*/";
}

void AstLongLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstLongLiteral:#" << id << "*/";
    os << lex_stream -> NameString(long_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstLongLiteral#" << id << "*/";
}

void AstFloatLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFloatLiteral:#" << id << "*/";
    os << lex_stream -> NameString(float_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstFloatLiteral#" << id << "*/";
}

void AstDoubleLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDoubleLiteral:#" << id << "*/";
    os << lex_stream -> NameString(double_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstDoubleLiteral#" << id << "*/";
}

void AstTrueLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstTrueLiteral:#" << id << "*/";
    os << lex_stream -> NameString(true_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstTrueLiteral#" << id << "*/";
}

void AstFalseLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFalseLiteral:#" << id << "*/";
    os << lex_stream -> NameString(false_literal_token);
    if (Ast::debug_unparse)
        os << "/*:AstFalseLiteral#" << id << "*/";
}

void AstStringLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstStringLiteral:#" << id << "*/";
    bool old_expand = os.ExpandWchar();
    os.SetExpandWchar(true);
    os << lex_stream -> NameString(string_literal_token);
    os.SetExpandWchar(old_expand);
    if (Ast::debug_unparse)
        os << "/*:AstStringLiteral#" << id << "*/";
}

void AstCharacterLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCharacterLiteral:#" << id << "*/";
    bool old_expand = os.ExpandWchar();
    os.SetExpandWchar(true);
    os << lex_stream -> NameString(character_literal_token);
    os.SetExpandWchar(old_expand);
    if (Ast::debug_unparse)
        os << "/*:AstCharacterLiteral#" << id << "*/";
}

void AstNullLiteral::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstNullLiteral:#" << id << "*/";
    os << lex_stream -> NameString(null_token);
    if (Ast::debug_unparse)
        os << "/*:AstNullLiteral#" << id << "*/";
}

void AstThisExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstThisExpression:#" << id << "*/";
    os << lex_stream -> NameString(this_token);
    if (Ast::debug_unparse)
        os << "/*:AstThisExpression#" << id << "*/";
}

void AstSuperExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstSuperExpression:#" << id << "*/";
    os << lex_stream -> NameString(super_token);
    if (Ast::debug_unparse)
        os << "/*:AstSuperExpression#" << id << "*/";
}

void AstParenthesizedExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstParenthesizedExpression:#" << id << "*/";
    os << lex_stream -> NameString(left_parenthesis_token);
    expression -> Unparse(os, lex_stream);
    os << lex_stream -> NameString(right_parenthesis_token);
    if (Ast::debug_unparse)
        os << "/*:AstParenthesizedExpression#" << id << "*/";
}

void AstTypeExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstTypeExpression:#" << id << "*/";
    type -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstTypeExpression#" << id << "*/";
}

void AstClassInstanceCreationExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstClassInstanceCreationExpression:#" << id << "*/";
    if (dot_token_opt /* base_opt - see ast.h for explanation */)
        base_opt -> Unparse(os, lex_stream);
    os << lex_stream -> NameString(new_token) << ' ';
    class_type -> Unparse(os, lex_stream);
    os << "( ";
    for (int j = 0; j < NumArguments(); j++)
    {
        if (j > 0)
            os << ", ";
        Argument(j) -> Unparse(os, lex_stream);
    }
    os << " )";

    if (class_body_opt)
        class_body_opt -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstClassInstanceCreationExpression#" << id << "*/";
}

void AstDimExpr::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstDimExpr:#" << id << "*/";
    os << '[';
    expression -> Unparse(os, lex_stream);
    os << ']';
    if (Ast::debug_unparse)
        os << "/*:AstDimExpr#" << id << "*/";
}

void AstArrayCreationExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayCreationExpression:#" << id << "*/";
    os << lex_stream -> NameString(new_token) << ' ';
    array_type -> Unparse(os, lex_stream);
    for (int i = 0; i < NumDimExprs(); i++)
        DimExpr(i) -> Unparse(os, lex_stream);
    for (int k = 0; k < NumBrackets(); k++)
        Brackets(k) -> Unparse(os, lex_stream);
    if (array_initializer_opt)
        array_initializer_opt -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstArrayCreationExpression#" << id << "*/";
}

void AstFieldAccess::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstFieldAccess:#" << id << "*/";
    base -> Unparse(os, lex_stream);
    os << lex_stream -> NameString(dot_token)
       << lex_stream -> NameString(identifier_token);
    if (Ast::debug_unparse)
        os << "/*:AstFieldAccess#" << id << "*/";
}

void AstMethodInvocation::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstMethodInvocation:#" << id << "*/";
    method -> Unparse(os, lex_stream);
    os << '(';
    for (int i = 0; i < NumArguments(); i++)
    {
        if (i > 0)
            os << ", ";
        Argument(i) -> Unparse(os, lex_stream);
    }
    os << ')';
    if (Ast::debug_unparse)
        os << "/*:AstMethodInvocation#" << id << "*/";
}

void AstArrayAccess::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstArrayAccess:#" << id << "*/";
    base -> Unparse(os, lex_stream);
    os << '[';
    expression -> Unparse(os, lex_stream);
    os << ']';
    if (Ast::debug_unparse)
        os << "/*:AstArrayAccess#" << id << "*/";
}

void AstPostUnaryExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPostUnaryExpression:#" << id << "*/";
    expression -> Unparse(os, lex_stream);
    os << lex_stream -> NameString(post_operator_token);
    if (Ast::debug_unparse)
        os << "/*:AstPostUnaryExpression#" << id << "*/";
}

void AstPreUnaryExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstPreUnaryExpression:#" << id << "*/";
    os << lex_stream -> NameString(pre_operator_token);
    expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstPreUnaryExpression#" << id << "*/";
}

void AstCastExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstCastExpression:#" << id << "*/";
    if (left_parenthesis_token_opt && type_opt)
    {
        os << '(';
        type_opt -> Unparse(os, lex_stream);
        for (int i = 0; i < NumBrackets(); i++)
            Brackets(i) -> Unparse(os, lex_stream);
        os << ')';
    }

    expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstCastExpression#" << id << "*/";
}

void AstBinaryExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstBinaryExpression:#" << id << "*/";
    left_expression -> Unparse(os, lex_stream);
    os << ' ' << lex_stream -> NameString(binary_operator_token) << ' ';
    right_expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstBinaryExpression#" << id << "*/";
}

void AstConditionalExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstConditionalExpression:#" << id << "*/";
    test_expression -> Unparse(os, lex_stream);
    os << " ? ";
    true_expression -> Unparse(os, lex_stream);
    os << " : ";
    false_expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstConditionalExpression#" << id << "*/";
}

void AstAssignmentExpression::Unparse(Ostream& os, LexStream* lex_stream)
{
    if (Ast::debug_unparse)
        os << "/*AstAssignmentExpression:#" << id << "*/";
    left_hand_side -> Unparse(os, lex_stream);
    os << ' ' << lex_stream -> NameString(assignment_operator_token) << ' ';
    expression -> Unparse(os, lex_stream);
    if (Ast::debug_unparse)
        os << "/*:AstAssignmentExpression#" << id << "*/";
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // JIKES_DEBUG
